prepare_live_from_long <- function(live_long, category_names) {
  stopifnot(all(c("bureau_id", "liste", "voix") %in% names(live_long)))

  # agrégation défensive au cas où une ligne serait dupliquée
  live_agg <- live_long[
    ,
    .(voix = sum(voix, na.rm = TRUE)),
    by = .(bureau_id, liste)
  ]

  # passage en large
  live_wide <- dcast(
    live_agg,
    bureau_id ~ liste,
    value.var = "voix",
    fill = 0
  )

  # ajouter les catégories manquantes si besoin
  missing_cols <- setdiff(category_names, names(live_wide))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      live_wide[, (col) := 0]
    }
  }

  # garder l'ordre souhaité
  setcolorder(live_wide, c("bureau_id", category_names))

  # N reconstruit à partir du simplex sur inscrits
  live_wide[, N := rowSums(.SD), .SDcols = category_names]

  # table bureau_id | N observé
  meta_obs <- live_wide[, .(bureau_id, N)]

  list(
    live_wide = live_wide,
    meta_obs = meta_obs
  )
}

softmax_mat <- function(eta) {
  eta <- eta - apply(eta, 1, max)
  exp_eta <- exp(eta)
  exp_eta / rowSums(exp_eta)
}

unpack_params <- function(theta, A, K) {
  n_alpha <- K - 1
  alpha_free <- theta[1:n_alpha]
  B_free <- matrix(
    theta[(n_alpha + 1):length(theta)],
    nrow = A,
    ncol = K - 1
  )
  list(alpha_free = alpha_free, B_free = B_free)
}

compute_probs <- function(W, theta, A, K) {
  pars <- unpack_params(theta, A, K)
  eta_free <- sweep(W %*% pars$B_free, 2, pars$alpha_free, `+`)
  eta <- cbind(eta_free, 0) # catégorie de référence
  softmax_mat(eta)
}

negloglik_multinom <- function(theta, W, Y, lambda = 1e-2, theta_prior = NULL) {
  A <- ncol(W)
  K <- ncol(Y)

  P <- compute_probs(W, theta, A, K)
  eps <- 1e-12
  ll <- sum(Y * log(P + eps))

  if (is.null(theta_prior)) {
    penalty <- lambda * sum(theta^2)
  } else {
    penalty <- lambda * sum((theta - theta_prior)^2)
  }

  -(ll - penalty)
}

fit_live_model <- function(W_obs, Y_obs, theta_init = NULL, lambda = 1e-2, theta_prior = NULL) {
  A <- ncol(W_obs)
  K <- ncol(Y_obs)

  if (is.null(theta_init)) {
    theta_init <- rep(0, (K - 1) + A * (K - 1))
  }

  optim(
    par = theta_init,
    fn = negloglik_multinom,
    W = W_obs,
    Y = Y_obs,
    lambda = lambda,
    theta_prior = theta_prior,
    method = "BFGS",
    control = list(maxit = 1000, reltol = 1e-9)
  )
}

predict_missing_counts <- function(W_mis, N_mis, theta, K) {
  P_mis <- compute_probs(W_mis, theta, ncol(W_mis), K)
  Y_hat <- P_mis * N_mis
  list(
    probs = P_mis,
    expected_counts = Y_hat
  )
}

aggregate_projection <- function(Y_obs, Y_mis_hat, category_names) {
  total_obs <- colSums(Y_obs)
  total_mis <- colSums(Y_mis_hat)

  total <- total_obs + total_mis
  shares <- total / sum(total)

  data.table(
    category = category_names,
    projected_count = as.numeric(total),
    projected_share = as.numeric(shares)
  )
}

update_projection_long <- function(live_long,
                                   W,
                                   meta_all,
                                   category_names,
                                   theta_current = NULL,
                                   lambda = 1e-2,
                                   theta_prior = NULL) {
  stopifnot(all(c("bureau_id", "N") %in% names(meta_all)))

  prep <- prepare_live_from_long(live_long, category_names)
  live_wide <- prep$live_wide

  obs_ids <- live_wide$bureau_id
  all_ids <- rownames(W)
  mis_ids <- setdiff(all_ids, obs_ids)

  # Alignement observés
  W_obs <- W[obs_ids, , drop = FALSE]
  Y_obs <- as.matrix(live_wide[, ..category_names])

  # Contrôle N observé reconstruit vs N théorique
  N_obs_rebuilt <- live_wide$N
  N_obs_theoretical <- meta_all[match(obs_ids, bureau_id), N]

  if (any(!is.na(N_obs_theoretical) & N_obs_rebuilt != N_obs_theoretical)) {
    warning("Écart entre N reconstruit depuis le flux live et N théorique pour certains bureaux.")
  }

  # Bureaux manquants
  W_mis <- W[mis_ids, , drop = FALSE]
  N_mis <- meta_all[match(mis_ids, bureau_id), N]

  if (any(is.na(N_mis))) {
    stop("N manquant pour au moins un bureau non observé. Impossible de projeter sans masse totale.")
  }

  fit <- fit_live_model(
    W_obs = W_obs,
    Y_obs = Y_obs,
    theta_init = theta_current,
    lambda = lambda,
    theta_prior = theta_prior
  )

  theta_new <- fit$par

  pred <- predict_missing_counts(
    W_mis = W_mis,
    N_mis = N_mis,
    theta = theta_new,
    K = length(category_names)
  )

  proj <- aggregate_projection(
    Y_obs = Y_obs,
    Y_mis_hat = pred$expected_counts,
    category_names = category_names
  )

  list(
    theta = theta_new,
    fit = fit,
    projection = proj,
    live_wide = live_wide,
    observed_ids = obs_ids,
    missing_ids = mis_ids,
    pred_missing = pred
  )
}
