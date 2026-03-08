library(dplyr)
library(tidyr)

# -----------------------------
# Outils simplex
# -----------------------------

softmax <- function(x) {
  z <- x - max(x)
  ez <- exp(z)
  ez / sum(ez)
}

project_strict_simplex <- function(x, eps = 1e-8) {
  y <- pmax(x, eps)
  y / sum(y)
}

theta_to_eta <- function(theta_row, ref = NULL, eps = 1e-8) {
  theta_row <- project_strict_simplex(theta_row, eps = eps)
  J <- length(theta_row)
  if (is.null(ref)) ref <- J
  eta <- log(theta_row[-ref] / theta_row[ref])
  as.numeric(eta)
}

eta_to_theta <- function(eta_row, J, ref = NULL) {
  if (is.null(ref)) ref <- J
  full <- numeric(J)
  full[-ref] <- eta_row
  full[ref] <- 0
  softmax(full)
}

pack_theta_from_eta <- function(eta_vec, K, J, ref = NULL) {
  if (is.null(ref)) ref <- J
  n_par <- J - 1
  eta_mat <- matrix(eta_vec, nrow = K, ncol = n_par, byrow = TRUE)
  theta <- t(apply(eta_mat, 1, eta_to_theta, J = J, ref = ref))
  rownames(theta) <- paste0("X", seq_len(K))
  theta
}

unpack_eta_from_theta <- function(theta, ref = NULL, eps = 1e-8) {
  K <- nrow(theta)
  eta <- lapply(seq_len(K), function(k) theta_to_eta(theta[k, ], ref = ref, eps = eps))
  unlist(eta, use.names = FALSE)
}

# -----------------------------
# Préparation des résultats live
# -----------------------------

prepare_live_from_long <- function(live_long, category_names) {
  required_cols <- c("bureau_id", "liste", "voix")
  missing_cols <- setdiff(required_cols, names(live_long))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Colonnes manquantes dans live_long: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  live_agg <- live_long %>%
    transmute(
      bureau_id = as.character(.data$bureau_id),
      liste = as.character(.data$liste),
      voix = as.numeric(.data$voix)
    ) %>%
    group_by(.data$bureau_id, .data$liste) %>%
    summarise(voix = sum(.data$voix, na.rm = TRUE), .groups = "drop")

  unknown_lists <- setdiff(unique(live_agg$liste), category_names)
  if (length(unknown_lists) > 0) {
    stop(sprintf(
      "Catégories inconnues dans live_long: %s",
      paste(unknown_lists, collapse = ", ")
    ))
  }

  live_wide <- live_agg %>%
    mutate(liste = factor(.data$liste, levels = category_names)) %>%
    tidyr::pivot_wider(
      names_from = .data$liste,
      values_from = .data$voix,
      values_fill = 0
    )

  # Garantit l'ordre des colonnes
  for (nm in category_names) {
    if (!nm %in% names(live_wide)) {
      live_wide[[nm]] <- 0
    }
  }

  live_wide <- live_wide %>%
    select(.data$bureau_id, all_of(category_names))

  Y_counts <- as.matrix(live_wide[, category_names, drop = FALSE])
  rownames(Y_counts) <- live_wide$bureau_id

  row_totals <- rowSums(Y_counts)
  if (any(row_totals <= 0)) {
    bad_ids <- rownames(Y_counts)[row_totals <= 0]
    stop(sprintf(
      "Certains bureaux ont un total de voix nul: %s",
      paste(head(bad_ids, 20), collapse = ", ")
    ))
  }

  Y <- Y_counts / row_totals

  list(
    bureau_id = live_wide$bureau_id,
    Y_counts = Y_counts,
    Y = Y,
    totals = row_totals,
    live_wide = live_wide
  )
}

# -----------------------------
# Appariement bureaux -> lignes de W
# -----------------------------

match_bureaux_to_W <- function(bureau_ids, W, meta_all = NULL) {
  bureau_ids <- as.character(bureau_ids)

  # Cas 1 : rownames(W) = bureau_id
  rw <- rownames(W)
  if (!is.null(rw)) {
    rw <- as.character(rw)
    idx <- match(bureau_ids, rw)
    if (!anyNA(idx)) {
      return(idx)
    }
  }

  # Cas 2 : mapping via meta_all$bureau_id
  if (is.null(meta_all) || !("bureau_id" %in% names(meta_all))) {
    stop("Impossible d'apparier les bureaux à W : ni rownames(W), ni meta_all$bureau_id utilisable.")
  }

  meta_ids <- as.character(meta_all$bureau_id)
  idx <- match(bureau_ids, meta_ids)

  if (anyNA(idx)) {
    missing_ids <- bureau_ids[is.na(idx)]
    stop(sprintf(
      "Bureaux absents du référentiel W/meta_all: %s",
      paste(head(unique(missing_ids), 20), collapse = ", ")
    ))
  }

  idx
}

# -----------------------------
# Estimation contrainte simplex
# -----------------------------

fit_theta_simplex <- function(W_obs,
                              Y_obs,
                              theta_init = NULL,
                              lambda = 1e-3,
                              ref = NULL,
                              active_tol = 1e-12,
                              maxit = 1000,
                              trace = 0) {
  W_obs <- as.matrix(W_obs)
  Y_obs <- as.matrix(Y_obs)

  n <- nrow(W_obs)
  K <- ncol(W_obs)
  J <- ncol(Y_obs)

  if (nrow(Y_obs) != n) {
    stop("W_obs et Y_obs doivent avoir le même nombre de lignes.")
  }

  if (is.null(ref)) ref <- J

  # Archétypes identifiés ou non par les observations live
  active_k <- which(colSums(abs(W_obs)) > active_tol)
  inactive_k <- setdiff(seq_len(K), active_k)

  if (length(active_k) == 0) {
    stop("Aucun archétype actif dans W_obs. Estimation impossible.")
  }

  W_act <- W_obs[, active_k, drop = FALSE]
  K_act <- length(active_k)

  # Initialisation theta
  if (is.null(theta_init)) {
    theta_full_init <- matrix(1 / J, nrow = K, ncol = J)
  } else {
    theta_full_init <- as.matrix(theta_init)
    if (!all(dim(theta_full_init) == c(K, J))) {
      stop("theta_init n'a pas les bonnes dimensions.")
    }
    theta_full_init <- t(apply(theta_full_init, 1, project_strict_simplex))
  }

  theta_act_init <- theta_full_init[active_k, , drop = FALSE]
  eta_init <- unpack_eta_from_theta(theta_act_init, ref = ref)

  objective <- function(eta_vec) {
    theta_act <- pack_theta_from_eta(eta_vec, K = K_act, J = J, ref = ref)
    Y_hat <- W_act %*% theta_act

    resid <- Y_obs - Y_hat
    loss_fit <- sum(resid^2)

    # Pénalisation sur les logits, pas sur theta directement
    loss_pen <- lambda * sum(eta_vec^2)

    loss_fit + loss_pen
  }

  opt <- optim(
    par = eta_init,
    fn = objective,
    method = "BFGS",
    control = list(maxit = maxit, trace = trace, REPORT = 20)
  )

  theta_act_hat <- pack_theta_from_eta(opt$par, K = K_act, J = J, ref = ref)

  # Reconstitution theta complet
  theta_hat <- theta_full_init
  theta_hat[active_k, ] <- theta_act_hat

  # Inactifs : on garde l'init, donc theta_current si fourni, sinon uniforme
  theta_hat <- t(apply(theta_hat, 1, project_strict_simplex))

  rownames(theta_hat) <- colnames(W_obs)
  colnames(theta_hat) <- colnames(Y_obs)

  list(
    theta = theta_hat,
    active_k = active_k,
    inactive_k = inactive_k,
    convergence = opt$convergence,
    value = opt$value,
    counts = opt$counts,
    message = opt$message
  )
}

# -----------------------------
# Projection finale
# -----------------------------

update_projection_long_simplex <- function(live_long,
                                           W,
                                           meta_all,
                                           category_names,
                                           theta_current = NULL,
                                           lambda = 1e-3,
                                           ref = NULL,
                                           maxit = 1000,
                                           trace = 0) {
  W <- as.matrix(W)

  if (is.null(colnames(W))) {
    colnames(W) <- paste0("X", seq_len(ncol(W)))
  }

  prep <- prepare_live_from_long(live_long, category_names)

  obs_rows <- match_bureaux_to_W(prep$bureau_id, W, meta_all = meta_all)
  W_obs <- W[obs_rows, , drop = FALSE]

  # theta_current éventuel
  if (!is.null(theta_current)) {
    theta_current <- as.matrix(theta_current)
    if (is.null(rownames(theta_current))) {
      rownames(theta_current) <- colnames(W)
    }
    if (is.null(colnames(theta_current))) {
      colnames(theta_current) <- category_names
    }

    # Réordonne proprement
    theta_current <- theta_current[colnames(W), category_names, drop = FALSE]
  }

  fit <- fit_theta_simplex(
    W_obs = W_obs,
    Y_obs = prep$Y,
    theta_init = theta_current,
    lambda = lambda,
    ref = ref,
    maxit = maxit,
    trace = trace
  )

  theta_hat <- fit$theta

  # Projection sur tous les bureaux
  Y_hat_all <- W %*% theta_hat

  # Sécurité numérique
  Y_hat_all[Y_hat_all < 0] <- 0
  Y_hat_all <- Y_hat_all / rowSums(Y_hat_all)

  bureau_ref <- if (!is.null(rownames(W))) {
    rownames(W)
  } else if (!is.null(meta_all) && "bureau_id" %in% names(meta_all)) {
    as.character(meta_all$bureau_id)
  } else {
    as.character(seq_len(nrow(W)))
  }

  projection_wide <- as.data.frame(Y_hat_all)
  projection_wide <- tibble::as_tibble(projection_wide)
  projection_wide <- dplyr::mutate(projection_wide, bureau_id = bureau_ref, .before = 1)

  projection_long <- projection_wide %>%
    tidyr::pivot_longer(
      cols = all_of(category_names),
      names_to = "liste",
      values_to = "part"
    )

  observed_fit <- as.data.frame(W_obs %*% theta_hat)
  observed_fit[observed_fit < 0] <- 0
  observed_fit <- observed_fit / rowSums(observed_fit)
  observed_fit <- tibble::as_tibble(observed_fit)
  observed_fit <- dplyr::mutate(observed_fit, bureau_id = prep$bureau_id, .before = 1)

  list(
    theta = theta_hat,
    fit = fit,
    projection = list(
      wide = projection_wide,
      long = projection_long,
      observed_fit = observed_fit
    ),
    prep = prep
  )
}
