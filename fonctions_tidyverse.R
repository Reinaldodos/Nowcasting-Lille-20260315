# Tidyverse rewrite of the live-projection helpers
# ------------------------------------------------
# Hypothesis of the model:
# - W is a bureau x archetype weight matrix
# - theta is an archetype x category parameter matrix to estimate on the fly
# - the current election result at bureau level is approximated by:
#       Y ~= W %*% theta
#   where Y is bureau x category, expressed on the simplex requested by the user
#
# Inputs expected by the main function:
# - live_long: tibble/data.frame with columns bureau_id, liste, voix
# - W: matrix or data.frame, one row per bureau, one column per archetype
# - meta_all: data.frame/tibble containing at least bureau_id when W has no rownames
# - category_names: character vector, exhaustive ordered list of categories to predict
# - theta_current: optional prior matrix K x C, same dimensions as theta
# - lambda: ridge penalty
#
# Output:
# - theta: estimated archetype x category matrix
# - projection: list with wide/long outputs and some diagnostics

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(rlang)
})

assert_has_cols <- function(x, cols, object_name = deparse(substitute(x))) {
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    abort(sprintf(
      "%s is missing required columns: %s",
      object_name,
      paste(missing, collapse = ", ")
    ))
  }
}

as_W_matrix <- function(W) {
  if (is.matrix(W)) {
    return(W)
  }

  if (is.data.frame(W)) {
    W_num <- W %>% dplyr::select(where(is.numeric))
    if (ncol(W_num) == 0) {
      abort("W does not contain any numeric archetype columns.")
    }
    return(as.matrix(W_num))
  }

  abort("W must be a matrix or a data.frame.")
}

resolve_w_rows <- function(bureau_ids, W, meta_all = NULL) {
  # 1) Best case: rownames(W) are bureau_id
  rn <- rownames(W)
  if (!is.null(rn) && all(bureau_ids %in% rn)) {
    idx <- match(bureau_ids, rn)
    return(idx)
  }

  # 2) Fallback: meta_all provides the bureau_id -> row mapping
  if (is.null(meta_all)) {
    abort("Unable to align W with bureau_id: add rownames(W) or provide meta_all with bureau_id.")
  }

  meta_all <- as_tibble(meta_all)
  assert_has_cols(meta_all, "bureau_id", "meta_all")

  idx <- match(bureau_ids, meta_all$bureau_id)

  if (anyNA(idx)) {
    missing_ids <- bureau_ids[is.na(idx)]
    abort(sprintf(
      "Some live bureau_id are absent from W/meta_all: %s",
      paste(head(missing_ids, 20), collapse = ", ")
    ))
  }

  if (any(idx < 1 | idx > nrow(W))) {
    bad <- idx[idx < 1 | idx > nrow(W)]
    abort(sprintf(
      "Row indices outside W bounds. nrow(W) = %s ; faulty indices: %s",
      nrow(W), paste(head(bad, 20), collapse = ", ")
    ))
  }

  idx
}

prepare_live_from_long <- function(live_long, category_names) {
  live_long <- as_tibble(live_long)
  assert_has_cols(live_long, c("bureau_id", "liste", "voix"), "live_long")

  if (!is.numeric(live_long$voix)) {
    abort("live_long$voix must be numeric.")
  }

  live_agg <- live_long %>%
    transmute(
      bureau_id = as.character(.data$bureau_id),
      liste = as.character(.data$liste),
      voix = as.numeric(.data$voix)
    ) %>%
    filter(!is.na(.data$bureau_id), !is.na(.data$liste)) %>%
    group_by(.data$bureau_id, .data$liste) %>%
    summarise(voix = sum(.data$voix, na.rm = TRUE), .groups = "drop") %>%
    mutate(liste = factor(.data$liste, levels = category_names)) %>%
    filter(!is.na(.data$liste))

  if (nrow(live_agg) == 0) {
    abort("No valid live observation remains after aggregation/filtering.")
  }

  live_wide <- live_agg %>%
    tidyr::complete(
      bureau_id,
      liste = factor(category_names, levels = category_names),
      fill = list(voix = 0)
    ) %>%
    mutate(liste = as.character(.data$liste)) %>%
    pivot_wider(
      names_from = .data$liste,
      values_from = .data$voix,
      values_fill = 0
    ) %>%
    arrange(.data$bureau_id)

  # Ensure column order exactly follows category_names
  missing_categories <- setdiff(category_names, names(live_wide))
  if (length(missing_categories) > 0) {
    for (nm in missing_categories) {
      live_wide[[nm]] <- 0
    }
  }

  live_wide <- live_wide %>%
    select(.data$bureau_id, all_of(category_names))

  Y_obs <- live_wide %>%
    select(all_of(category_names)) %>%
    as.matrix()

  row_totals <- rowSums(Y_obs, na.rm = TRUE)
  zero_rows <- which(row_totals <= 0)
  if (length(zero_rows) > 0) {
    abort(sprintf(
      "At least one observed bureau has a non-positive total of voix. Problematic bureau_id: %s",
      paste(live_wide$bureau_id[zero_rows], collapse = ", ")
    ))
  }

  Y_obs_share <- Y_obs / row_totals

  list(
    live_agg = live_agg,
    live_wide = live_wide,
    bureau_ids = live_wide$bureau_id,
    n_obs = row_totals,
    Y_obs_counts = Y_obs,
    Y_obs = Y_obs_share
  )
}

estimate_theta_ridge <- function(W_obs, Y_obs, lambda = 1e-2, theta_current = NULL) {
  if (!is.matrix(W_obs)) W_obs <- as.matrix(W_obs)
  if (!is.matrix(Y_obs)) Y_obs <- as.matrix(Y_obs)

  K <- ncol(W_obs)
  C <- ncol(Y_obs)

  if (!is.null(theta_current)) {
    theta_current <- as.matrix(theta_current)
    if (!all(dim(theta_current) == c(K, C))) {
      abort(sprintf(
        "theta_current has dimension %s x %s, expected %s x %s.",
        nrow(theta_current), ncol(theta_current), K, C
      ))
    }
  } else {
    theta_current <- matrix(0, nrow = K, ncol = C)
  }

  XtX <- crossprod(W_obs)
  XtY <- crossprod(W_obs, Y_obs)
  ridge <- lambda * diag(K)

  theta <- solve(XtX + ridge, XtY + lambda * theta_current)

  # Numerical hygiene: project back to the simplex by row of bureau after prediction,
  # not on theta itself, because theta can in principle have negative entries.
  theta
}

normalize_rows <- function(M, eps = 1e-12) {
  rs <- rowSums(M, na.rm = TRUE)
  rs[rs < eps] <- 1
  M / rs
}

update_projection_long <- function(live_long,
                                   W,
                                   meta_all,
                                   category_names,
                                   theta_current = NULL,
                                   lambda = 1e-2) {
  prep <- prepare_live_from_long(live_long, category_names)

  W_mat <- as_W_matrix(W)
  obs_rows <- resolve_w_rows(prep$bureau_ids, W_mat, meta_all)
  W_obs <- W_mat[obs_rows, , drop = FALSE]

  theta <- estimate_theta_ridge(
    W_obs = W_obs,
    Y_obs = prep$Y_obs,
    lambda = lambda,
    theta_current = theta_current
  )

  Y_hat <- W_mat %*% theta
  Y_hat <- pmax(Y_hat, 0)
  Y_hat <- normalize_rows(Y_hat)
  colnames(Y_hat) <- category_names

  meta_tbl <- as_tibble(meta_all)
  assert_has_cols(meta_tbl, "bureau_id", "meta_all")

  if (nrow(meta_tbl) != nrow(Y_hat)) {
    abort(sprintf(
      "meta_all has %s rows whereas W implies %s rows. They must be aligned.",
      nrow(meta_tbl), nrow(Y_hat)
    ))
  }

  projection_wide <- meta_tbl %>%
    mutate(bureau_id = as.character(.data$bureau_id)) %>%
    bind_cols(as_tibble(Y_hat))

  projection_long <- projection_wide %>%
    select(.data$bureau_id, all_of(category_names)) %>%
    pivot_longer(
      cols = all_of(category_names),
      names_to = "liste",
      values_to = "part_proj"
    )

  observed_fit <- tibble(
    bureau_id = prep$bureau_ids,
    rmse_obs = sqrt(rowMeans((prep$Y_obs - normalize_rows(W_obs %*% theta))^2))
  )

  list(
    theta = theta,
    projection = list(
      wide = projection_wide,
      long = projection_long,
      observed_fit = observed_fit,
      observed_bureau_ids = prep$bureau_ids
    )
  )
}

# Example usage -------------------------------------------------------------
# source("fonctions_tidyverse.R")
# theta_current <- NULL
# res <- update_projection_long(
#   live_long = live_long,
#   W = W,
#   meta_all = meta_all,
#   category_names = category_names,
#   theta_current = theta_current,
#   lambda = 1e-2
# )
# theta_current <- res$theta
# res$projection$wide
# res$projection$long
