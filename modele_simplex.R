source("fonctions_simplex.R")

theta_current <- NULL
output <- list()

for (k in 1:nrow(meta_all)) {
  nb_obs <- k

  source(file = "prep_dummy.R")

  res <- update_projection_long_simplex(
    live_long = live_long,
    W = W,
    meta_all = meta_all,
    category_names = category_names,
    theta_current = theta_current,
    lambda = 1e-3,
    maxit = 2000,
    trace = 1
  )

  output[[k]] <- res$projection$long

  theta_current <- res$theta
}
