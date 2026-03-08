source("fonctions_tidyverse.R")

theta_current <- NULL
output <- list()

for (k in 1:nrow(meta_all)) {
  cat(k, sep = "\n")
  nb_obs <- k

  source(file = "prep_dummy.R")

  res <- update_projection_long(
    live_long = live_long,
    W = W,
    meta_all = meta_all,
    category_names = category_names,
    theta_current = theta_current,
    lambda = 1e-2
  )

  output[[k]] <- res$projection$long

  theta_current <- res$theta
}
