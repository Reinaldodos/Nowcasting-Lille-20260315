source("fonctions_tidyverse.R")
source("prep_W.R")
source("prep_data_2020.R")

W_arch <- create_W(arch_file = archfile_5)

theta_current <- NULL
output <- list()

for (k in 1:nrow(meta_all)) {
  cat(k, sep = "\n")
  nb_obs <- k

  source(file = "prep_dummy.R")

  res <- update_projection_long(
    live_long = live_long,
    W = W_arch,
    meta_all = meta_all,
    category_names = category_names,
    theta_current = theta_current,
    lambda = 1e-4
  )

  output[[k]] <- res$projection$long

  theta_current <- res$theta
}
