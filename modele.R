theta_current <- NULL

res <- update_projection_long(
  live_long = live_long,
  W = W,
  meta_all = meta_all,
  category_names = category_names,
  theta_current = theta_current,
  lambda = 1e-2
)

theta_current <- res$theta
res$projection
