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

projections <-
  output |>
  bind_rows(.id = "nb_bureaux_obs") |>
  inner_join(y = meta_all) |>
  mutate(voix = N * part_proj) |>
  summarise(
    voix_proj = sum(voix),
    .by = c(liste, nb_bureaux_obs)
  ) |>
  readr::type_convert()

projections |>
  ggplot(mapping = aes(x = nb_bureaux_obs, y = voix_proj, colour = liste)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom")

inscrits |>
  summarise(
    voix_reel = sum(voix),
    .by = c(liste)
  ) |>
  inner_join(y = projections) |>
  mutate(
    delta = abs(voix_reel - voix_proj),
    ratio = delta / voix_reel
  ) |>
  ggplot(mapping = aes(x = nb_bureaux_obs, y = delta, colour = liste)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom")

projection |>
  ggplot(mapping = aes(x = voix_reel, y = voix_proj)) +
  geom_point() +
  geom_abline() +
  ggrepel::geom_text_repel(mapping = aes(label = liste))
