theta <- res$theta
rowSums(theta)
apply(theta, 1, min)

res

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

plotly::ggplotly(last_plot())

delta_projections <-
  inscrits |>
  summarise(
    voix_reel = sum(voix),
    .by = c(liste)
  ) |>
  inner_join(y = projections) |>
  mutate(
    delta = abs(voix_reel - voix_proj),
    ratio = delta / voix_reel
  )

delta_projections |>
  ggplot(mapping = aes(x = nb_bureaux_obs, y = delta, colour = liste)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom")

delta_projections |>
  filter(nb_bureaux_obs == max(nb_bureaux_obs)) |>
  ggplot(mapping = aes(x = voix_reel, y = voix_proj, colour = liste)) +
  geom_point() +
  geom_abline()
