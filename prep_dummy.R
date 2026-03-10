# nb_obs <- 20
live_long <-
  meta_all |>
  arrange(N) |>
  slice(1:nb_obs) |>
  semi_join(
    x = inscrits,
    by = join_by(bureau_id)
  )

category_names <-
  unique(live_long$liste) |>
  unique() |>
  sort()
