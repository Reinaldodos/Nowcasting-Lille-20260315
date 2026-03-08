live_long <-
  meta_all |>
  sample_frac(size = 0.1) |>
  semi_join(
    x = inscrits,
    by = join_by(bureau_id)
  )

category_names <- live_long$liste |>
  unique() |>
  sort()
