source("prep_W.R")
W_arch <- create_W(arch_file = archfile_9)

res <- NULL

source(file = "fonctions_tidyverse.R")

source(file = "prep data live.R")
res <- update_projection_long(
  live_long = live_long,
  W = W_arch,
  meta_all = meta_all,
  category_names = listes,
  theta_current = res$theta,
  lambda = 1e-4
)

res$projection$wide |>
  mutate(across(
    .cols = all_of(listes),
    .fns = ~ round(N * .)
  )) |>
  DT::datatable()

output_inscrits <-
  res$projection$long |>
  inner_join(
    x = meta_all,
    by = join_by(bureau_id)
  ) |>
  mutate(voix_proj = round(part_proj * N)) |>
  summarise(
    voix_proj = sum(voix_proj, na.rm = TRUE),
    .by = c(liste)
  ) |>
  mutate(exprimes = !liste %in% c("ABSTENTION", "B&N")) |>
  arrange(-voix_proj)

output_inscrits |>
  filter(exprimes) |>
  mutate(score = scales::percent(voix_proj / sum(voix_proj),
    accuracy = .01
  )) |>
  select(-exprimes) |>
  gt::gt()
