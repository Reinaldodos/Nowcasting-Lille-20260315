library(tidyverse)

test_data <-
  "https://tabular-api.data.gouv.fr/api/resources/8fb977b3-57be-44b9-bd8e-ef0a130a003d/data/json/" |>
  jsonlite::read_json(simplifyVector = TRUE) |>
  filter(numero_tour == 1) |>
  mutate(code_bdv = str_pad(string = `__id`, width = 4, side = "left", pad = "0")) |>
  mutate(bureau_id = str_c("59_350_", code_bdv)) |>
  slice(1:nrow(arch))

test_data$bureau_id <- arch$geo_id

meta_all <-
  test_data |>
  select(bureau_id, N = nb_inscrits, nb_exprimes) |>
  readr::type_convert()

resultats <-
  test_data |>
  mutate_all(.funs = as.character) |>
  select(bureau_id, contains("candidat"), contains("pourcentage")) |>
  pivot_longer(cols = -bureau_id, names_to = "type", values_to = "nom") |>
  tidyr::separate(col = type, sep = "_", into = c("type", "numero")) |>
  pivot_wider(names_from = type, values_from = nom) |>
  mutate(pourcentage = as.numeric(pourcentage)) |>
  inner_join(
    y = meta_all,
    by = join_by(bureau_id)
  ) |>
  mutate(voix = round(nb_exprimes * pourcentage / 100)) |>
  select(bureau_id, candidat, voix)

inscrits <-
  test_data |>
  readr::type_convert() |>
  transmute(bureau_id, nb_nuls, nb_blancs,
    abstention = nb_inscrits - nb_votants
  ) |>
  pivot_longer(cols = -bureau_id, names_to = "candidat", values_to = "voix") |>
  bind_rows(resultats) |>
  rename(liste = candidat)
