library(tidyverse)

test_data <-
  "data/M20_lille.parquet" |>
  arrow::read_parquet() |>
  filter(tour == 1) |>
  mutate(code_bdv = str_pad(string = bv, width = 4, side = "left", pad = "0")) |>
  mutate(bureau_id = str_c("59_350_", code_bdv)) |>
  rename(
    nb_inscrits = inscrits,
    nb_exprimes = exprimes
  )

meta_all <-
  test_data |>
  distinct(bureau_id, N = nb_inscrits, nb_exprimes) |>
  readr::type_convert()

inscrits <-
  test_data |>
  mutate_all(.funs = as.character) |>
  select(bureau_id, candidat = candidats, voix) |>
  readr::type_convert() |>
  rename(liste = candidat)
