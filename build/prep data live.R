get_live_data <- function(raw_data) {
  library(tidyverse)
  input <-
    raw_data |>
    mutate(
      BV = as.numeric(BV),
      code_bdv = str_pad(string = BV, width = 4, side = "left", pad = "0"),
      bureau_id = str_c("59_350_", code_bdv)
    ) |>
    drop_na(BV)

  meta_all <-
    input |>
    select(bureau_id, N = Inscrits)

  live_long <-
    input |>
    mutate(ABSTENTION = Inscrits - Votants) |>
    select(bureau_id, all_of(listes)) |>
    pivot_longer(
      cols = -bureau_id,
      names_to = "liste",
      values_to = "voix",
      values_drop_na = TRUE
    )

  list(
    meta_all = meta_all,
    live_long = live_long
  )
}
