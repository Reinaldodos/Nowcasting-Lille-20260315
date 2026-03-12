url <- "https://docs.google.com/spreadsheets/d/1cIiW13JaqudBTFH_Nz23mZhQXeuhXbjASk2SGsVwBX0/edit?usp=sharing"

# url <- "https://docs.google.com/spreadsheets/d/1moiNM2Mlp4wkrPX6bJusTGBMDNFSdeQfYXN2lYkfbik/edit?gid=2142927731#gid=2142927731"

raw_data <-
  googlesheets4::read_sheet(
    ss = url, sheet = 1, skip = 2, n_max = 126
  )

listes <- c("ABSTENTION", colnames(raw_data)[c(6, 8:16)])

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
