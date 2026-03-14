source("build/prep_W.R")
source("fonctions/fonctions_tidyverse.R")
source(file = "build/prep data live.R")

run_model_selon_k <- function(live_long,
                              meta_all,
                              listes,
                              archfile,
                              theta_current = NULL,
                              lambda = 1e-4) {
  archfile |>
    create_W() |>
    update_projection_long(
      live_long = live_long,
      meta_all = meta_all,
      category_names = listes
    )
}

get_outputs_from_res <- function(res) {
  nb_bdv_obs <- res$projection$observed_bureau_ids |> length()

  bdv_proj <-
    res$projection$wide |>
    mutate(across(
      .cols = all_of(listes),
      .fns = ~ round(N * .)
    ))

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

  scores_exprimes <-
    output_inscrits |>
    filter(exprimes) |>
    mutate(score = scales::percent(voix_proj / sum(voix_proj),
      accuracy = .01
    )) |>
    select(-exprimes)

  list(
    "modele" = res,
    "nb bureaux observés" = nb_bdv_obs,
    "projection par bureau" = bdv_proj,
    "résultats" = output_inscrits,
    "scores par liste" = scores_exprimes
  )
}
