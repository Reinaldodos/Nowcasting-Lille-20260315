archfile_9 <- "data/archetypes_Lille.csv"
archfile_5 <- "data/archetypes_Lille_k5.csv"

create_W <- function(arch_file) {
  arch <- readr::read_csv(file = arch_file)

  archetype_cols <- setdiff(names(arch), "geo_id")

  W <- as.matrix(arch[, archetype_cols])
  W <- W / rowSums(W)
  rownames(W) <- arch$geo_id

  W
}
