library(data.table)

arch <- fread("data/archetypes_Lille.csv")

archetype_cols <- setdiff(names(arch), "geo_id")

W <- as.matrix(arch[, ..archetype_cols])
W <- W / rowSums(W)
rownames(W) <- arch$geo_id
