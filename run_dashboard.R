library(googlesheets4)
library(digest)

sheet_id <- "1cIiW13JaqudBTFH_Nz23mZhQXeuhXbjASk2SGsVwBX0"
# sheet_id <- "1IJygvgzIgtbWsp8WWI1dkO21p2iuyyMc6wQpjsSJwEc"
# sheet_id <- "1cIiW13JaqudBTFH_Nz23mZhQXeuhXbjASk2SGsVwBX0"
sheet_id <- "1IJygvgzIgtbWsp8WWI1dkO21p2iuyyMc6wQpjsSJwEc"

last_hash <- NULL

listes <- c(
  "ABSTENTION", "B&N", "DESLANDES", "SPILLEBOUT", "DELEMER", "BALY",
  "SCALI", "MADELAIN", "VALET", "BENYOUCEF", "ADDOUCHE"
)

source("build/modele.R")

repeat {
  message("Screening des résultats: ", Sys.time())

  df <- tryCatch(
    suppressMessages(read_sheet(ss = sheet_id, skip = 2)),
    error = function(e) {
      message("Erreur lecture Google Sheet: ", e$message)
      return(NULL)
    }
  )

  if (!is.null(df)) {
    new_hash <- digest(df)

    if (!identical(new_hash, last_hash)) {
      message("Changement détecté → render")

      live_data <- get_live_data(raw_data = df)

      rmarkdown::render(
        input = "Dashboard.Rmd",
        params = list(
          timestamp = Sys.time(),
          live_data = live_data,
          listes = listes
        )
      )

      last_hash <- digest(df)
    }
  }

  Sys.sleep(10)
}
