library(googlesheets4)
library(digest)

sheet_id <- "1cIiW13JaqudBTFH_Nz23mZhQXeuhXbjASk2SGsVwBX0"
# sheet_id <- "1moiNM2Mlp4wkrPX6bJusTGBMDNFSdeQfYXN2lYkfbik"

last_hash <- NULL

repeat {
  message("Screening des résultats: ", Sys.time())

  df <- tryCatch(
    suppressMessages(read_sheet(sheet_id)),
    error = function(e) {
      message("Erreur lecture Google Sheet: ", e$message)
      return(NULL)
    }
  )

  if (!is.null(df)) {
    new_hash <- digest(df)

    if (!identical(new_hash, last_hash)) {
      message("Changement détecté → render")

      rmarkdown::render(
        "Dashboard.Rmd",
        params = list(timestamp = Sys.time())
      )

      last_hash <- digest(df)
    }
  }

  Sys.sleep(30)
}
