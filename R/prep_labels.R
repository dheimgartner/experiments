prep_labels <- function(lang = c("en", "de")) {
  lang <- match.arg(lang)

  path <- glue::glue("./data-raw/labels_{lang}.xlsx")
  sheets <- readxl::excel_sheets(path)

  labels <-
    sheets %>%
    map(function(s) {
      readxl::read_xlsx(path, sheet = s)
    })

  names(labels) <- sheets

  nn <- sheets[!(sheets %in% c("vehicle_type", "fuel_type"))]
  bl <- labels[nn]
  bundles <- c("A", "B")

  labels_ <- labels[!(names(labels) %in% nn)]

  bundle_labels <-
    bundles %>%
    map(function(b) {
      nn <- names(bl)
      nn <- paste(b, nn, sep = "_")
      names(bl) <- nn
      bl
    })

  A_bl <- bundle_labels[[1]]
  B_bl <- bundle_labels[[2]]

  labels <-
    labels_ %>%
    append(A_bl) %>%
    append(B_bl)

  return(labels)

}
