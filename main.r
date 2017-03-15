library(readxl)

read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  sheets <- sheets[3:length(sheets)]
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("Road-Accident-Safety-Data-Guide.xls")


removeNA <- function(data.frame.list) {
  x <- lapply(names(data.frame.list), function(X) data.frame.list[[X]][!is.na(data.frame.list[[X]])])
  x
}

mysheets <- removeNA(mysheets)