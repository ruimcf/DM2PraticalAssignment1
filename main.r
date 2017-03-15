library(readxl)

read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  #sheets <- sheets[3:length(sheets)]
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("Road-Accident-Safety-Data-Guide.xls")
data <- read.csv(file="Accidents_2015.csv", header=TRUE)
  
# Converts code values of col col_name to labels in excel_section_name
convert_codes <- function(df, col_name, excel_section_name) {
  df[[col_name]] <- sapply(df[[col_name]], function(i) subset(mysheets[[excel_section_name]], code == i)$label)
  df
}

# Example
small_sample <- head(data, n = 200)
sample_with_label <- convert_codes(small_sample, "Police_Force", "Police Force")