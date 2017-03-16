library(readxl)
library(lubridate)

read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  sheets <- sheets[3:length(sheets)]
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
sample_with_label <- convert_codes(small_sample, "Accident_Severity", "Accident Severity")
sample_with_label <- convert_codes(sample_with_label, "Police_Force", "Police Force")

convert_date_and_time <- function(df) {
  df$Date <- as.Date(df$Date, "%d/%m/%Y")
  df$Day <- day(df$Date)               
  df$Month <- month(df$Date)
  df$Date <- NULL
  
  df$Time <- hm(df$Time)
  df$Hour <- hour(df$Time)
  df$Minute <- minute(df$Time)
  df$Time <- NULL
  df
}

useless <- c("Accident_Index", "Location_Easting_OSGR", "Location_Northing_OSGR")
data.discretized <- data[, !names(data) %in% useless]
data.discretized <- convert_date_and_time(data.discretized)
data.discretized$Police_Force <- factor(data.discretized$Police_Force)
n_vehicles.range <- c(1,2,3,4,5,Inf)
data.discretized$Number_of_Vehicles <- discretize(data.discretized$Number_of_Vehicles, method = "fixed", categories = n_vehicles.range)
