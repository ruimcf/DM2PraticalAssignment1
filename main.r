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

useless <- c("Accident_Index", "Location_Easting_OSGR", "Location_Northing_OSGR", "LSOA_of_Accident_Location")
data.discretized <- data[, !names(data) %in% useless]
data.discretized <- convert_date_and_time(data.discretized)
data.discretized[data.discretized==-1] <- NA
# Longitude 
data.discretized$Longitude<- discretize(data.discretized$Longitude, method = "interval", categories = 10)
# Latitude
data.discretized$Latitude<- discretize(data.discretized$Latitude, method = "interval", categories = 10)
# Police Force
data.discretized <- convert_codes(data.discretized, "Police_Force", "Police Force")
data.discretized$Police_Force <- factor(data.discretized$Police_Force)
# Accident Severity
data.discretized <- convert_codes(data.discretized, "Accident_Severity", "Accident Severity")
data.discretized$Accident_Severity <- factor(data.discretized$Accident_Severity)
# Number of Vehicles
n_vehicles.range <- c(1,2,3,4,5,Inf)
data.discretized$Number_of_Vehicles <- discretize(data.discretized$Number_of_Vehicles, method = "fixed", categories = n_vehicles.range)
# Number of Casualties
n_casualties.range <- c(1,2,3,4,5,Inf)
data.discretized$Number_of_Casualties <- discretize(data.discretized$Number_of_Casualties, method = "fixed", categories = n_casualties.range)
# Day of Week
data.discretized <- convert_codes(data.discretized, "Day_of_Week", "Day of Week")
data.discretized$Day_of_Week <- factor(data.discretized$Day_of_Week)
# Local Authority District
data.discretized <- convert_codes(data.discretized, "Local_Authority_.District.", "Local Authority (District)")
data.discretized$Local_Authority_.District. <- factor(data.discretized$Local_Authority_.District.)
# Local Authority Highway
data.discretized <- convert_codes(data.discretized, "Local_Authority_.Highway.", "Local Authority (Highway)")
data.discretized$Local_Authority_.Highway. <- factor(data.discretized$Local_Authority_.Highway.)
# 1st Road Class
data.discretized <- convert_codes(data.discretized, "X1st_Road_Class", "1st Road Class")
data.discretized$X1st_Road_Class <- factor(data.discretized$X1st_Road_Class)
# 1st Road Number
data.discretized$X1st_Road_Number <- factor(data.discretized$X1st_Road_Number)
# Road Type
data.discretized <- convert_codes(data.discretized, "Road_Type", "Road Type")
data.discretized$Road_Type <- factor(data.discretized$Road_Type)
# Speed Limit (????)
data.discretized$Speed_limit <- factor(data.discretized$Speed_limit)
# Junction Detail
data.discretized <- convert_codes(data.discretized, "Junction_Detail", "Junction Detail")
data.discretized$Junction_Detail <- factor(data.discretized$Junction_Detail, exclude = NULL)
# Junction Control
data.discretized <- convert_codes(data.discretized, "Junction_Control", "Junction Control")
data.discretized$Junction_Control <- factor(data.discretized$Junction_Control, exclude = NULL)
# 2nd Road Class
data.discretized <- convert_codes(data.discretized, "X2nd_Road_Class", "2nd Road Class")
data.discretized$X2nd_Road_Class <- factor(data.discretized$X2nd_Road_Class, exclude = NULL)
# 2nd Road Number
data.discretized$X2nd_Road_Number <- factor(data.discretized$X2nd_Road_Number)
# Ped Cross - Human
data.discretized <- convert_codes(data.discretized, "Pedestrian_Crossing.Human_Control", "Ped Cross - Human")
data.discretized$Pedestrian_Crossing.Human_Control <- factor(data.discretized$Pedestrian_Crossing.Human_Control, exclude = NULL)
# Ped Cross - Physical
data.discretized <- convert_codes(data.discretized, "Pedestrian_Crossing.Physical_Facilities", "Ped Cross - Physical")
data.discretized$Pedestrian_Crossing.Physical_Facilities <- factor(data.discretized$Pedestrian_Crossing.Physical_Facilities)
# Light Conditions
data.discretized <- convert_codes(data.discretized, "Light_Conditions", "Light Conditions")
data.discretized$Light_Conditions <- factor(data.discretized$Light_Conditions)
# Weather Conditions
data.discretized <- convert_codes(data.discretized, "Weather_Conditions", "Weather")
data.discretized$Weather_Conditions <- factor(data.discretized$Weather_Conditions)
# Road Surface
data.discretized <- convert_codes(data.discretized, "Road_Surface_Conditions", "Road Surface")
data.discretized$Road_Surface_Conditions <- factor(data.discretized$Road_Surface_Conditions)
# Special Conditions
data.discretized <- convert_codes(data.discretized, "Special_Conditions_at_Site", "Special Conditions at Site")
data.discretized$Special_Conditions_at_Site <- factor(data.discretized$Special_Conditions_at_Site)
# Carriageway Hazards
data.discretized <- convert_codes(data.discretized, "Carriageway_Hazards", "Carriageway Hazards")
data.discretized$Carriageway_Hazards <- factor(data.discretized$Carriageway_Hazards)
# Urban Rural
data.discretized <- convert_codes(data.discretized, "Urban_or_Rural_Area", "Urban Rural")
data.discretized$Urban_or_Rural_Area <- factor(data.discretized$Urban_or_Rural_Area)
# Police Officer Attend
data.discretized <- convert_codes(data.discretized, "Did_Police_Officer_Attend_Scene_of_Accident", "Police Officer Attend")
data.discretized$Did_Police_Officer_Attend_Scene_of_Accident <- factor(data.discretized$Did_Police_Officer_Attend_Scene_of_Accident)
# Day
data.discretized$Day <- discretize(data.discretized$Day, method="interval", categories=4)
# Month
data.discretized$Month <- discretize(data.discretized$Month, method="interval", categories=4)
# Hour
data.discretized$Hour <- discretize(data.discretized$Hour, method="interval", categories=4)
# Minutes
data.discretized$Minutes <- discretize(data.discretized$Minutes, method="interval", categories=4)
