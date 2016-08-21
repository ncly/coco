library(lubridate) 

data <- read.csv2("CoCo_Data.txt", header = TRUE, sep=" ", dec=".", as.is=TRUE)
data[[1]] <- as.Date(data[[1]])
sd(data$cocoprice)


data <- read.csv2("CoCo_Data.txt", header = TRUE, sep=" ", dec=".", as.is=TRUE)
data$cocodate <- as.Date(data$cocodate)
data$cocoprice <- data$cocoprice/data$cocoprice[1]
data$shareprice <- data$shareprice/data$shareprice[1]
write.table(data, file = "CoCo_data_2.txt")



data <- read.csv2("spot_interest rate.csv", header = TRUE, sep=";", dec=",", as.is=TRUE)
data[[1]] <- as.Date(data[[1]])
sd(data$cocoprice)