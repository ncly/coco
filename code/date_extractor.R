library(lubridate) 

data <- read.csv2("CoCo_Data.txt", header = TRUE, sep=" ", dec=".", as.is=TRUE)
data[[1]] <- as.Date(data[[1]])
sd(data$cocoprice)


data <- read.csv2("CoCo_Data.txt", header = TRUE, sep=" ", dec=".", as.is=TRUE)
data$cocodate <- as.Date(data$cocodate)
data$cocoprice <- data$cocoprice/data$cocoprice[1]
data$shareprice <- data$shareprice/data$shareprice[1]
write.table(data, file = "CoCo_data_2.txt")


# CIR data
data <- read.csv2("spot_interest rate.csv", header = TRUE, sep=";", dec=",", as.is=TRUE)
data[[1]] <- as.Date(data[[1]])
data <- data[rowSums(is.na(data)) == 0,]
data <- data[data$date>="2013-08-01" & data$date<="2013-11-01", ]

tau <- cbind(rep(1, nrow(data)), rep(2, nrow(data)), rep(5, nrow(data)), rep(10, nrow(data)))
tau <- as.vector(tau)

data <- cbind(data$X1.0, data$X2.0, data$X5.0, data$X10.0)
data <- as.vector(data)

data <- cbind(data, tau)

#t(t(data[,2:ncol(data)]) / 100)
