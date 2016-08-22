source('estimateCIRParameter.R')
source('estimateMertonParameter.R')
source('estimateJumpParameter.R')

calibrate_CIR <- function(cir_data){
  cir_parameters <- estimate_CIR_parameters(cir.data)
  
  kappa <- cir_parameters$kappa
  r_bar <- cir_parameters$r_bar
  sigma_r <- cir_parameters$sigma_r
  
  return(cir_parameters)
}

calibrate_Merton <- function(deposits, marketcap, r, volatility_equity){
  merton_parameters <- estimate_Merton_parameters(deposits, marketcap, r, volatility_equity)
 
  asset_volatility <- merton_parameters$asset_volatility
  asset_value <- merton_parameters$asset_value
  
  return(merton_parameters)
}

calibrate_Jump <- function(returns, jump_Intesity){
  jump_parameters <- estimate_Jump_parameters(returns, jump_Intensity)
  
  mean_jump <- jump_parameters$mean_jump
  sd_jump <- jump_parameters$sd_jump
  
  return(jump_parameters)
}

# CIR data
# Input data: [R,tau] (n x 2), with R: yields in percentage and tau: maturities in years
data <- read.csv2("data/spot_interest rate.csv", header = TRUE, sep=";", dec=",", as.is=TRUE)
data[[1]] <- as.Date(data[[1]])
data <- data[rowSums(is.na(data)) == 0,]
data <- data[data$date>="2015-03-30" & data$date<="2015-03-30", ]

tau <- cbind(rep(1, nrow(data)), rep(2, nrow(data)), rep(3, nrow(data)), rep(10, nrow(data)))
tau <- as.vector(tau)

data <- cbind(data$X1.0, data$X2.0, data$X5.0, data$X10.0)
data <- as.vector(data)

cir.data <- cbind(data, tau)
cir.data <- calibrate_CIR(cir.data)

kappa <- cir.data$kappa
r_bar <- cir.data$r_bar
sigma_r <- cir.data$sigma_r


# Merton
sharepriceGBP <- read.csv2("data/final_shareprice_HSBC.csv", header = TRUE, sep=";", dec=".", as.is=TRUE)
sharepriceGBP[[1]] <- as.Date(sharepriceGBP[[1]])
sharepriceGBP <- sharepriceGBP[sharepriceGBP$date>="2014-03-29" & sharepriceGBP$date<="2016-03-29", ]
sharepriceGBP <- na.locf(sharepriceGBP)
returns <- matrix(nrow=nrow(sharepriceGBP)-1, ncol = 1)
for(z in 1:(nrow(sharepriceGBP)-1)){
  r[z,1] <- as.numeric(sharepriceGBP$shareprice[z]) / as.numeric(sharepriceGBP$shareprice[z + 1]) - 1
}


deposits <- matrix(1:1010, ncol = 1)
marketcap <- matrix(2:1011, ncol = 1)
r <- c(matrix(0.01, nrow = 253), matrix(0.02, nrow = 252), matrix(0.03, nrow = 254), matrix(0.04, nrow = 251))

volatility_equity <- matrix(0.1, nrow = 1010)

estimate_Merton_parameters(deposits, marketcap, r, volatility_equity)

# Jumps
returns <- as.timeSeries(data("bmw"))$SS.1
estimate_Jump_parameters(returns, 2)