require(zoo)

# For the case study the approaches are adjusted to account for semi-annual coupon payments
source('CreditDerivativeApproach.R')
source('EquityDerivativeApproach.R')
source('StructuralApproach.R')

source('calibrateStructuralApproach')

calculate_empiricalAnalysis <- function(){
  
  # Data load
  # Coco data
  cocopriceUSD <- read.csv2("data/final_CoCoPrices_USD.csv", header = TRUE, sep=";", dec=",", as.is = TRUE)
  cocopriceUSD[[1]] <- as.Date(cocopriceUSD[[1]])
  cocopriceUSD <- cocopriceUSD[rowSums(is.na(cocopriceUSD)) == 0, ]
  cocopriceUSD <- cocopriceUSD[cocopriceUSD$Date >= "2015-03-30" & cocopriceUSD$Date <= "2016-06-30", ]
  cocopriceGBP <- cocopriceUSD
  
  # Exchange rate data
  fxrate <- read.csv2("data/final_fxrateUSDGBP.csv", header = TRUE, sep = ";", dec = ",", as.is = TRUE)
  fxrate[[1]] <- as.Date(fxrate[[1]])
  fxrate <- fxrate[fxrate$date >= "2015-03-30" & fxrate$date <= "2016-06-30", ]
  
  # Coco data in GBP
  for(i in 1:nrow(cocopriceUSD)){
    for(j in 1:nrow(fxrate)){
      if(fxrate$date[j] == cocopriceUSD$Date[i]){
        cocopriceGBP$High[i] <- cocopriceUSD$High[i] / fxrate$fxrate[j]
      }
    }
  }
  
  # Share price data
  sharepriceGBP <- read.csv2("data/final_shareprice_HSBC.csv", header = TRUE, sep = ";", dec = ".", as.is = TRUE)
  sharepriceGBP[[1]] <- as.Date(sharepriceGBP[[1]])
  sharepriceGBP_historic <- sharepriceGBP[sharepriceGBP$date <= "2015-03-29" & sharepriceGBP$date <= "2011-03-29", ]
  sharepriceGBP <- sharepriceGBP[sharepriceGBP$date >= "2015-03-30" & sharepriceGBP$date <= "2016-06-30", ]
  sharepriceGBP <- na.locf(sharepriceGBP)
  
  returns <- matrix(nrow = nrow(sharepriceGBP) - 1)
  
  # share price return data
  for(i in 1:nrow(sharepriceGBP)){
    returns[1, i] <- sharepriceGBP[2, i + 1] / sharepriceGBP[2, i] - 1
  }
  
  # number of outstanding share data
  numberShares <- read.csv2("data/numberShares_HSBC.csv", header = TRUE, sep = ";", dec = ".", as.is = TRUE)
  numberShares[[1]] <- as.Date(numberShares[[1]])
  numberShares <- numberShares[numberShares$date >= "2015-03-30" & numberShares$date <= "2016-06-30", ]
  numberShares <- na.locf(numberShares)
  
  marketCap <- matrix(nrow = nrow(sharepriceGBP), ncol = 2)
  
  # market capitalization
  for(i in 1:nrow(sharepriceGBP)){
    if(sharepriceGBP$date[j] == numberShares$date[i]){
    marketCap$date[i, 1] <- sharepriceGBP[i, 1]
    marketCap$value[i, 2] <- sharepriceGBP[i, 2] * numberShares[i, 2]
    }
  }
  
  # asset and deposit data, interpolation already done with excel
  accountingdata <- read.csv2("data/accounting_data_HSBC.csv", header = TRUE, sep = ";", dec = ".", as.is = TRUE)
  accountingdata[[1]] <- as.Date(accountingdata[[1]])
  accountingdata <- accountingdata[accountingdata$date >= "2015-03-30" & accountingdata$date <= "2016-06-30", ]
  accountingdata <- na.locf(accountingdata)
  
  # yield curve data
  interestratedata <- read.csv2("data/yield_curve.csv", header = TRUE, sep = ";", dec = ".", as.is = TRUE)
  interestratedata[[1]] <- as.Date(interestratedata[[1]])
  interestratedata_historic <- interestratedata[interestratedata$date <= "2015-03-29" & interestratedata$date <= "2011-03-29", ]
  interestratedata <- interestratedata[interestratedata$date >= "2015-03-30" & interestratedata$date <= "2016-06-30", ]
  interestratedata <- na.locf(interestratedata)
  
  #alldata
  alldata <- matrix(nrow = nrow(cocopriceGBP), ncol = 1)
  for(i in 1:nrow(cocopriceGBP)){
    for(j in 1:nrow(sharepriceGBP)){
      if(sharepriceGBP$date[j] == cocopriceGBP$Date[i]){
        alldata[i,1] <- sharepriceGBP$shareprice[j]
      }
    }
  }
  
  #data collection
  basic_data <- na.locf(cbind(cocopriceGBP, alldata))
  colnames(basic_data) <- c("date", "cocoprice", "shareprice")
  
  #sigma_E
  sigma_E <- sd(sharepriceGBP_historic$price)
  
  # Fitting Equity Derivative Approach
  simulation_coco_price_ed <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(simulation_coco_price_ed) <- c("date", "sim1")
  
  diff_simulation_coco_price_ed <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(diff_simulation_coco_price_ed) <- c("abssimdiff", "squaredsimdiff")
  
  simRMSE_checker <- 0
  simMAE_checker <- 0
  fitted_S_star <- 0
  plotting_data <- matrix(nrow = nrow(basic_data), ncol=5)
  colnames(plotting_data) <- c("date", "coco","ed", "cd", "sa")
  plotting_data[, 1] <- basic_data[, 1]
  plotting_data[, 2] <- cocopriceGBP$High
  
  for(simS_star in seq(0.1, 400, 0.01)){
    for(counter in 1:nrow(basic_data)){
      simulation_coco_price_ed[counter, 1] <- basic_data[counter, 1]
      simulation_coco_price_ed[counter, 2] <- price_coco_ed(t <- 0, T <- 100, S_t <- as.numeric(basic_data$shareprice[counter]), S_star <- simS_star, C_p <- 403.488, c_i <- (6.375 / 2), r <- 0.022, N <- 100, q <- 0.0443, sigma <- sigma_E, alpha <- 1)
      simulation_coco_price_ed[counter, 2] <- simulation_coco_price_ed[counter, 2]/fxrate$fxrate[counter]
    
      diff_simulation_coco_price_ed[counter, 1] <- abs(as.numeric(cocopriceGBP[counter, 2]) - as.numeric(simulation_coco_price_ed[counter, 2]))
      diff_simulation_coco_price_ed[counter, 2] <- ((as.numeric(cocopriceGBP[counter, 2]) - as.numeric(simulation_coco_price_ed[counter, 2])))^2
    }
    MAE <- sum(as.numeric(diff_simulation_coco_price_ed[, 1]))/nrow(diff_simulation_coco_price_ed)
    RMSE <- sqrt(sum(as.numeric(diff_simulation_coco_price_ed[, 1])))/nrow(diff_simulation_coco_price_ed)
    if(simRMSE_checker > RMSE ||  simRMSE_checker == 0){
      simRMSE_checker <- RMSE
      simMAE_checker <- MAE
      fitted_S_star <- simS_star
      plotting_data[, 3] <- simulation_coco_price_ed[, 2]
    }
  }
  simRMSE_checker_ed <- simRMSE_checker
  simMAE_checker_ed <- simMAE_checker
  fitted_S_star_ed <- fitted_S_star
  
  # Fitting Credit Derivative Approach
  simulation_coco_price_cd <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(simulation_coco_price_cd) <- c("date", "sim1")
  
  diff_simulation_coco_price_cd <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(diff_simulation_coco_price_cd) <- c("abssimdiff", "squaredsimdiff")
  
  simRMSE_checker <- 0
  simMAE_checker <- 0
  fitted_S_star <- 0
  
  for(simS_star in seq(0.1, 400, 0.01)){
    for(counter in 1:nrow(basic_data)){
      simulation_coco_price_cd[counter, 1] <- basic_data[counter, 1]
      simulation_coco_price_cd[counter, 2] <- price_coco_cd(t <- 0, T <- 32, S_t <- as.numeric(basic_data$shareprice[counter]), S_star <- simS_star, C_p <- 403.488, c_i <- (6.375 / 2), r <- 0.022, N <- 100, q <- 0.0443, sigma <- sigma_E)
      simulation_coco_price_cd[counter, 2] <- simulation_coco_price_cd[counter, 2]/fxrate$fxrate[counter]
      
      
      diff_simulation_coco_price_cd[counter, 1] <- abs(as.numeric(cocopriceGBP[counter, 2]) - as.numeric(simulation_coco_price_cd[counter, 2]))
      diff_simulation_coco_price_cd[counter, 2] <- ((as.numeric(cocopriceGBP[counter, 2]) - as.numeric(simulation_coco_price_cd[counter, 2])))^2
    }
    MAE <- sum(as.numeric(diff_simulation_coco_price_cd[, 1])) / nrow(diff_simulation_coco_price_cd)
    RMSE <- sqrt(sum(as.numeric(diff_simulation_coco_price_cd[, 1]))) / nrow(diff_simulation_coco_price_cd)
    if(simRMSE_checker > RMSE ||  simRMSE_checker == 0){
      simRMSE_checker <- RMSE
      simMAE_checker <- MAE
      fitted_S_star <- simS_star
      plotting_data[, 4] <- simulation_coco_price_cd[, 2]
    }
  }
  simRMSE_checker_cd <- simRMSE_checker
  simMAE_checker_cd <- simMAE_checker
  fitted_S_star_cd <- fitted_S_star
  
  # Parametrization Structural Approach
  D_0 <- as.numeric(accountingdata[accountingdata$date = "2015-03-30"])
  
  # merton data
  merton_paramters <- calibrate_Merton(D_0, marketCap[1, 2], interestratedata[1, 2], sigma_E)
  asset_volatility <- merton_paramters$asset_volatility
  
  # jump data
  jump_parameters <- estimate_Jump_parameters(returns, 2)
  mean_jump <- jump_parameters$mean_jump
  sd_jump <- jump_parameters$sd_jump
  
  # CIR data
  # Input data: [R,tau] (n x 2), with R: annual bonds yields in percentage and tau: maturities in years
  tau <- cbind(rep(1, nrow(interestratedata)), rep(2, nrow(interestratedata)), rep(5, nrow(interestratedata)), rep(10, nrow(interestratedata)))
  tau <- as.vector(tau)
  
  interestratedata <- cbind(interestratedata$X1.0, interestratedata$X2.0, interestratedata$X5.0, interestratedata$X10.0)
  interestratedata <- as.vector(interestratedata)
  
  cir.data <- cbind(interestratedata, tau)
  cir.data <- calibrate_CIR(cir.data)
  
  kappa <- cir.data$kappa
  r_bar <- cir.data$r_bar
  sigma_r <- cir.data$sigma_r
  
  rho <- cor(interestratedata_historic[2, ], sharepriceGBP_historic[2, ])
  
  # Fitting Structural Approach
  simulation_coco_price_sa <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(simulation_coco_price_sa) <- c("date", "sim1")
  
  diff_simulation_coco_price_sa <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(diff_simulation_coco_price_sa) <- c("abssimdiff", "squaredsimdiff")
  
  simRMSE_checker <- 0
  simMAE_checker <- 0
  fitted_S_star <- 0
  plotting_data <- matrix(nrow = nrow(basic_data), ncol = 5)
  colnames(plotting_data) <- c("date", "coco","ed", "cd", "sa")
  plotting_data[, 1] <- basic_data[, 1]
  plotting_data[, 2] <- cocopriceGBP$High
  
  for(simS_star in seq(0.1, 400, 100)){
    for(counter in 1:nrow(basic_data)){
      simulation_coco_price_sa[counter, 1] <- basic_data[counter, 1]
      simulation_coco_price_sa[counter, 2] <- price_coco_sa(T <- 100, nsimulations <- 1000, rho <- rho, kappa <- kappa, r_bar <- r_bar, r0 <- interestratedata[1, counter], sigma_r <- sigma_r, mu_Y <- mean_jump, sigma_Y <- sd_jump, lambda <- 2, g <- 0.5, x_hat <- (numberShares[1, 2] * sharepriceGBP[1, 2]) / D_0, b0 <- 2475000000 / D_0, p <- simS_star / 403.488, e_bar <- (numberShares[1, 2] * simS_star + D_0) / D_0, sigma_A <- asset_volatility, x0 <- (as.numeric(accountingdata$deposits[counter]) + as.numeric(numberShares[2, counter]) * as.numeric(basic_data$shareprice[counter])) / as.numeric(accountingdata$deposits[counter]), B <- 1, coupon <- (0.06375 / 2))
      simulation_coco_price_sa[counter, 2] <- simulation_coco_price_sa[counter, 2] / fxrate$fxrate[counter]
      
      diff_simulation_coco_price_sa[counter, 1] <- abs(as.numeric(cocopriceGBP[counter, 2]) - as.numeric(simulation_coco_price_sa[counter, 2]))
      diff_simulation_coco_price_sa[counter, 2] <- ((as.numeric(cocopriceGBP[counter, 2]) - as.numeric(simulation_coco_price_sa[counter, 2])))^2
    }
    MAE <- sum(as.numeric(diff_simulation_coco_price_sa[, 1])) / nrow(diff_simulation_coco_price_sa)
    RMSE <- sqrt(sum(as.numeric(diff_simulation_coco_price_ed[, 1]))) / nrow(diff_simulation_coco_price_sa)
    if(simRMSE_checker > RMSE ||  simRMSE_checker == 0){
      simRMSE_checker <- RMSE
      simMAE_checker <- MAE
      fitted_S_star <- simS_star
      plotting_data[, 5] <- simulation_coco_price_sa[, 2]
    }
  }
  simRMSE_checker_sa <- simRMSE_checker
  simMAE_checker_sa <- simMAE_checker
  fitted_S_star_sa <- fitted_S_star
  
  
  write.csv2(plotting_data, file = "data/plottingdata_final.txt")
}