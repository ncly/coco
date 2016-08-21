require(zoo)
# adjusts the credit derivative approach for semi-annual coupon payments
source('CreditDerivativeApproach_2.R')
# adjusts the equity derivative approach for semi-annual coupon payments
source('EquityDerivativeApproach_2.R')
# adjusts the structural approach for semi-annual coupon payments
source('StructuralApproach_2.R')

calculate_empiricalAnalysis <- function(){
  
  # coco data
  cocopriceUSD <- read.csv2("data/final_CoCoPrices_USD.csv", header = TRUE, sep=";", dec=",", as.is=TRUE)
  cocopriceUSD[[1]] <- as.Date(cocopriceUSD[[1]])
  cocopriceUSD <- cocopriceUSD[rowSums(is.na(cocopriceUSD)) == 0,]
  cocopriceUSD <- cocopriceUSD[cocopriceUSD$Date>="2015-03-30" & cocopriceUSD$Date<="2016-06-30", ]
  cocopriceGBP <- cocopriceUSD
  
  # exchange rate data
  fxrate <- read.csv2("data/final_fxrateUSDGBP.csv", header = TRUE, sep=";", dec=",", as.is=TRUE)
  fxrate[[1]] <- as.Date(fxrate[[1]])
  fxrate <- fxrate[fxrate$date>="2015-03-30" & fxrate$date<="2016-06-30", ]
  
  # coco data GBP
  for(i in 1:nrow(cocopriceUSD)){
    for(j in 1:nrow(fxrate)){
      if(fxrate$date[j] == cocopriceUSD$Date[i]){
        cocopriceGBP$High[i] <- cocopriceUSD$High[i]/fxrate$fxrate[j]
      }
    }
  }
  
  # share price data
  sharepriceGBP <- read.csv2("data/final_shareprice_HSBC.csv", header = TRUE, sep=";", dec=".", as.is=TRUE)
  sharepriceGBP[[1]] <- as.Date(sharepriceGBP[[1]])
  sharepriceGBP <- sharepriceGBP[sharepriceGBP$date>="2015-03-30" & sharepriceGBP$date<="2016-06-30", ]
  sharepriceGBP <- na.locf(sharepriceGBP)
  
  # share price data
  numberShares <- read.csv2("data/numberShares_HSBC.csv", header = TRUE, sep=";", dec=".", as.is=TRUE)
  numberShares[[1]] <- as.Date(numberShares[[1]])
  numberShares <- numberShares[numberShares$date>="2015-03-30" & numberShares$date<="2016-06-30", ]
  numberShares <- na.locf(numberShares)
  
  marketCap <- matrix(nrow = nrow(sharepriceGBP), ncol = 2)
  
  # market capitalization
  for(i in 1:nrow(sharepriceGBP)){
    marketCap$date[i, 1] <- sharepriceGBP[i, 1]
    marketCap$value[i, 2] <- 2
  }
  
  
  # asset and deposit data
  sharepriceGBP <- read.csv2("data/final_shareprice_HSBC.csv", header = TRUE, sep=";", dec=".", as.is=TRUE)
  sharepriceGBP[[1]] <- as.Date(sharepriceGBP[[1]])
  sharepriceGBP <- sharepriceGBP[sharepriceGBP$date>="2015-03-30" & sharepriceGBP$date<="2016-06-30", ]
  sharepriceGBP <- na.locf(sharepriceGBP)
  
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
  colnames(basic_data) <- c("date","cocoprice","shareprice")
  
  #sigma_E
  sharepriceGBP <- read.csv2("data/final_shareprice_HSBC.csv", header = TRUE, sep=";", dec=".", as.is=TRUE)
  sharepriceGBP[[1]] <- as.Date(sharepriceGBP[[1]])
  sharepriceGBP <- shareprice[sharepriceGBP$date>="2014-03-29" & sharepriceGBP$date<="2016-03-29", ]
  sharepriceGBP <- na.locf(sharepriceGBP)
  returns <- matrix(nrow=nrow(sharepriceGBP)-1, ncol = 1)
  for(z in 1:(nrow(sharepriceGBP)-1)){
    returns[z,1] <- as.numeric(sharepriceGBP$shareprice[z]) / as.numeric(sharepriceGBP$shareprice[z + 1]) - 1
  }
  
  sigma_E <- sd(returns)
  
  # Fitting Equity Derivative Approach
  simulation_coco_price_ed <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(simulation_coco_price_ed) <- c("date","sim1")
  
  diff_simulation_coco_price_ed <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(diff_simulation_coco_price_ed) <- c("abssimdiff", "squaredsimdiff")
  
  simRMSE_checker <- 0
  simMAE_checker <- 0
  fitted_S_star <- 0
  plotting_data <- matrix(nrow = nrow(basic_data), ncol=5)
  colnames(plotting_data) <- c("date", "coco","ed", "cd", "sa")
  plotting_data[,1] <- basic_data[,1]
  plotting_data[,2] <- cocopriceGBP$High
  
  for(simS_star in seq(0.1, 400, 0.01)){
    for(counter in 1:nrow(basic_data)){
      simulation_coco_price_ed[counter, 1] <- basic_data[counter,1]
      simulation_coco_price_ed[counter, 2] <- 1/1.45 * price_coco_ed(t <- 0, T <- 100, S_t <- as.numeric(basic_data$shareprice[counter]), S_star <- simS_star, C_p <- 403.488, c_i <- (6.375/2), r <- 0.022, N <- 100, q <- 0.0443, sigma <- sigma_E, alpha = 1)
      
      diff_simulation_coco_price_ed[counter, 1] <- abs(as.numeric(cocopriceGBP[counter,2]) - as.numeric(simulation_coco_price_ed[counter, 2]))
      diff_simulation_coco_price_ed[counter, 2] <- ((as.numeric(cocopriceGBP[counter,2]) - as.numeric(simulation_coco_price_ed[counter, 2])))^2
    }
    MAE <- sum(as.numeric(diff_simulation_coco_price_ed[,1]))/nrow(diff_simulation_coco_price_ed)
    RMSE <- sqrt(sum(as.numeric(diff_simulation_coco_price_ed[,1])))/nrow(diff_simulation_coco_price_ed)
    if(simRMSE_checker > RMSE ||  simRMSE_checker == 0){
      simRMSE_checker <- RMSE
      simMAE_checker <- MAE
      fitted_S_star <- simS_star
      plotting_data[,3] <- simulation_coco_price_ed[,2]
    }
  }
  simRMSE_checker_ed <- simRMSE_checker
  simMAE_checker_ed <- simMAE_checker
  fitted_S_star_ed <- fitted_S_star
  
  # Fitting Credit Derivative Approach
  simulation_coco_price_cd <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(simulation_coco_price_cd) <- c("date","sim1")
  
  diff_simulation_coco_price_cd <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(diff_simulation_coco_price_cd) <- c("abssimdiff", "squaredsimdiff")
  
  simRMSE_checker <- 0
  simMAE_checker <-0
  fitted_S_star <- 0
  
  for(simS_star in seq(0.1, 400, 0.01)){
    for(counter in 1:nrow(basic_data)){
      simulation_coco_price_cd[counter, 1] <- basic_data[counter,1]
      simulation_coco_price_cd[counter, 2] <- 1/1.45 * price_coco_cd(t <- 0, T <- 32, S_t <- as.numeric(basic_data$shareprice[counter]), S_star <- simS_star, C_p <- 403.488, c_i <- (6.375/2), r <- 0.022, N <- 100, q <- 0.0443, sigma <- sigma_E)
      
      diff_simulation_coco_price_cd[counter, 1] <- abs(as.numeric(cocopriceGBP[counter,2]) - as.numeric(simulation_coco_price_cd[counter, 2]))
      diff_simulation_coco_price_cd[counter, 2] <- ((as.numeric(cocopriceGBP[counter,2]) - as.numeric(simulation_coco_price_cd[counter, 2])))^2
    }
    MAE <- sum(as.numeric(diff_simulation_coco_price_cd[,1]))/nrow(diff_simulation_coco_price_cd)
    RMSE <- sqrt(sum(as.numeric(diff_simulation_coco_price_cd[,1])))/nrow(diff_simulation_coco_price_cd)
    if(simRMSE_checker > RMSE ||  simRMSE_checker == 0){
      simRMSE_checker <- RMSE
      simMAE_checker <- MAE
      fitted_S_star <- simS_star
      plotting_data[,4] <- simulation_coco_price_cd[,2]
    }
  }
  simRMSE_checker_cd <- simRMSE_checker
  simMAE_checker_cd <- simMAE_checker
  fitted_S_star_cd <- fitted_S_star
  
  
  # Fitting Structural Approach
  simulation_coco_price_sa <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(simulation_coco_price_sa) <- c("date","sim1")
  
  diff_simulation_coco_price_sa <- matrix(nrow = nrow(basic_data), ncol = 2)
  colnames(diff_simulation_coco_price_sa) <- c("abssimdiff", "squaredsimdiff")
  
  simRMSE_checker <- 0
  simMAE_checker <- 0
  fitted_S_star <- 0
  plotting_data <- matrix(nrow = nrow(basic_data), ncol=5)
  colnames(plotting_data) <- c("date", "coco","ed", "cd", "sa")
  plotting_data[,1] <- basic_data[,1]
  plotting_data[,2] <- cocopriceGBP$High
  
  for(simS_star in seq(0.1, 400, 100)){
    for(counter in 1:nrow(basic_data)){
      simulation_coco_price_sa[counter, 1] <- basic_data[counter,1]
      simulation_coco_price_sa[counter, 2] <- 1/1.48 * price_coco_sa(T <- 100, nsimulations <- 1000, rho <- 0.5, kappa <- 0.04, r_bar <- 0.06, r0 <- 0.022, sigma_r <- 0.05, mu_Y <- 0.00, sigma_Y <- 0.02, lambda <- 2, g <- 0.5, x_hat <- 1.1494, b0 <- 0.0341, p <- simS_star/403.488, e_bar <- 0.0681, sigma_A <- 0.0367, x0 <- (880+0.2214022*as.numeric(basic_data$shareprice[counter]))/880, B <- 1, coupon <- (0.06375/2))
      diff_simulation_coco_price_sa[counter, 1] <- abs(as.numeric(cocopriceGBP[counter,2]) - as.numeric(simulation_coco_price_sa[counter, 2]))
      diff_simulation_coco_price_sa[counter, 2] <- ((as.numeric(cocopriceGBP[counter,2]) - as.numeric(simulation_coco_price_sa[counter, 2])))^2
    }
    MAE <- sum(as.numeric(diff_simulation_coco_price_sa[,1]))/nrow(diff_simulation_coco_price_sa)
    RMSE <- sqrt(sum(as.numeric(diff_simulation_coco_price_ed[,1])))/nrow(diff_simulation_coco_price_sa)
    if(simRMSE_checker > RMSE ||  simRMSE_checker == 0){
      simRMSE_checker <- RMSE
      simMAE_checker <- MAE
      fitted_S_star <- simS_star
      plotting_data[,5] <- simulation_coco_price_sa[,2]
    }
  }
  simRMSE_checker_sa <- simRMSE_checker
  simMAE_checker_sa <- simMAE_checker
  fitted_S_star_sa <- fitted_S_star
  
  
  write.csv2(plotting_data, file = "data/plottingdata_3.txt")
  
  #relative price development plot data
  #alldata$High <- as.numeric(alldata$High)
  #alldata$alldata <- as.numeric(alldata$alldata)
  #alldata <- alldata[alldata$Date>="2015-03-30" & alldata$Date<="2016-06-30", ]
  #alldata$High <- alldata$High / 67.47136
  #alldata$alldata <- alldata$alldata / 537.105
  #write.csv2(alldata, file="data/final_relativepricedevelopment_HSBC-CoCo.txt")

}