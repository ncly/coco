require(zoo)
require(quantmod)
source('CreditDerivativeApproach.R')
source('EquityDerivativeApproach.R')


calculate_empiricalAnalysis <- function(){
  
  # coco data
  cocopriceUSD <- read.csv2("data/final_CoCoPrices_USD.csv", header = TRUE, sep=";", dec=",", as.is=TRUE)
  cocopriceUSD[[1]] <- as.Date(cocopriceUSD[[1]])
  cocopriceUSD <- cocopriceUSD[rowSums(is.na(cocopriceUSD)) == 0,]
  cocopriceUSD <- cocopriceUSD[cocopriceUSD$Date>="2015-03-30" & cocopriceUSD$Date<="2016-06-30", ]
  cocopriceGBP <- cocopriceUSD
  
  #exchange rate data
  fxrate <- read.csv2("data/final_fxrateUSDGBP.csv", header = TRUE, sep=";", dec=",", as.is=TRUE)
  fxrate[[1]] <- as.Date(fxrate[[1]])
  fxrate <- fxrate[fxrate$date>="2015-03-30" & fxrate$date<="2016-06-30", ]
  
  #coco data GBP
  for(i in 1:nrow(cocopriceUSD)){
    for(j in 1:nrow(fxrate)){
      if(fxrate$date[j] == cocopriceUSD$Date[i]){
        cocopriceGBP$High[i] <- cocopriceUSD$High[i]/fxrate$fxrate[j]
      }
    }
  }
  
  #share price data
  sharepriceGBP <- read.csv2("data/final_shareprice_HSBC.csv", header = TRUE, sep=";", dec=".", as.is=TRUE)
  sharepriceGBP[[1]] <- as.Date(sharepriceGBP[[1]])
  sharepriceGBP <- shareprice[sharepriceGBP$date>="2015-03-30" & sharepriceGBP$date<="2016-06-30", ]
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
  Delt(sharepriceGBP)
  sd(sharepriceGBP$shareprice)
  
  for(counter in 1:basic_data){
    price_coco_cd(t <- 0, T <- 9999, S_t <- basic_data$shareprice, S_star <- 60, C_p <- 75, c_i <- 6.00, r <- 0.03, N <- 100, q <- 0.00, sigma <- 0.3)
  }
  
  
  #price_coco_cd(t <- 0, T <- 10, S_t <- 120, S_star <- 60, C_p <- 75, c_i <- 6.00, r <- 0.03, N <- 100, q <- 0.00, sigma <- 0.3)
  
  #relative price development plot data
  #alldata$High <- as.numeric(alldata$High)
  #alldata$alldata <- as.numeric(alldata$alldata)
  #alldata <- alldata[alldata$Date>="2015-03-30" & alldata$Date<="2016-06-30", ]
  #alldata$High <- alldata$High / 67.47136
  #alldata$alldata <- alldata$alldata / 537.105
  #write.csv2(alldata, file="data/final_relativepricedevelopment_HSBC-CoCo.txt")

}