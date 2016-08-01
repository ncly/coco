require(nleqslv)

estimate_Merton_parameters <- function(deposits, marketcap, r, volatility_equity){
  # Estimation of asset value and asset volatility pursuant to Merton 1974 model
  data <- new.env()

  for(i in 1:nrow(marketcap)){
    fnewton <- function(x){
      values <- numeric(2)
      d1 <- (log(x[1]/deposits[i])+(r[i]+x[2]^2/2))/x[2]
      d2 <- d1-x[2]
      
      values[1] <- marketcap[i]- (x[1]*pnorm(d1) - exp(-r[i])*deposits[i]*pnorm(d2))
      values[2]<-volatility_equity[i]*marketcap[i]-pnorm(d1)*x[2]*x[1]
      return(values)
    }
    xstart <- c(marketcap[i]+deposits[i],volatility_equity[i])  
    data$asset_new_value[i]<- nleqslv(xstart, fnewton, method="Newton")$x[1]
    data$asset_new_volat[i]<-nleqslv(xstart, fnewton, method="Newton")$x[2]
  }
  return(list("asset_volatility" = data$asset_new_volat, "asset_value" = data$asset_new_value))
}