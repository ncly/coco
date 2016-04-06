library(magrittr)

### Plain-Vanilla Black-Scholes

bs <- function(spot, strike, r_d, r_f, sigma, maturity, callput = 1) {
  forward <- spot * exp((r_d - r_f) * maturity)
  d_plus <- (log(forward / strike) + sigma ** 2 / 2 * maturity) / (sigma * sqrt(maturity))
  d_minus <- (log(forward / strike) - sigma ** 2 / 2 * maturity) / (sigma * sqrt(maturity))
  exp(-r_d * maturity) * callput * (forward * pnorm(callput * d_plus) - strike * pnorm(callput * d_minus))
}

plain_vanilla_call <- function(spot, strike, r_d, r_f, sigma, maturity) {
  bs(spot, strike, r_d, r_f, sigma, maturity, 1)
}

plain_vanilla_put <- function(spot, strike, r_d, r_f, sigma, maturity) {
  bs(spot, strike, r_d, r_f, sigma, maturity, -1)
}

### Double Knock-Out Options
# Pricing Options with curved boundaries, Naoto Kunitomo, Masayuki Ikeda (1992)
# case for delta_1=delta_2=0

dko <- function(spot, strike, upper, lower, r_d, r_f, sigma, t, callput = 1) {
  stopifnot(callput %in% c(-1,1))
  if (or(spot < lower, spot > upper)) { return(0)}
  r_prime <- (r_d - r_f + sigma ** 2 / 2) * t
  sigma_scaled <- sigma * sqrt(t)
  c1 <- 2 * (r_d - r_f) / (sigma ** 2) + 1
  c3 <- c1
  c2 <- c1 + 1
  -5:5 %>% sapply(function(n){
    if (callput == -1) {
      d1n <- (log(spot * upper ** (2 * n)  / (lower ** (2 * n + 1)))             + r_prime) / sigma_scaled
      d2n <- (log(spot * upper ** (2 * n)  / (strike * lower ** (2 * n)))        + r_prime) / sigma_scaled
      d3n <- (log(lower ** (2 * n + 2)     / (lower * spot * upper ** (2 * n)))  + r_prime) / sigma_scaled
      d4n <- (log(lower ** (2 * n + 2)     / (strike * spot * upper ** (2 * n))) + r_prime) / sigma_scaled
    }
    if (callput == 1) {
      d1n <- (log(spot * upper ** (2 * n)  / (strike * lower ** (2 * n)))         + r_prime) / sigma_scaled
      d2n <- (log(spot * upper ** (2 * n - 1) / (lower ** (2 * n)))               + r_prime) / sigma_scaled
      d3n <- (log(lower ** (2 * n + 2)     / (strike * spot * upper ** (2 * n)))  + r_prime) / sigma_scaled
      d4n <- (log(lower ** (2 * n + 2)     / (spot * upper ** (2 * n + 1)))       + r_prime) / sigma_scaled
    }
    c2n <- 2 * n / sigma ** 2
    
    sum1 <- (upper ** n / lower ** n) ** c1 * (pnorm(d1n) - pnorm(d2n)) -
      ((lower ** (n + 1)) / (upper ** n * spot)) ** c3 * (pnorm(d3n) - pnorm(d4n))
    sum2 <- (upper ** n / lower ** n) ** (c1 - 2) * (pnorm(d1n - sigma_scaled) - pnorm(d2n - sigma_scaled)) -
      ((lower ** (n + 1)) / (upper ** n * spot)) ** (c3 - 2) * (pnorm(d3n - sigma_scaled) - pnorm(d4n - sigma_scaled))
    
    c(sum1, sum2)
  }) %>% rowSums %>%  (function(sums) {
    (callput * c(spot * exp(-r_f * t), -strike * exp(-r_d * t))) %*% sums
  })
}

dko_call <- function(spot, strike, upper, lower, r_d, r_f, sigma, t) {
  dko(spot, strike, upper, lower, r_d, r_f, sigma, t, 1)
}

dko_put <- function(spot, strike, upper, lower, r_d, r_f, sigma, t) {
  dko(spot, strike, upper, lower, r_d, r_f, sigma, t, -1)
}

####

#dko(1.1, 1.10522, 1.155, 1.045, 0.5E-2, -0.2E-2, 11.08E-2, 0.5, -1)

package <- function(spot = 1.1, strike = 1.12, upper=1.21, lower=1.02, leverage=1, maturity=0.5) {
  r_d <- 0.5E-2
  r_f <- -0.2E-2
  sigma <- 11.08E-2
  result <- leverage * dko_call(spot, strike, upper, lower, r_d, r_f, sigma, maturity) + 
            leverage * dko_put(spot, strike, upper, lower, r_d, r_f, sigma, maturity) + 
              plain_vanilla_call(spot, strike, r_d, r_f, sigma, maturity) -
              plain_vanilla_put(spot, strike, r_d, r_f, sigma, maturity)
  result
}

dko_package <- function(spot=1.1, sigma=11.08E-2, strike=1.13351, upper=1.21, lower=0.99) {
  r_d <- 0.5E-2
  r_f <- -0.2E-2
  maturity <- 0.5
  result <- 100 * dko_call(spot, strike, upper, lower, r_d, r_f, sigma, maturity) +
    100 * dko_put(spot, strike, upper, lower, r_d, r_f, sigma, maturity)
  result
}

#dko_package2 <- function(spot=1.1, sigma=11.08E-2, strike=1.13351, dist=0.05) {
#  r_d <- 0.5E-2
#  r_f <- -0.2E-2
#  maturity <- 0.5
#  forward <- 1.10395
#  worst_case <- strike
#  upper <- worst_case + dist
#  lower <- worst_case - dist
#  result <- 100 * dko_call(spot, strike, upper, lower, r_d, r_f, sigma, maturity) +
#    100 * dko_put(spot, strike, upper, lower, r_d, r_f, sigma, maturity) + 
#    plain_vanilla_call(1.1, strike, r_d, r_f, sigma, maturity) -
#    plain_vanilla_put(1.1, strike, r_d, r_f, sigma, maturity)
#  result
#}

########### Zero Cost
#dko_package2(strike = 1.12, dist = 0.04531) #0  EUR
#dko_package2(strike = 1.12, dist = 0.04535) #2k EUR
###########

spot <- seq(1,1.25,0.005)
vol <- 11.02E-2
spot %>% sapply(function(x)dko_package(x,vol)) %>% plot(spot, y = ., type = "l", main = "Value")
spot %>% sapply(function(x)dko_package(x,vol)) %>% diff %>% plot(spot[-1], y = ., type = "l", main = "Delta")

vol <- seq(0.5E-2,30E-2,0.005)
vol %>% sapply(function(x)dko_package(sigma = x)) %>% plot(vol, y = ., type = "l", main = "Value")
vol %>% sapply(function(x)dko_package(sigma = x)) %>% diff %>% plot(vol[-1], y = ., type = "l", main = "Vega")


######

spot <- seq(0.9,1.35,0.005)
spot %>% sapply(function(x)package(spot = x, upper = 1.2190, lower = 1.0230)) %>% data.frame(.,spot)
  plot(spot, y = ., type = "l", main = "Value")

writeOut <- function(name, fn) {
  spot %>% sapply(fn) %>% multiply_by(1E7) %>% data.frame(spot,value=.) %>% write.table(file = name, na = "nan", row.names = F, quote = F)
}

writeOut("unlev", function(x)package(spot = x, upper = 1.2190, lower = 1.0230))
writeOut("unlev_margin", function(x)package(spot = x, upper = 1.2190, lower = 1.0243))
writeOut("lev", function(x)package(spot = x, upper = 1.1792, lower = 1.0608, leverage = 10))
writeOut("lev_margin", function(x)package(spot = x, upper = 1.1789, lower = 1.0611, leverage = 10))

## ---- OpenGL 3D 
library(rgl)
spot <- seq(0.95,1.25,0.005)
vol <- seq(0.5E-2,15E-2,0.005)
l <- list(spot, vol)
comb <- do.call(expand.grid, l)
colnames(comb) <- c("Spot", "Vol")
head(comb)
value <- mapply(function(Spot, Vol) dko_package(Spot, Vol, strike=1.12, upper=1.2190, lower=1.0230), comb$Spot, comb$Vol)

result <- cbind(comb, value)
result %>% write.table(file = "3dplot", na = "nan", row.names = F, quote = F)
#wireframe(value ~ Spot * Vol, result)
persp3d(spot, vol, result$value, col = "skyblue",xlab = "Spot", ylab="Volatility", zlab="Value")
## -----

time <- seq(1/12,1,1/12)

c <- do.call(expand.grid, list(spot, time))
colnames(c) <- c("Spot", "Time")

value_delta <- mapply(function(Spot, Time) dko_package(Spot, Time, strike=1.12, upper = 1.2190, lower = 1.0230), c$Spot, c$Time)
result_delta <- cbind(c, value_delta)

idx <- result_delta %>% group_by(Time) %>% attr(.,"indices")

deltas <- idx %>% lapply(function(indices){
  result_delta[indices,] %>% (function(x)diff(x$value_delta))
})

c_merged <- do.call(expand.grid, list(spot[-1], time))
colnames(c_merged) <- c("Spot", "Time")
final_result_delta <- cbind(c_merged, delta=unlist(deltas))
final_result_delta %>% write.table(file = "delta_surface", na = "nan", row.names = F, quote = F)
persp3d(spot[-1], time, final_result_delta$delta, col = "skyblue",xlab = "Spot", ylab="Volatility", zlab="Value")
