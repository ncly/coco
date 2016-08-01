require(SMFI5)

estimate_CIR_parameters <- function(data, method = 'Hessian', days = 360 , significanceLevel = 0.95){
  # Estimation of parameters of Cox-Ingersoll-Ross 1985 model
  R   <- data[,1]
  tau <- data[,2]
  h <- 1 / days
  
  # Estimation of starting parameters corresponding to those of a Feller process
  phi0 <- acf(R, 1, plot = FALSE)
  kappa0 <- - log(phi0[1]$acf) / h
  r_bar0  <- mean(R)
  sigma_r0 <- sd(R) * sqrt(2 * kappa0 / r_bar0)
  
  theta0 <- c(log(kappa0), log(r_bar0), log(sigma_r0), 0, 0)
  
  # Maximization of the log-Likelihood
  n <- length(R)
  optim.results <- optim(theta0, function(x) sum(LogLikCIR(x, R, tau, days, n)), hessian = TRUE)
  theta <- optim.results$par
  kappa <- exp(theta[1])
  r_bar  <- exp(theta[2])
  sigma_r <- exp(theta[3])
  
  return(list("kappa" = kappa, "r_bar" = r_bar, "sigma_r" = sigma_r))
}