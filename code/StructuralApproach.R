# Price of Contingent Convertible Bond
price_coco_sa <- function(T , nsimulations , rho , kappa , r_bar, r0, sigma_r, mu_Y, sigma_Y, lambda, g, x_hat, b0, p, e_bar, sigma_A, x0, B, coupon){
  
  ndays <- T * 250
  dt <- T / ndays
  
  # Get Brownian motions
  result <- sim_corrProcess(T, nsimulations, rho, ndays, dt)
  dz_1 <- result$dz_1
  dz_2corr <- result$dz_2corr
  
  # Simulate Cox et al. (1985) term-structure process
  r <- sim_interestrate(kappa, r_bar, r0, sigma_r, dz_2corr, ndays, nsimulations, dt)
  
  # Simulate price of contingent convertible bond with a Monte-Carlo simulation
  V_t_sa <- get_price(nsimulations, ndays, dt, dz_1, dz_2corr, r, mu_Y, sigma_Y, lambda, g, x_hat, b0, p, e_bar, sigma_A, x0, B, coupon) * 100
  
  return(V_t_sa)
}

# Create correlated Brownian motions for asset and interest rate process
sim_corrProcess <- function(T, nsimulations, rho, ndays, dt){
  
  # Compute the Choleski factorization of a real symmetric positive-definite square matrix.
  chol_RHO <- t(chol(matrix(c(1, rho, rho, 1), nrow = 2)))
  
  # Random generation for the normal distribution with mean equal to 0 and standard deviation equal to 1
  dz_1 <- matrix(1, ndays, nsimulations)
  dz_2 <- matrix(1, ndays, nsimulations)
  for(j in 1:nsimulations)
  {
    dz_1[ , j] <- rnorm(ndays) * sqrt(dt)
    dz_2[ , j] <- rnorm(ndays) * sqrt(dt)
  }

  # Create correlated Brownian motions using Cholesky-decomposition for the Cox et al. (1985) term-structure process
  dz_2corr <- matrix(1, ndays, nsimulations)
  for(j in 1:nsimulations)
  {
    for(i in 1:ndays)
    {
      dz_2corr[i, j] <- dz_1[i, j] * chol_RHO[2, 1] + dz_2[i, j] * chol_RHO[2, 2]
    }
  }
  
  return(list("dz_1" = dz_1, "dz_2corr" = dz_2corr))
}

# Simulate Cox et al. (1985) term-structure process
sim_interestrate <- function(kappa, r_bar, r0, sigma_r, dz_2corr, ndays, nsimulations, dt){
  r <- matrix(r0, ndays + 1, nsimulations)
  
  for(j in 1:nsimulations)
  {
    for(i in 1:ndays)
    {
      r[i + 1, j] <- r[i, j] + kappa * (r_bar - r[i, j]) * dt + sigma_r * sqrt(abs(r[i, j])) * dz_2corr[i, j] 
    }
  }

  return(r)
}

get_price <- function(nsimulations, ndays, dt, dz_1, dz_2corr, r, mu_Y, sigma_Y, lambda, g, x_hat, b0, p, e_bar, sigma_A, x0, B, coupon){
  
  # Define parametres
  phi <- matrix(rbinom( ndays %*% nsimulations, 1, dt * lambda), ndays, nsimulations)
  
  ln_Y <- matrix(rnorm(ndays %*% nsimulations, mu_Y, sigma_Y), ndays, nsimulations)
  
  # Ratio of contingent capital's nominal to the value of deposits
  b <- matrix(b0, ndays + 1, nsimulations)
    
  h <- matrix(1, ndays, nsimulations)
  
  # Paramter for jump diffusion process 
  k <- exp(mu_Y + 0.5 * sigma_Y^2) - 1
    
  # Target asset-to-deposit ratio
  x_bar0 <- 1 + e_bar + p * b0
  x_bar <- matrix(x_bar0, ndays + 1, nsimulations)
  
  # Asset-to-deposit ratio
  x <- matrix(x0, ndays + 1, nsimulations)
  ln_x0 <- matrix(log(x0), ndays + 1, nsimulations)
  ln_x <- ln_x0
  
  trigger_dummy <- matrix(1, ndays + 1, nsimulations)
  
  # Simulate asset-to-deposit ratio and trigger events   

  for(j in 1:nsimulations)
  {
    for(i in 1:ndays)
    {
      d_1 <- (ln_x[i, j] + mu_Y) / sigma_Y
      d_2 <- d_1 + sigma_Y
            
      h[i, j] <- lambda * (pnorm( - d_1) - exp(ln_x[i, j]) * exp(mu_Y + 0.5 * sigma_Y^2) * pnorm(-d_2))
            
      b[i + 1, j] <- b[i, j] * exp(- g * (exp(ln_x[i, j]) - x_hat) * dt)
            
      ln_x[i + 1, j] <- ln_x[i, j] + ( (r[i, j] - lambda * k) - (r[i, j] + h[i, j] + coupon * b[i, j]) / exp(ln_x[i, j]) - g * (exp(ln_x[i, j]) - x_hat) - 0.5 * sigma_A^2) * dt + sigma_A * sqrt(dt) * dz_1[i, j] + ln_Y[i,j] * phi[i, j]
            
      x[i + 1, j] <- exp(ln_x[i + 1, j])
            
      x_bar[i + 1, j] <- 1 + e_bar + p * b[i + 1, j]
      
      if(is.na(trigger_dummy[i, j]) == TRUE){
        trigger_dummy[i, j] <- trigger_dummy[i-1, j]
      }
      
      if(x[i + 1, j] >= x_bar[i + 1, j] && trigger_dummy[i, j] > 0.5)
      {
        trigger_dummy[i + 1, j] <- 1
      }else
      {
        trigger_dummy[i + 1, j] <- 0
      }
    }
  }
        
  cashflows <- matrix(c(rep(coupon * dt, ndays - 1), B), ndays, nsimulations) * trigger_dummy[1:ndays, ]
  
  # Determine cashflows for each simulation
  for(j in 1:nsimulations){
    for(i in 2:ndays){
      if(cashflows[i, j] == 0 && p * b[sum(trigger_dummy[ , j]) + 1, j] <= x[sum(trigger_dummy[ , j]) + 1, j] - 1 ){
        cashflows[i, j] <- p * B
        break
      }
      else if(cashflows[i, j] == 0 && 0 < x[sum(trigger_dummy[ , j]) + 1, j] - 1 && x[sum(trigger_dummy[ , j]) + 1, j] - 1 < p * b[sum(trigger_dummy[ , j]) + 1, j]){
        cashflows[i, j] <- (x[sum(trigger_dummy[ , j]) + 1, j] - 1) * B / b[sum(trigger_dummy[ , j]) + 1, j]
        break
      }
      else{
        cashflows[i, j] <- cashflows[i, j]
      }
    }
  }
  list_discounted_cashflows <- rep(0, nsimulations)
  
  # Discount cashflows for each simulation
  for(j in 1:nsimulations)
  {
    disc_cashflows <- 0
    int_r <- 0
          
    for(i in 1:ndays)
    { 
      int_r <- int_r + r[i, j] * dt
      disc_cashflows <- disc_cashflows + exp(- int_r) * cashflows[i, j]
    }
    list_discounted_cashflows[j] <- disc_cashflows
  }
  
  # Calculate arithmetic average over all simulations as present value of contingent convertibles bond
  V_t_sa <- mean(list_discounted_cashflows)
        
  return(V_t_sa)
}

# Pricing Example
# price_coco_sa(T <- 10, nsimulations <- 5000, rho <- 0.5, kappa <- 0.04, r_bar <- 0.06, r0 <- 0.03, sigma_r <- 0.05, mu_Y <- 0.00, sigma_Y <- 0.02, lambda <- 2, g <- 0.5, x_hat <- 1.1494, b0 <- 0.0341, p <- 0.8, e_bar <- 0.0681, sigma_A <- 0.0367, x0 <- 1.1364, B <- 1, coupon <- 0.06)