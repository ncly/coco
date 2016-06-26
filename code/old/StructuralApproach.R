# Price of Contingent Convertible Bond
price_coco_sa <- function(T , npath , rho , kappa , r_bar, r0, sigma_r, mu_Y, sigma_Y, lambda, g, x_hat, b0, p, e_bar, sigma_x, x0_low, x0_high, x0_nint, B, c_low, c_high, c_nint){
  n <- T * 250
  dt <- T / n
  
  result <- sim_corrProcess(T, npath, rho, n, dt)
  dz_1 <- result$dz_1
  dz_2corr <- result$dz_2corr
  
  r <- sim_interestrate(kappa, r_bar, r0, sigma_r, dz_2corr, n, npath, dt)
  
  V_t_sa <- get_price(npath, n, dt, dz_1, dz_2corr, r, mu_Y, sigma_Y, lambda, g, x_hat, b0, p, e_bar, sigma_x, x0_low, x0_high, x0_nint, B, c_low, c_high, c_nint) * 100
  return(V_t_sa)
}

# Create correlated Brownian motions for asset and interest rate process
sim_corrProcess <- function(T, npath, rho, n, dt){
  # Variables for Choleski decomposition
  vect <- c(1, rho, rho, 1)
  RHO <- matrix(vect, nrow = 2)
  chol_RHO <- t(chol(RHO))
  
  dz_1 <- matrix(1, n, npath)
  dz_2 <- matrix(1, n, npath)
  
  for(j in 1:npath)
  {
    dz_1[ , j] <- rnorm(n) * sqrt(dt)
    dz_2[ , j] <- rnorm(n) * sqrt(dt)
  }
  
  # Create correlated process based on Brownian motions using Cholesky-Decomposition
  dz_2corr <- matrix(1, n, npath)
  for(j in 1:npath)
  {
    for(i in 1:n)
    {
      dz_2corr[i, j] <- dz_1[i, j] * chol_RHO[2, 1] + dz_2[i, j] * chol_RHO[2, 2]
    }
  }
  
  return(list("dz_1" = dz_1, "dz_2corr" = dz_2corr))
}

# Create Interest Rate Process
sim_interestrate <- function(kappa, r_bar, r0, sigma_r, dz_2corr, n, npath, dt){
  r <- matrix(r0, n + 1, npath)
  
  for(j in 1:npath)
  {
    for(i in 1:n)
    {
      r[i + 1, j] <- r[i, j] + kappa * (r_bar - r[i, j]) * dt + sigma_r * sqrt(r[i, j]) * dz_2corr[i, j] 
    }
  }
  
  return(r)
}

get_price <- function(npath, n, dt, dz_1, dz_2corr, r, mu_Y, sigma_Y, lambda, g, x_hat, b0, p, e_bar, sigma_x, x0_low, x0_high, x0_nint, B, c_low, c_high, c_nint){
  
  c_fit_matrix <- matrix(0, x0_nint, length(lambda))
  
  for(w in 1:length(lambda))
  {
    # Create Parametres for Jump Process
    phi <- matrix(rbinom( n%*%npath, 1, dt * lambda[w]), n, npath)
    ln_Y <- matrix(rnorm(n%*%npath, mu_Y, sigma_Y), n, npath)
    
    b <- matrix(b0, n + 1, npath)
    x_bar0 <- 1 + e_bar + p * b0 
    x_bar <- matrix(x_bar0, n + 1, npath)
    
    h <- matrix(1, n, npath)
    
    k <- exp(mu_Y + 0.5 * sigma_Y^2) - 1
    
    c <- seq(c_low, c_high, length = c_nint)
    x0 <- seq(x0_low, x0_high, length = x0_nint)
    
    for(l in 1:x0_nint)
    {
      for(m in 1:c_nint)
      {
        x <- matrix(x0[l],n+1,npath)
        ln_x0 <- matrix(log(x0[l]),n+1,npath)
        ln_x <- ln_x0
        binom_c <- matrix(1,n+1,npath)
        
        for(j in 1:npath)
        {
          for(i in 1:n)
          {
            d_1 <- (ln_x[i, j] + mu_Y) / sigma_Y
            d_2 <- d_1 + sigma_Y
            
            h[i, j] <- lambda[w] * (pnorm( - d_1) - exp(ln_x[i, j]) * exp(mu_Y + 0.5 * sigma_Y^2) * pnorm(-d_2))
            
            b[i + 1, j] <- b[i, j] * exp(- g[w] * (exp(ln_x[i, j]) - x_hat) * dt)
            
            ln_x[i + 1, j] <- ln_x[i, j] + ( (r[i, j] - lambda[w] * k) - (r[i, j] + h[i, j] + c[m] * b[i, j]) / exp(ln_x[i, j]) - g[w] * (exp(ln_x[i, j]) - x_hat) - 0.5 * sigma_x^2) * dt + sigma_x * sqrt(dt) * dz_1[i, j] + ln_Y[i,j] * phi[i, j]
            
            x[i + 1, j] <- exp(ln_x[i + 1, j])
            
            x_bar[i + 1, j] <- 1 + e_bar + p * b[i + 1, j]
            
            if(x[i + 1, j] >= x_bar[i + 1, j] && binom_c[i, j] > 0.5)
            {
              binom_c[i + 1, j] <- 1
            }else
            {
              binom_c[i + 1, j] <- 0
            }
          }
        }
        
        cashflows <- matrix(c(rep(c[m] * dt, n - 1), B), n, npath) * binom_c[1:n, ]
        
        for(j in 1:npath){
          for(i in 2:n){
            if(cashflows[i, j] == 0 && p * b[sum(binom_c[ , j]) + 1, j] <= x[sum(binom_c[ , j]) + 1, j] - 1 ){
              cashflows[i, j] <- p * B
              break
            }
            else if(cashflows[i, j] == 0 && 0 < x[sum(binom_c[ , j]) + 1, j] - 1 && x[sum(binom_c[ , j]) + 1, j] - 1 < p * b[sum(binom_c[ , j]) + 1, j]){
              cashflows[i, j] <- (x[sum(binom_c[ , j]) + 1, j] - 1) * B / b[sum(binom_c[ , j]) + 1, j]
              break
            }
            else{
              cashflows[i, j] <- cashflows[i, j]
            }
          }
        }
        vec_disc_v <- rep(0, npath)
        for(j in 1:npath)
        {
          disc_v <- 0
          int_r <- 0
          
          for(i in 1:n)
          { 
            int_r <- int_r + r[i, j] * dt
            disc_v <- disc_v + exp(- int_r) * cashflows[i, j]
          }
          vec_disc_v[j] <- disc_v
        }
        
        V_t_sa <- mean(vec_disc_v)
        
        return(V_t_sa)
      }
    }
  }
}

# Pricing Example
price_coco_sa(T <- 10, npath <- 1000, rho <- - 0.2, kappa <- 0.114, r_bar <- 0.069, r0 <- 0.01, sigma_r <- 0.07, mu_Y <- -0.01, sigma_Y <- 0.02, lambda <- c(1), g <- c(0.5), x_hat <- 1.1, b0 <- 0.04, p <- 1, e_bar <- 0.02, sigma_x <- 0.02, x0_low <- 1.15, x0_high <- 1.15, x0_nint <- 10, B <- 1, c_low <- 0.06, c_high <- 0.06, c_nint <- 10)

