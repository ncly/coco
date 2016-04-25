price_coco_sa <- function(T , npath , rho , kappa , r.bar = 0.069, r0 = 0.035, sigma_r = 0.07){
  n <- T * 250
  dt <- T / n
  
  result <- sim_corrProcess(T, npath, rho, n, dt)
  dW_1 <- result$dW_1
  dW_2corr <- result$dW_2corr
  
  r <- sim_interestrate(kappa, r.bar, r0, sigma_r, dW_2corr, n, npath, dt)
  
  V_t_sa <- get_price(npath, n, dt, dW_1, dW_2corr, r) * 100
  
  print(V_t_sa)
}

sim_corrProcess <- function(T, npath, rho, n, dt){
  vect <- c(1, rho, rho, 1)
  RHO <- matrix(vect, nrow = 2)
  chol.RHO <- t(chol(RHO)) ### What does this function do?

  # Create two Brownian Motions
  dW_1 <- matrix(1, n, npath)
  dW_2 <- matrix(1, n, npath)
  
  for(j in 1:npath)
  {
    dW_1[ , j] <- rnorm(n) * sqrt(dt)
    dW_2[ , j] <- rnorm(n) * sqrt(dt)
  }
  
  # Create Correlated Process based on Brownian Motions using Cholesky-Decomposition
  dW_2corr <- matrix(1, n, npath)
  for(j in 1:npath)
  {
    for(i in 1:n)
    {
      dW_2corr[i, j] <- dW_1[i, j] * chol.RHO[2, 1] + dW_2[i, j] * chol.RHO[2, 2] ### Why are those GBMs correlated?
    }
  }
  return(list("dW_1" = dW_1, "dW_2corr" = dW_2corr))
}

# Create Interest Rate Process
sim_interestrate <- function(kappa, r.bar, r0, sigma_r, dW_2corr, n, npath, dt){
  r <- matrix(r0, n + 1, npath)
  
  for(j in 1:npath)
  {
    for(i in 1:n)
    {
      r[i + 1, j] <- r[i, j] + kappa * (r.bar - r[i, j]) * dt + sigma_r * sqrt(r[i, j]) * dW_2corr[i, j] ### Why are those GBMs correlated?
    }
  }
  return(r)
}


get_price <- function(npath, n, dt, dW_1, dW_2corr, r){

  # Initiliaze jump parameters
  mu_Y <- - 0.01
  sigma_Y <- 0.02
  lambda <- c(1, 0, 1)
  
  #x
  g <- c(0.5, 0.5, 0.25)
  x.hat <- 1.1 # Capital ratio target
  b0 <- 0.04 # CCB-to-deposit ratio, B/D
  p <- 1 # Conversion parameter: par=1, premium>1, discount<1
  e.bar <- 0.02 # equity-to-deposit ratio
  sigma_x <- 0.02 # Standard deviation for assets
  #x0 <- 1.075 # Starting capital-to-deposit ratio
  x0.low <- 1 + 0.065
  x0.high <- 1 + 0.15
  x0.nint <- 10 # number of intervals between x0.low and x0.high
  B <- 1
  
  c.low <- 0.05
  c.high <- 0.05
  c.nint <- 10
  c.fit.matrix <- matrix(0, x0.nint, length(lambda))
  
  
  for(w in 1:length(lambda))
  {
    # Create parametres for jump process
    phi <- matrix(rbinom( n%*%npath, 1, dt * lambda[w]), n, npath)
    ln.Y <- matrix(rnorm(n%*%npath, mu_Y, sigma_Y), n, npath)

    b <- matrix(b0, n + 1, npath) # Ratio of the contingent capital's par value to the date t value of deposits
    x.bar0 <- 1 + e.bar + p * b0 
    x.bar <- matrix(x.bar0, n + 1, npath) # Asset-to-deposit threshold
    
    h <- matrix(1, n, npath) # Fair deposit insurance premium or deposit credit risk premium
    
    k <- exp(mu_Y + 0.5 * sigma_Y^2) - 1
    
    c <- seq(c.low, c.high, length = c.nint)
    x0 <- seq(x0.low, x0.high, length = x0.nint)
    reg.matrix <- matrix(0, c.nint, length(x0))
    
    reg.fit1 <- 1:x0.nint
    reg.fit2 <- 1:x0.nint
    c.fit <- 1:x0.nint
    
    for(l in 1:x0.nint)
    {
      for(m in 1:c.nint)
      {
        x <- matrix(x0[l],n+1,npath)
        ln.x0 <- matrix(log(x0[l]),n+1,npath)
        ln.x <- ln.x0
        binom.c <- matrix(1,n+1,npath)
        
        for(j in 1:npath)
        {
          for(i in 1:n)
          {
            d_1 <- (ln.x[i, j] + mu_Y) / sigma_Y
            d_2 <- d_1 + sigma_Y
            
            h[i, j] <- lambda[w] * (pnorm( - d_1) - exp(ln.x[i, j]) * exp(mu_Y + 0.5 * sigma_Y^2) * pnorm(-d_2))
            
            b[i + 1, j] <- b[i, j] * exp(- g[w] * (exp(ln.x[i, j]) - x.hat) * dt)
            
            ln.x[i + 1, j] <- ln.x[i, j] + ( (r[i, j] - lambda[w] * k) - (r[i, j] + h[i, j] + c[m] * b[i, j]) / exp(ln.x[i, j]) - g[w] * (exp(ln.x[i, j]) - x.hat) - 0.5 * sigma_x^2) * dt + sigma_x * sqrt(dt) * dW_1[i, j] + ln.Y[i,j] * phi[i, j]
            
            x[i + 1, j] <- exp(ln.x[i + 1, j])
            
            x.bar[i + 1, j] <- 1 + e.bar + p * b[i + 1, j]
            
            if(x[i + 1, j] >= x.bar[i + 1, j] && binom.c[i, j] > 0.5)
            {
              binom.c[i + 1, j] <- 1
            }else
            {
              binom.c[i + 1, j] <- 0
            }
          }
        }
        
        payments <- matrix(c(rep(c[m] * dt, n - 1), B), n, npath) * binom.c[1:n, ]
        for(j in 1:npath){
          for(i in 2:n){
            if(payments[i, j] == 0 && p * b[sum(binom.c[ , j]) + 1, j] <= x[sum(binom.c[ , j]) + 1, j] - 1 ){
              payments[i, j] <- p * B
              break
            }
            else if(payments[i, j] == 0 && 0 < x[sum(binom.c[ , j]) + 1, j] - 1 && x[sum(binom.c[ , j]) + 1, j] - 1 < p * b[sum(binom.c[ , j]) + 1, j]){
              payments[i, j] <- (x[sum(binom.c[ , j]) + 1, j] - 1) * B / b[sum(binom.c[ , j]) + 1, j]
              break
            }
            else{
              payments[i, j] <- payments[i, j]
            }
          }
        }
        
        vec.disc.v <- rep(0, npath)
        for(j in 1:npath)
        {
          disc.v <- 0
          int.r <- 0
          
          for(i in 1:n)
          {
            int.r <- int.r + r[i, j] * dt
            disc.v <- disc.v + exp(-int.r) * payments[i, j]
          }
          vec.disc.v[j] <- disc.v
        }
        V_t_sa <- mean(vec.disc.v)
        
        reg.matrix[m, l] <- V_t_sa
      }
      
      reg.fit1[l] <- lm(reg.matrix[ , l]~1+c)$coef[1]
      reg.fit2[l] <- lm(reg.matrix[,l]~1+c)$coef[2]

      c.fit[l] <- (B-reg.fit1[l])/reg.fit2[l]
    }
    c.fit.matrix[,w] <- c.fit
  }
  return(V_t_sa)
}

# Pricing Example
price_coco_sa(T = 5, npath = 50, rho = - 0.2, kappa = 0.114, r.bar = 0.069, r0 = 0.035, sigma_r = 0.07)