# Price of Contingent Convertible Bond
price_coco_cd <- function(t, T, S_t, S_star, C_p, c_i, r, N, q, sigma){
  
  spread_coco <- calc_spread_coco(t, T, S_t, S_star, C_p, r, q, sigma)
  V_t_coco <- N * exp(-(r + spread_coco) * (T - t))
  
  for (t in 1:T){
    V_t_coco <- V_t_coco + c_i * exp(-(r + spread_coco) * t)  
  }
  return(V_t_coco)
}

# Calculation of Trigger Probability
calc_p_star <- function(t, T, S_t, S_star, r, q, sigma){
  p_star <- pnorm((log(S_star / S_t) - calc_mu(r, q, sigma) * (T - t)) / (sigma * sqrt(T - t))) + (S_star / S_t)^(2 * calc_mu(r, q, sigma) / sigma^2) * pnorm((log(S_star / S_t) + calc_mu(r, q, sigma) * (T - t)) / (sigma * sqrt(T - t)))
  return(p_star)
}

# Calculation of Drift of Underlying
calc_mu <- function(r, q, sigma){
  mu <- r - q - sigma^2 / 2
  return(mu)
}

# Spread of CoCo Bond
calc_spread_coco <- function(t, T, S_t, S_star, C_p, r, q, sigma){
  spread_coco <- - log(1 - calc_p_star(t, T, S_t, S_star, r, q, sigma)) / (T - t) * (1 - S_star / C_p)
  return(spread_coco)
}

# Pricing Example
price_coco_cd(t <- 0, T <- 5, S_t <- 40, S_star <- 20, C_p <- 25, c_i <- 7, r <- 0.03, N <- 100, q <- 0, sigma <- 0.3)