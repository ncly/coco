# Price of Contingent Convertible Bond
price_coco_ed <- function(t, T, S_t, S_star, C_p, c_i, r, N, q, sigma, alpha){
  V_t_ed <- price_cb(t, T, c_i, r, N) - price_dibi(t, T, S_t, S_star, c_i, r, q, sigma, alpha) + price_difwd(t, T, S_t, S_star, C_p, r, N, q, sigma, alpha)
  
  return(V_t_ed)
}

# Price of Corporate Bond
price_cb <- function(t, T, c_i, r, N){
  V_t_cb <- N * exp(-r * (T - t))
  
  for (t in 1:T){
  V_t_cb <- V_t_cb + c_i * exp(-r * t)  
  }
  
  return(V_t_cb)
}

# Price of Binary Option
price_dibi <- function(t, T, S_t, S_star, c_i, r, q, sigma, alpha){
  V_t_dibi <- 0
  
  i <- t
  k <- T
  
  for (i in 1:k) {
  V_t_dibi <- V_t_dibi + c_i * exp(- r * i) * (pnorm(- calc_x_1_i(S_t, S_star, sigma, r, q, i) + sigma * sqrt(i)) + (S_star / S_t)^(2 * calc_lambda(r, q, sigma) - 2) * pnorm ( calc_y_1_i(S_t, S_star, sigma, r, q, i) - sigma * sqrt(i)))
  }
  
  V_t_dibi <- alpha * V_t_dibi
  
  return(V_t_dibi)
}

# Price of Down-And-In Forward
price_difwd <- function(t, T, S_t, S_star, C_p, r, N, q, sigma, alpha){
  V_t_difwd <- calc_conversion_rate(C_p, N, alpha) * (S_t * exp(- q * (T - t)) * (S_star / S_t) ^ (2 * calc_lambda(r, q, sigma)) * pnorm(calc_y_1(t, T, S_t, S_star, r, q, sigma)) - C_p * exp(- r * (T - t)) * (S_star / S_t)^(2 * calc_lambda(r, q, sigma) - 2) * pnorm(calc_y_1(t, T, S_t, S_star, r, q, sigma) - sigma * sqrt(T - t)) - C_p * exp(- r * (T - t)) * pnorm(- calc_x_1(t, T, S_t, S_star, r, q, sigma) + sigma * sqrt(T - t)) + S_t * exp(- q * (T - t)) * pnorm(- calc_x_1(t, T, S_t, S_star, r, q, sigma)))
  
  return(V_t_difwd)
}

# Calculation of Conversion Rate
calc_conversion_rate <- function(C_p, N, alpha){
  C_r <- alpha * N / C_p
  
  return(C_r)
}
  
# Calculation of additional Parameters
calc_x_1_i <- function(S_t, S_star, sigma, r, q, t_i){
  x_1_i <- log(S_t / S_star) / (sigma * sqrt(t_i)) + calc_lambda(r, q, sigma) * sigma * sqrt(t_i)
  
  return(x_1_i)
}

calc_y_1_i <- function(S_t, S_star, sigma, r, q, t_i){
  y_1_i <- log(S_star / S_t) / (sigma * sqrt(t_i)) + calc_lambda(r, q, sigma) * sigma * sqrt(t_i)
  
  return(y_1_i)
}

calc_lambda <- function(r, q, sigma){
  lambda <- (r - q + sigma^2 / 2) / sigma^2
  
  return(lambda)
}

calc_x_1 <- function(t, T, S_t, S_star, r, q, sigma){
  x_1 <- log(S_t / S_star) / (sigma * sqrt(T - t)) + calc_lambda(r, q, sigma) * sigma * sqrt(T - t)
  
  return(x_1)
}

calc_y_1 <- function(t, T, S_t, S_star, r, q, sigma){
  y_1 <- log(S_star / S_t) / (sigma * sqrt(T - t)) + calc_lambda(r, q, sigma) * sigma * sqrt(T - t)
  
  return(y_1)
}

# Pricing Example
price_coco_ed(t <- 0, T <- 10, S_t <- 100, S_star <- 35, C_p <- 65, c_i <- 6.00, r <- 0.01, N <- 100, q <- 0.02, sigma <- 0.3, alpha <- 1)

