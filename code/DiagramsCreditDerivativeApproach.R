source('CreditDerivativeApproach.R')

# CoCo price V^cd as function of share price S and volatility sigma
createData_CD_S_sigma <- function(S_min, S_max, sigma_min, sigma_max){
  data <- matrix(1, 121, 3)
  counter <- 1
  for(S_increment in seq(from=S_min, to=S_max, by=((S_max-S_min)/10)))
  {
    for(sigma_increment in seq(from=sigma_min, to=sigma_max, by=((sigma_max-sigma_min)/10)))
    {
      data[counter, 1] <- S_increment
      data[counter, 2] <- price_coco_cd(t <- 0, T <- 10, S_t <- S_increment, S_star <- 35, C_p <- 65, c_i <- 6, r <- 0.01, N <- 100, q <- 0.02, sigma <- sigma_increment)
      data[counter, 3] <- sigma_increment
      counter <- counter + 1
    }
  }
  write.table(data, file = "createData_CD_S_sigma.txt", row.names = FALSE, quote=FALSE)
}

# CoCo price V^cd as function of maturity T and risk-free interest rate r
createData_CD_T_r <- function(T_min, T_max, r_min, r_max){
  data <- matrix(1, 121, 3)
  counter <- 1
  for(T_increment in seq(from=T_min, to=T_max, by=((T_max-T_min)/10)))
  {
    for(r_increment in seq(from=r_min, to=r_max, by=((r_max-r_min)/10)))
    {
      data[counter, 1] <- T_increment
      data[counter, 2] <- price_coco_cd(t <- 0, T <- T_increment, S_t <- 100, S_star <- 35, C_p <- 65, c_i <- 6, r <- r_increment, N <- 100, q <- 0.02, sigma <- 0.3)
      data[counter, 3] <- r_increment
      counter <- counter + 1    
    }
  }
  write.table(data, file = "createData_CD_T_r.txt", row.names = FALSE, quote=FALSE)
}

# CoCo price V^cd as function of trigger price S^* and conversion price C_p
createData_CD_Sstar_Cp <- function(S_star_min, S_star_max, C_p_min, C_p_max){
  data <- matrix(1, 121, 3)
  counter <- 1
  for(S_star_increment in seq(from=S_star_min, to=S_star_max, by=((S_star_max-S_star_min)/10)))
  {
    for(C_p_increment in seq(from=C_p_min, to=C_p_max, by=((C_p_max-C_p_min)/10)))
    {
      data[counter, 1] <- S_star_increment
      data[counter, 2] <- price_coco_cd(t <- 0, T <- 10, S_t <- 100, S_star <- S_star_increment, C_p <- C_p_increment, c_i <- 6, r <- 0.01, N <- 100, q <- 0.02, sigma <- 0.3)
      data[counter, 3] <- C_p_increment
      counter <- counter + 1    
    }
  }
  write.table(data, file = "createData_CD_Sstar_Cp.txt", row.names = FALSE, quote=FALSE)
}

createData_CD_S_sigma(35.01, 100, 0.1, 0.5)
createData_CD_T_r(1, 50, 0.01, 0.05)
createData_CD_Sstar_Cp(20, 40, 40, 70)