source('CreditDerivativeApproach.R')

# CoCo price V^st as function of initial asset-to-deposit ratio x_0 and volatility sigma
createData_SA_x0_sigma <- function(x0_min, x0_max, sigma_min, sigma_max){
  data <- matrix(1, 121, 3)
  counter <- 1
  for(x0_increment in seq(from=x0_min, to=x0_max, by=((x0_max-x0_min)/10)))
  {
    for(sigma_increment in seq(from=sigma_min, to=sigma_max, by=((sigma_max-sigma_min)/10)))
    {
      data[counter, 1] <- x0_increment
      data[counter, 2] <- price_coco_sa(T <- 10, npath <- 5000, rho <- - 0.2, kappa <- 0.114, r_bar <- 0.069, r0 <- 0.01, sigma_r <- 0.07, mu_Y <- -0.01, sigma_Y <- 0.02, lambda <- c(1), g <- c(0.5), x_hat <- 1.1, b0 <- 0.04, p <- 1, e_bar <- 0.02, sigma_x <- sigma_increment, x0_low <- x0_increment, x0_high <- x0_increment, x0_nint <- 10, B <- 1, c_low <- 0.06, c_high <- 0.06, c_nint <- 10)
      data[counter, 3] <- sigma_increment
      counter <- counter + 1
    }
  }
  write.table(data, file = "createData_SA_x0_sigma.txt", row.names = FALSE, quote=FALSE)
}

# CoCo price V^sa as function of maturity T and risk-free interest rate r
createData_SA_T_r <- function(T_min, T_max, r_min, r_max){
  data <- matrix(1, 121, 3)
  counter <- 1
  for(T_increment in seq(from=T_min, to=T_max, by=((T_max-T_min)/10)))
  {
    for(r_increment in seq(from=r_min, to=r_max, by=((r_max-r_min)/10)))
    {
      data[counter, 1] <- T_increment
      data[counter, 2] <- price_coco_sa(T <- T_increment, npath <- 5000, rho <- - 0.2, kappa <- 0.114, r_bar <- 0.069, r0 <- r_increment, sigma_r <- 0.07, mu_Y <- -0.01, sigma_Y <- 0.02, lambda <- c(1), g <- c(0.5), x_hat <- 1.1, b0 <- 0.04, p <- 1, e_bar <- 0.02, sigma_x <- 0.02, x0_low <- 1.15, x0_high <- 1.15, x0_nint <- 10, B <- 1, c_low <- 0.06, c_high <- 0.06, c_nint <- 10)
      data[counter, 3] <- r_increment
      counter <- counter + 1    
    }
  }
  write.table(data, file = "createData_SA_T_r.txt", row.names = FALSE, quote=FALSE)
}

# CoCo price V^sa as function of initial asset-to-deposit ratio x_0 and equity-to-deposit threshold bar_e
createData_SA_x0_ebar <- function(x0_min, x0_max, ebar_min, ebar_max){
  data <- matrix(1, 121, 3)
  counter <- 1
  for(x0_increment in seq(from=x0_min, to=x0_max, by=((x0_max-x0_min)/10)))
  {
    for(ebar_increment in seq(from=ebar_min, to=ebar_max, by=((ebar_max-ebar_min)/10)))
    {
      data[counter, 1] <- x0_increment
      data[counter, 2] <- price_coco_sa(T <- 10, npath <- 5000, rho <- - 0.2, kappa <- 0.114, r_bar <- 0.069, r0 <- 0.01, sigma_r <- 0.07, mu_Y <- -0.01, sigma_Y <- 0.02, lambda <- c(1), g <- c(0.5), x_hat <- 1.1, b0 <- 0.04, p <- 1, e_bar <- ebar_increment, sigma_x <- 0.02, x0_low <- x0_increment, x0_high <- x0_increment, x0_nint <- 10, B <- 1, c_low <- 0.06, c_high <- 0.06, c_nint <- 10)
      data[counter, 3] <- ebar_increment
      counter <- counter + 1    
    }
  }
  write.table(data, file = "createData_SA_x0_ebar.txt", row.names = FALSE, quote=FALSE)
}

# CoCo price V^st as function of initial asset-to-deposit ratio x_0 and jump intensity in asset return process lambda
createData_SA_x0_lambda <- function(x0_min, x0_max, lambda_min, lambda_max){
  data <- matrix(1, 121, 3)
  counter <- 1
  for(x0_increment in seq(from=x0_min, to=x0_max, by=((x0_max-x0_min)/10)))
  {
    for(lambda_increment in seq(from=lambda_min, to=lambda_max, by=((lambda_max-lambda_min)/10)))
    {
      data[counter, 1] <- x0_increment
      data[counter, 2] <- price_coco_sa(T <- 10, npath <- 5000, rho <- - 0.2, kappa <- 0.114, r_bar <- 0.069, r0 <- 0.01, sigma_r <- 0.07, mu_Y <- -0.01, sigma_Y <- 0.02, lambda <- c(lambda_increment), g <- c(0.5), x_hat <- 1.1, b0 <- 0.04, p <- 1, e_bar <- 0.02, sigma_x <- 0.02, x0_low <- x0_increment, x0_high <- x0_increment, x0_nint <- 10, B <- 1, c_low <- 0.06, c_high <- 0.06, c_nint <- 10)
      data[counter, 3] <- lambda_increment
      counter <- counter + 1    
    }
  }
  write.table(data, file = "createData_SA_x0_lambda.txt", row.names = FALSE, quote=FALSE)
}

# CoCo price V^st as function of initial asset-to-deposit ratio x_0 and initial ratio of contingent capital to deposits b0
createData_SA_x0_b0 <- function(x0_min, x0_max, b0_min, b0_max){
  data <- matrix(1, 121, 3)
  counter <- 1
  for(x0_increment in seq(from=x0_min, to=x0_max, by=((x0_max-x0_min)/10)))
  {
    for(b0_increment in seq(from=b0_min, to=b0_max, by=((b0_max-b0_min)/10)))
    {
      data[counter, 1] <- x0_increment
      data[counter, 2] <- price_coco_sa(T <- 10, npath <- 5000, rho <- - 0.2, kappa <- 0.114, r_bar <- 0.069, r0 <- 0.01, sigma_r <- 0.07, mu_Y <- -0.01, sigma_Y <- 0.02, lambda <- c(1), g <- c(0.5), x_hat <- 1.1, b0 <- b0_increment, p <- 1, e_bar <- 0.02, sigma_x <- 0.02, x0_low <- x0_increment, x0_high <- x0_increment, x0_nint <- 10, B <- 1, c_low <- 0.06, c_high <- 0.06, c_nint <- 10)
      data[counter, 3] <- b0_increment
      counter <- counter + 1    
    }
  }
  write.table(data, file = "createData_SA_x0_b0.txt", row.names = FALSE, quote=FALSE)
}

createData_SA_x0_sigma(1.08, 1.17, 0.01, 0.05)
createData_SA_T_r(1, 50, 0.01, 0.05)
createData_SA_x0_ebar(1.08, 1.17, 0.005, 0.03)
createData_SA_x0_lambda(1.08, 1.17, 0, 2)
createData_SA_x0_b0(1.08, 1.17, 0.1, 0.06)
