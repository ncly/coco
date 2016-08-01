source('StructuralApproach.R')

# CoCo price V^st as function of initial asset-to-deposit ratio x_0 and volatility sigma
createData_SA_x0_sigma <- function(x0_min, x0_max, sigma_min, sigma_max){
  data <- matrix(1, 121, 3)
  counter <- 1
  for(x0_increment in seq(from=x0_min, to=x0_max, by=((x0_max-x0_min)/10)))
  {
    for(sigma_increment in seq(from=sigma_min, to=sigma_max, by=((sigma_max-sigma_min)/10)))
    {
      data[counter, 1] <- x0_increment
      data[counter, 2] <- price_coco_sa(T <- 10, nsimulations <- 5000, rho <- 0.5, kappa <- 0.04, r_bar <- 0.06, r0 <- 0.03, sigma_r <- 0.05, mu_Y <- 0.00, sigma_Y <- 0.02, lambda <- 2, g <- 0.5, x_hat <- 1.12, b0 <- 0.0341, p <- 0.8, e_bar <- 0.0681, sigma_x <- sigma_increment, x0 <- x0_increment, B <- 1, coupon <- 0.06)
      data[counter, 3] <- sigma_increment
      counter <- counter + 1
    }
  }
  write.table(data, file = "createData_SA_x0_sigma_31Aug2016.txt", row.names = FALSE, quote=FALSE)
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
      data[counter, 2] <- price_coco_sa(T <- T_increment, nsimulations <- 5000, rho <- 0.5, kappa <- 0.04, r_bar <- 0.06, r0 <- r_increment, sigma_r <- 0.05, mu_Y <- 0.00, sigma_Y <- 0.02, lambda <- 2, g <- 0.5, x_hat <- 1.12, b0 <- 0.0341, p <- 0.8, e_bar <- 0.0681, sigma_x <- 0.0363, x0 <- 1.1364, B <- 1, coupon <- 0.06)
      data[counter, 3] <- r_increment
      counter <- counter + 1    
    }
  }
  write.table(data, file = "createData_SA_T_r_31Aug2016.txt", row.names = FALSE, quote=FALSE)
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
      data[counter, 2] <- price_coco_sa(T <- 10, nsimulations <- 5000, rho <- 0.5, kappa <- 0.04, r_bar <- 0.06, r0 <- 0.03, sigma_r <- 0.05, mu_Y <- 0.00, sigma_Y <- 0.02, lambda <- 2, g <- 0.5, x_hat <- 1.12, b0 <- 0.0341, p <- 0.8, e_bar <- ebar_increment, sigma_x <- 0.0363, x0 <- x0_increment, B <- 1, coupon <- 0.06)
      data[counter, 3] <- ebar_increment
      counter <- counter + 1    
    }
  }
  write.table(data, file = "createData_SA_x0_ebar_31Aug2016.txt", row.names = FALSE, quote=FALSE)
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
      data[counter, 2] <- price_coco_sa(T <- 10, nsimulations <- 5000, rho <- 0.5, kappa <- 0.04, r_bar <- 0.06, r0 <- 0.03, sigma_r <- 0.05, mu_Y <- 0.00, sigma_Y <- 0.02, lambda <- lambda_increment, g <- 0.5, x_hat <- 1.12, b0 <- 0.0341, p <- 0.8, e_bar <- 0.0681, sigma_x <- 0.0363, x0 <- x0_increment, B <- 1, coupon <- 0.06)
      data[counter, 3] <- lambda_increment
      counter <- counter + 1    
    }
  }
  write.table(data, file = "createData_SA_x0_lambda_31Aug2016.txt", row.names = FALSE, quote=FALSE)
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
      data[counter, 2] <- price_coco_sa(T <- 10, nsimulations <- 5000, rho <- 0.5, kappa <- 0.04, r_bar <- 0.06, r0 <- 0.03, sigma_r <- 0.05, mu_Y <- 0.00, sigma_Y <- 0.02, lambda <- 2, g <- 0.5, x_hat <- 1.12, b0 <- b0_increment, p <- 0.8, e_bar <- 0.0681, sigma_x <- 0.0363, x0 <- x0_increment, B <- 1, coupon <- 0.06)
      data[counter, 3] <- b0_increment
      counter <- counter + 1    
    }
  }
  write.table(data, file = "createData_SA_x0_b0_31Aug2016.txt", row.names = FALSE, quote=FALSE)
}

createData_SA_x0_sigma(1.08, 1.17, 0.01, 0.05)
createData_SA_T_r(1, 100, 0.01, 0.05)
createData_SA_x0_ebar(1.08, 1.17, 0.01, 0.07)
createData_SA_x0_lambda(1.08, 1.17, 0, 2)
createData_SA_x0_b0(1.08, 1.17, 0.01, 0.06)
