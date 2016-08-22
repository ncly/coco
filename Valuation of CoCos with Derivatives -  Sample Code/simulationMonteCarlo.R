source('StructuralApproach.R')

createData_simulation <- function(simulations){
  data <- matrix(50)
  time <- matrix(50)
  boxplotdata <- matrix(6)
  counter <- 1
  for(counter in 1:50)
  {
    # Start the clock!
    start.time <- Sys.time()
    data[counter] <- price_coco_sa(T <- 10, nsimulations <- simulations, rho <- 0.5, kappa <- 0.04, r_bar <- 0.06, r0 <- 0.03, sigma_r <- 0.05, mu_Y <- 0.00, sigma_Y <- 0.02, lambda <- 2, g <- 0.5, x_hat <- 1.12, b0 <- 0.0341, p <- 0.8, e_bar <- 0.0681, sigma_x <- 0.0367, x0 <- 1.1364, B <- 1, coupon <- 0.06)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time[counter] <- time.taken
    counter <- counter + 1
  }
  boxplotdata[1] <- min(data)
  boxplotdata[2] <- quantile(data, 0.25)
  boxplotdata[3] <- median(data)
  boxplotdata[4] <- quantile(data, 0.75)
  boxplotdata[5] <- max(data)
  boxplotdata[6] <- mean(time)
  
  write.table(boxplotdata, file = paste("boxplotdata_", simulations ,"_8Aug2016.txt"), row.names = FALSE, quote=FALSE)
}

createData_simulation(1)
createData_simulation(5)
createData_simulation(10)
createData_simulation(50)
createData_simulation(100)
createData_simulation(500)
createData_simulation(1000)
createData_simulation(5000)
  

