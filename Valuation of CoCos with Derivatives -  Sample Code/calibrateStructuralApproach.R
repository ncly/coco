source('estimateCIRParameter.R')
source('estimateMertonParameter.R')
source('estimateJumpParameter.R')

calibrate_CIR <- function(cir_data){
  cir_parameters <- estimate_CIR_parameters(cir.data)
  
  kappa <- cir_parameters$kappa
  r_bar <- cir_parameters$r_bar
  sigma_r <- cir_parameters$sigma_r
  
  return(cir_parameters)
}

calibrate_Merton <- function(deposits, marketcap, r, volatility_equity){
  merton_parameters <- estimate_Merton_parameters(deposits, marketcap, r, volatility_equity)
 
  asset_volatility <- merton_parameters$asset_volatility
  asset_value <- merton_parameters$asset_value
  
  return(merton_parameters)
}

calibrate_Jump <- function(returns, jump_Intensity){
  jump_parameters <- estimate_Jump_parameters(returns, jump_Intensity)
  
  mean_jump <- jump_parameters$mean_jump
  sd_jump <- jump_parameters$sd_jump
  
  return(jump_parameters)
}