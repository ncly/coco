library(fExtremes)

estimate_Jump_parameters <- function(returns, jump_Intensity){
  # Estimation of mean jump size and standard deviation of jumps

  positivThreshold <- findThreshold(returns, n = jump_Intensity / 2)
  negativThreshold <- - findThreshold(-returns, n = jump_Intensity / 2)
  
  jumps <- rbind(positivThreshold, negativThreshold)
  mean_jump <- mean(jumps)
  sd_jump <- sd(jumps)
  
  return(list("mean_jump" = mean_jump, "sd_jump" = sd_jump))
}