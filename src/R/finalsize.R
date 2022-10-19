### function of recursive final size equation
final_size_fn = function(sar_ratio,zz){
  N <- 1000000 ### msm size
  RR <- rep(0,zz-1)
  
  for(i in 1:(zz-1)){
    RR[i] <- sar_ratio*df_SAR_Reff$Reff_1[i+1]*(df_SAR_Reff$Infections[i+1]-df_SAR_Reff$Infections[i])
  }
  return(c(sum(RR)/(df_SAR_Reff$Infections[zz])))
}

### solve the equation
final_exposure <- function(sar_ratio){
  x=1000
  X <- rep(0,x)
  for(i in 1:x) {X[i] <- final_size_fn(sar_ratio,i)}   
  
  num <- which(abs(X-1) == min(abs(X-1))) 
  return(df_SAR_Reff$Infections[num])
}

df_final <- mapply(final_exposure,c(1e-5,seq(0.05,4,0.05)))
result_df_final <- cbind(c(1e-5,seq(0.05,4,0.05))*0.1,df_final) %>% as.data.frame()
colnames(result_df_final) <- c("SAR","case")