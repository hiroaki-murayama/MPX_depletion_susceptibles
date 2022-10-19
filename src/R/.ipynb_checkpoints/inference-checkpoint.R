### set the parameters in the degree distribution estimated by Endo et al. (2022) Science.
alpha = 0.1; kappa=0.77; ### sensitivity analysis: [alpha = 0.1, kappa=0.88; alpha = 0.14, kappa=0.77; alpha = 0.06, kappa=0.77; alpha = 0.1, kappa=0.66; alpha = 0.1, kappa=0.88]
theta=(14/365)*((alpha/kappa)^(1/alpha));

dweibull_lt = function (x,shape,scale,u){
  if(min(x)<u){
    return(0)
    #stop('The value is less than left truncated point')
  }
  (dweibull(x,shape,scale)/(1-pweibull(u,shape,scale)))*heaviside(x,u)
}

pweibull_lt = function(q,shape,scale,u){
  if(min(q)<u){
    return(0)
    #stop('The quantile is less than left truncated point')
  }
  (pweibull(q,shape,scale)-pweibull(u,shape,scale))/
    (1-pweibull(u,shape,scale))
}

aa <- rep(0,1000*365)
width <- 1/365
for(t in 1:(1000*365)){
  x <- t*width
  aa[t] <- x*dweibull_lt(x,alpha,theta,14/365)
}
mu1 <- sum(aa)*width

width <- 1/365
N <- 1000000
upperl = 1001
S = rep(0,(upperl-1)/width)
for(t in 1:((upperl-1)/width)){
  x <- width*t 
  S[t] = width*dweibull_lt(x,alpha,theta,14/365)
}
wei_sum = sum(S)

Simulation_fn = function(n){
  upperl =1001
  fx = rep(0,(upperl-1)/width)
  sx = rep(0,(upperl-1)/width)
  for(t in 1:((upperl-1)/width)){
    x <- width*t 
    fx[t] = width*x*max(0,x-1)*dweibull_lt(x,alpha,theta,14/365)*exp(-(x*n)/(mu1*N))
    sx[t] = width*dweibull_lt(x,alpha,theta,14/365)*exp(-(x*n)/(mu1*N))
  }
  beta = mu1/sum(fx[1:((upperl-1)/width)])       ### SAR
  m = wei_sum-sum(sx[1:((upperl-1)/width)])      ### cumulative number of cases per MSM size
  Reff1 = 0.10*sum(fx[1:((upperl-1)/width)])/mu1 ### Reff under SAR of 0.1
  Reff2 = 0.20*sum(fx[1:((upperl-1)/width)])/mu1 ### Reff under SAR of 0.2
  df = c(beta,m,Reff1,Reff2) 
  return(df)
}

df <- mapply(Simulation_fn,seq(0,100*1600,100))
result_df <- df %>% t() %>% as.data.frame()
colnames(result_df) <- c("SAR","Infections","Reff_1","Reff_2")

result_df <- cbind(result_df,seq(0,100*1600,100))
colnames(result_df) <- c("SAR","Infections","Reff_1","Reff_2","n")
write.csv(result_df, "output/csv/inference/SAR_m_Reff.csv")
