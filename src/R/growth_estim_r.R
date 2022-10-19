c_number <- rep(0,length(country))
for(i in 1:length(country)){
  dff <- df %>% filter(Country==country[i])
  dff$Date <- as.Date(dff$Date)
  
  dff <- dff %>% dplyr::select("Cases","Cumulative_cases")  
  row_sub = apply(dff, 1, function(row) all(row !=0 ))
  num <- grep("TRUE",row_sub)
  dff<- dff[row_sub,] 
  dff %<>% cbind(num)
  df_fin <- dff[!apply(as.data.frame(dff$Cumulative_cases), 1, function(row) all(row <=10 )),]
  
  if(is.na(df_fin$Cases[1])) {c_number[i]=FALSE} else {if(df_fin$Cases[1]) {c_number[i]=i}} 
}
c_number[c_number==0] <-NA
c_number <- c_number[!is.na(c_number)]


### stan
numb_i <- c(1:length(c_number))
numb_i <- numb_i[-c(12,14,16,22,24,25,29,30,32,44,45,50,51)]  # exclude countries: less than 10 cases or only 5 days or less since  introduction of first 10 cases
for(i in numb_i){ 
  N_pop <- c(46090433, 26136558, 9115559, 11696761, 12011327, 215769215, 38445168, 19470178, 52051411, 4050768, 10751718, 95414401, 5846420, 11085852, 18220330, 1326535, 5559000, 65580278, 84352236, 32452865, 10314829, 1326535, 9607599, 346002, 1417566557, 5055121, 8655535, 60273038, 2961167, 6825445, 647588, 444076, 131843985, 17215189, 217163611, 5511411, 4314767, 33961522, 37757348, 10196709, 2702749, 18961090, 8661597, 5947961, 5465323, 2079544, 46793237, 10233038, 8789076, 86289462, 10147225, 68643511, 335184010)
  dff <- df %>% filter(Country==country[c_number[i]])
  dff$Date <- as.Date(dff$Date)
  
  dff <- dff %>% dplyr::select("Cases","Cumulative_cases")  
  row_sub = apply(dff, 1, function(row) all(row !=0 ))
  num <- grep("TRUE",row_sub)
  dff<- dff[row_sub,] 
  dff %<>% cbind(num)
  df_fin <- dff[!apply(as.data.frame(dff$Cumulative_cases), 1, function(row) all(row <=10 )),]
  df_fin1 <- dff                     
  
  it = df_fin$Cumulative_cases
  t = df_fin$num
  T= length(it)
  TT = df_fin$num[T]
  N_pop=N_pop[i]
  data = list(T=T, it=it, t=t, TT=TT, N_pop=N_pop)
  # specify parameters to monitor
  parameters = c("K","N0","a","cases","r")
  nuts_fit = stan(model_code=Model1, data=data, pars=parameters, iter=35000, warmup=5000, chain=4)
  
  stan_extract <- summary(nuts_fit)$summary %>% as.data.frame()
  stan_extract1 <- stan_extract[4:(TT+3),] %>% dplyr::select("2.5%","50%","97.5%") 
  estim_df <- cbind(seq(1,TT,1),stan_extract1)
  colnames(estim_df) <- c("date","lower","median","upper")
  
  stan_extract2 <- stan_extract %>% tail(TT+1) %>% dplyr::select("2.5%","50%","97.5%") 
  growth_df <- cbind(seq(1,TT,1),stan_extract2[-(TT+1),])
  colnames(growth_df) <- c("date","lower","median","upper")
  head(growth_df)
  
  
  
  
  #stan_extract3 <- stan_extract[4:(growth_date[1]-t[1]+4),] %>% dplyr::select("2.5%","50%","97.5%") 
  growth_date <- growth_df[growth_df$lower<=0.01 & growth_df$upper>=-0.01,]$date
  if(is.na(growth_date[1])) {
    stan_extract3 <- stan_extract[4:(TT+3),] %>% dplyr::select("2.5%","50%","97.5%") 
    stan_extract4 <- c(NA,NA,NA) %>% t()
    stan_extract5 <- c(NA,NA,NA) %>% t()
    
    estim_df1 <- cbind(seq(1,TT,1),stan_extract3) %>% as.data.frame()
    estim_df2 <- cbind(c(1),stan_extract4) %>% as.data.frame()
    estim_df3 <- cbind(c(1),stan_extract5) %>% as.data.frame()
    colnames(estim_df1) <- c("date","lower","median","upper")
    colnames(estim_df2) <- c("date","lower","median","upper")
    colnames(estim_df3) <- c("date","lower","median","upper") 
    L <- 1
  } else{if(growth_date[1]) {
    l <- length(growth_date); 
    stan_extract3 <- stan_extract[4:(growth_date[1]+3),] %>% dplyr::select("2.5%","50%","97.5%") 
    stan_extract4 <- stan_extract[(growth_date[1]+3):(growth_date[l]+3),] %>% dplyr::select("2.5%","50%","97.5%") 
    
    
    if(TT==growth_date[l]){
      stan_extract5 <- c(NA,NA,NA) %>% t()
    } 
    if(TT!=growth_date[l]){
      stan_extract5 <- stan_extract[(growth_date[l]+3):(TT+3),] %>% dplyr::select("2.5%","50%","97.5%") 
    }
    
    estim_df1 <- cbind(seq(1,growth_date[1],1),stan_extract3) %>% as.data.frame()
    estim_df2 <- cbind(seq(growth_date[1],growth_date[l],1),stan_extract4) %>% as.data.frame()
    estim_df3 <- cbind(seq(growth_date[l],TT,1),stan_extract5) %>% as.data.frame()
    colnames(estim_df1) <- c("date","lower","median","upper")
    colnames(estim_df2) <- c("date","lower","median","upper")
    colnames(estim_df3) <- c("date","lower","median","upper")
    L <- nrow(estim_df2)
  }}
  
  
  
  
  write.csv(estim_df2, paste0("output/csv/country/",country[c_number[i]],"_growth.csv"))
  
  options(repr.plot.width=20,repr.plot.height=8)
  estim <- ggplot(estim_df,aes(x=date)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = estim_df2$lower[1], ymax = estim_df2$upper[L], fill = my_colors("bly")[8], alpha = 0.15) +
    geom_bar(df_fin1, mapping=aes(x=num, y=Cumulative_cases, fill="Cumulative_cases"),stat='identity', width=0.7) + scale_fill_manual(values=c("#8856a7")) + 
    geom_line(estim_df1,mapping=aes(x=date, y=median), color=RColorBrewer::brewer.pal(11, "RdBu")[11], size=1, alpha=0.5) +
    geom_ribbon(estim_df1,mapping=aes(x=date,ymin=lower,ymax=upper), fill = RColorBrewer::brewer.pal(11, "RdBu")[11], alpha=0.1) +@
    geom_line(estim_df2,mapping=aes(x=date, y=median), color=RColorBrewer::brewer.pal(11, "RdBu")[11], size=1, alpha=1) +
    geom_ribbon(estim_df2,mapping=aes(x=date,ymin=lower,ymax=upper), fill = RColorBrewer::brewer.pal(11, "RdBu")[11], alpha=0.3) +@
    geom_line(estim_df3,mapping=aes(x=date, y=median), color=RColorBrewer::brewer.pal(11, "RdBu")[11], size=1, alpha=0.5) +
    geom_ribbon(estim_df3,mapping=aes(x=date,ymin=lower,ymax=upper), fill = RColorBrewer::brewer.pal(11, "RdBu")[11], alpha=0.1) +@
    theme_bw(base_size = 24)+ theme (legend.position="none") + ggtitle(paste0(country[c_number[i]])) +
    labs(x="# of days after an introduction of primary cases", y = "Cumulative incidence") + scale_x_continuous(breaks = seq(1, t[T]+1, 10), limits=c(1, t[T]+1))
  #scale_y_continuous()# + ggtitle("Check fitness")
  
  gr <- growth_df %>% ggplot(aes(x=date)) +
    geom_line(aes(x=date, y=median), color=RColorBrewer::brewer.pal(6, "RdBu")[6], size=1) +
    geom_ribbon(aes(x=date,ymin=lower,ymax=upper), fill = RColorBrewer::brewer.pal(6, "RdBu")[6], alpha=0.3) +
    theme_bw(base_size = 24)+ theme (legend.position="none") + ggtitle(paste0(country[c_number[i]])) +
    labs(x="# of days after an introduction of primary cases", y = "Local growth rate") + scale_x_continuous(breaks = seq(1, t[T]+1, 10), limits=c(1, t[T]+1)) +
    geom_hline(yintercept=0, linetype="dashed", color = "#1380A1", size =0.7) + 
    geom_hline(yintercept=-0.01, linetype="dashed", color = "#1380A1", size =1) + 
    geom_hline(yintercept=0.01, linetype="dashed", color = "#1380A1", size =1)
  # scale_y_continuous(breaks = seq(0, 100, 10), limits=c(0, 100)) + ggtitle("Check fitness")
  
  ggarrange(estim, gr, nrow=1,ncol=2,font.label=list(size=25),labels=c("A","B"))
  ggsave(file = paste0("output/fig/country/",country[c_number[i]],"_growth.png"), width = 20, height = 8)
}