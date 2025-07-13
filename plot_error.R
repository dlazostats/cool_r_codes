plot_error<-function(pred,obs){
  dpl<-data.frame(pred=pred,obs=obs)
  ggplot(dpl,aes(x = pred,y = obs)) +
    geom_point() +
    geom_abline(intercept = 0,slope = 1,color = "red",linewidth = 2)+
    labs(title=paste0("Test Prediction Error Plots, ", "RMSE_test: ",round(RMSE(dpl$pred,dpl$obs),3)),
         x="Prediction",y="Real")+
    theme_bw()->g1
  dplp<-dpl %>% mutate(id=1:n())%>% gather(var,value,-id)
  ggplot(dplp,aes(x=id,y=value,color=var))+
    geom_line(aes(linetype=var),size=0.8)+theme_bw()+theme(legend.title=element_blank())+
    scale_color_manual(values=c("#999999", "#56B4E9"))->g2
  dpl2<-dpl %>% mutate(error=pred-obs)
  ggplot(dpl2,aes(x=error))+
    geom_histogram(aes(y=after_stat(density)),color="black",fill="#E69F00")+
    geom_density(alpha=.2)+
    theme_bw()->g3
  dplp %>%
    ggplot( aes(x=value, fill=var)) +
    geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_bw() +
    labs(fill="",y="",x="")->g0
  dplp %>%
    ggplot(aes(x=var, y=value,color=var)) +
    geom_boxplot()+
    geom_jitter(shape=16, position=position_jitter(0.2))+
    theme_bw() +
    labs(x="")+
    coord_flip()->g00
  (g1+g3)/(g00+g0)/g2
}