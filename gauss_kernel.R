# Gaussin Kernel Density
#-----------------------
library(ggplot2)

#Values
Mean = 0
SD = 1
Values = seq(-3,3, 0.1)

#Function
Gaussian_F = function(Mean, Standard_deviation, Observed_values){
  y = exp(-(Observed_values-Mean)^2/(2*Standard_deviation^2))/(Standard_deviation*sqrt(2*pi))
  return(y)
}
curve = Gaussian_F(Mean, SD, Values)
GBell = data.frame(Y = curve, X = Values)
plot(curve,type="l")

# Building the kernel density plot
density(iris$Petal.Length) %>% plot()

## One point
point = iris$Petal.Length[7]
range = seq(-3, 6, 0.01)
b = 1 #bandwith
GaussianPoint = data.frame(X = range,
                           Y = exp(-(range-point)^2/(2*b^2))/(b*sqrt(2*pi))/length(point))

ggplot()+
  geom_area(data = GaussianPoint, aes(x=X, y=Y), fill = "blue", alpha=0.5)+
  geom_point(aes(x=1.4, y=max(GaussianPoint$Y)))+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman", size=15)
  )

#Two points
point1 = iris$Petal.Length[7]
point2 = iris$Petal.Length[65]
range = seq(-10, 10, 0.01)
b = 1
n = 2
GaussianPoints = data.frame(
  X = range,
  Y1 = exp(-(range-point1)^2/(2*b^2))/(b*sqrt(2*pi)),
  Y2 = exp(-(range-point2)^2/(2*b^2))/(b*sqrt(2*pi))
)
GaussianPoints = GaussianPoints%>%
  mutate(Kernel = (Y1+Y2)/n)

ggplot()+
  geom_area(data = GaussianPoints, aes(x=X, y=Y1, fill = "Point 1"), alpha=0.5)+
  geom_area(data = GaussianPoints, aes(x=X, y=Y2, fill = "Point 2"), alpha=0.5)+
  geom_area(data = GaussianPoints, aes(x=X, y=Kernel, fill = "Kernel"), alpha=0.7)+
  scale_fill_manual(values = c("red", "yellow", "blue"))+
  geom_point(aes(x=1.4, y=max(GaussianPoints$Y1)))+
  geom_point(aes(x=3.6, y=max(GaussianPoints$Y2)))+
  labs(x = "Range", y="Densities", fill="Estimations")+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman", size=15)
  )

Gauss_K = function(Values, Range, h=1){
  library(dplyr) 
  
  #Result
  densities = data.frame()
  #Temporal variable
  Temp = data.frame()
  #One by one variable
  V = vector()
  
  for (i in 1:length(Values))
  {
    #The value
    V = Values[i]
    #Gaussian Function
    Temp =
      data.frame(
        Density = exp(-(Range-V)^2/(2*h^2))/(h*sqrt(2*pi)),
        Range = Range,
        Value = as.factor(paste0("X",i))
      )
    densities = rbind(densities, Temp)
  }
  Densities1 = densities
  Densities2 = densities%>%
    #Sum K by range value!
    group_by(Range)%>%
    summarise(Bell_sum = sum(Density))%>%
    #Normalization
    mutate(Kernel_Density = Bell_sum /length(Values))
  #Data Frame with points
  Points = Densities1%>%
    group_by(Value)%>%
    summarise(Y = max(Density))%>%
    mutate(Value = Values)
  
  
  return(
    list(Densities1, Densities2, Points)
  )
}

data = iris$Petal.Length
Range = seq(-5, 10, 0.1)
OurKernel = Gauss_K(data, Range, h = 1)
ggplot()+
  #Our estimation
  
  geom_line(data=OurKernel[[1]], aes(x=Range, y=Density, col=Value), 
            show.legend = F,alpha=0.3, position = "identity")+
  scale_fill_manual(values=sample(rainbow(1000),500))+
  
  geom_area(data = OurKernel[[2]], aes(x=Range, y=Kernel_Density, 
                                       fill = "Our Density"), 
            alpha=0.8)+
  
  geom_point(data = OurKernel[[3]], aes(x = Value, y = Y))+
  
  #Ggplot function
  geom_density(data = data.frame(data), aes(x = data, 
                                            fill="GGplot Density"), 
               alpha = 0.5)+
  
  scale_fill_manual(values = c("blue", "red"))+
  
  labs(x = "Range", y="Densities", fill="Estimations")+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman", size=15)
  )


data = iris$Petal.Length
Range = seq(-5, 10, 0.1)
OurKernel = Gauss_K(data, Range, h = bw.nrd0(data))
ggplot()+
  #Our estimation
  
  geom_line(data=OurKernel[[1]], aes(x=Range, y=Density, col=Value), 
            show.legend = F,alpha=0.3, position = "identity")+
  scale_fill_manual(values=sample(rainbow(1000),500))+
  
  geom_area(data = OurKernel[[2]], aes(x=Range, y=Kernel_Density, 
                                       fill = "Our Density"), 
            alpha=0.8)+
  
  geom_point(data = OurKernel[[3]], aes(x = Value, y = Y))+
  
  #Ggplot function
  geom_density(data = data.frame(data), aes(x = data, 
                                            fill="GGplot Density"), 
               alpha = 0.5)+
  scale_fill_manual(values = c("blue", "red"))+
  
  xlim(-3,10)+
  labs(x = "Range", y="Densities", fill="Estimations")+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman", size=15)
  )
