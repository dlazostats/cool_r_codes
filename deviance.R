# Data for boys
gest_boys <- c(40, 38, 40, 35, 36, 37, 41, 40, 37, 38, 40, 38)
weight_boys <- c(2968, 2795, 3163, 2925, 2625, 2847, 3292, 3473, 2628, 3176, 3421, 2975)

# Data for girls
gest_girls <- c(40, 36, 40, 38, 42, 39, 40, 37, 36, 38, 39, 40)
weight_girls <- c(3317, 2729, 2935, 2754, 3210, 2817, 3126, 2539, 2412, 2991, 2875, 3231)

# Create a combined dataframe
df <- data.frame(
  weight = c(weight_boys, weight_girls),
  gest = c(gest_boys, gest_girls),
  sex = factor(rep(c("Boy", "Girl"), each = 12))
) %>% mutate(sexo=ifelse(sex=="Boy",1,0))

m1<-lm(weight~gest+sexo,data=df)
summary(m1)
a1<-coef(m1)[[1]]+coef(m1)[[3]]
a2<-coef(m1)[[1]]
b<-coef(m1)[[2]]
c(a1,a2,b)

m2<-lm(weight~gest*sexo,data=df)
a1<-coef(m2)[[1]]+coef(m2)[[3]]
a2<-coef(m2)[[1]]
b1<-coef(m2)[[2]]
b2<-coef(m2)[[2]]+coef(m2)[[4]]
c(a1,a2,b1,b2)

anova(m1,m2)
