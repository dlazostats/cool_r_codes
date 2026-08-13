library(ggplot2)
library(psych)
library(forcats)
library(hexbin)
library(ggpointdensity)


# setworking directory
setwd("D:/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/pract ml")

# load data
db0<-read.csv("train_qtb.csv") %>% filter(Temp_lam>550 & Temp_lam<720)
db1<-db0 %>% select(RES_TRA ,PE_MET,Temp_lam,C:material_c)
head(db1);dim(db1)
samp1<-db0 %>% 
       slice_sample(n=200)
pairs.panels(samp1)

db0 %>% 
  mutate(f1 = cut_number(Temp_lam, n = 4)) %>% 
  ggplot(aes(x=S,y=P,color=f1))+
    geom_point()+
    theme_bw()

db0 %>% 
  mutate(f1 = case_when(
    grepl("CALIENTE", Tipo) ~ "CALIENTE",
    grepl("FRIA", Tipo)     ~ "FRIA",
    TRUE                    ~ "OTRO"
  )) %>% 
  ggplot(aes(x=S,y=P,color=f1))+
  geom_point()+
  theme_bw()

db0 %>% 
  mutate(f1 = cut_number(FLUENCIA, n = 4)) %>% 
  ggplot(aes(x=S,y=P,color=f1))+
  geom_point()+
  theme_bw()  

db0 %>% 
  mutate(f1 = cut_number(PE_MET, n = 4)) %>% 
  ggplot(aes(x=S,y=P,color=f1))+
  geom_point()+
  theme_bw()  

db0 %>%
  mutate(f1 = fct_lump_min(
    factor(MAKTX_PAC), 
    min = 100, 
    other_level = "Otros"
  )) %>% 
  ggplot(aes(x=S,y=P,color=f1))+
  geom_point()+
  theme_bw()  

db0 %>%
  mutate(f1 = fct_lump_min(
    factor(MATNR), 
    min = 82, 
    other_level = "Otros"
  )) %>% 
  ggplot(aes(x=S,y=P,color=f1))+
  geom_point()+
  theme_bw()  

db0 %>%
    ggplot(aes(x=S,y=P))+
    geom_hex(bins = 30) +
    scale_fill_viridis_c() +  # Applies a clean, readable color palette
    theme_minimal()

bin <- hexbin(db0$S, db0$P, xbins = 30)
plot(bin, main = "Hexbin Plot", xlab = "S", ylab = "P")

db0 %>%
  ggplot(aes(x=S,y=P))+
  geom_density_2d_filled(bins = 10) +
  theme_minimal()

db0 %>%
  ggplot(aes(x = S, y = P)) +
  geom_pointdensity(adjust = 0.1) +
  scale_color_viridis_c() +
  theme_minimal()
