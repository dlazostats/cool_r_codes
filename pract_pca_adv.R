library(dplyr)
library(ggplot2)
library(ggrepel)   
library(factoextra)

# set working directory
script_name <- 'pract_pca.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
db0<-read.csv("dftrain.csv")
set.seed(2121)
db0_sqmple<-db0 %>% 
  slice_sample(n = 1500)%>% 
  dplyr::select(C:Pb)
