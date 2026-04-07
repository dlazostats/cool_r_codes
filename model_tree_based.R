library(partykit)
library(ppsr)
data("TeachingRatings", package = "AER")
tr <- subset(TeachingRatings, credits == "more")
tr_tree <- lmtree(eval ~ beauty | minority + age + gender + division +
                    native + tenure, data = tr, weights = students, caseweights = FALSE)
plot(tr_tree)

# set working directory
script_name <- 'model_tree_based.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

dt<-read.csv("dftrain.csv")

tr_tree <- lmtree(Fluencia ~ C | PesoMetrico+Mn+P+S, 
                  data = dt, weights = material_c, caseweights = FALSE,minsize = 50)
plot(tr_tree)


tr_tree <- lmtree(Fluencia ~ C | PesoMetrico+Mn+P+S, 
                  data = dt, weights = material_c, caseweights = FALSE,maxdepth = 4)
plot(tr_tree)

dt2<-dt %>% select(-Resistencia,-Relacion,-Alarga,-material)
ppsr::score_predictors(df = dt2, y = 'Fluencia') %>% arrange(desc(pps))

tr_tree <- lmtree(Fluencia ~ C | TempLam+V+PesoMetrico+Mn+P+Si, 
                  data = dt, weights = material_c, caseweights = FALSE,maxdepth = 4)
plot(tr_tree)
