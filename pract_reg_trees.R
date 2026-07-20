#Practice ML
#------------
library(dplyr)
library(rpart)
library(mboost)
library(rpart.plot)
library(ranger)
library(dbarts)
library(caret)
library(dbarts)
library(bartMachine)
library(partykit)
library(party)
library(parttree)
library(tidymodels)
library(MLmetrics)

# setworking directory
setwd("D:/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/pract ml")

# load data
db0<-read.csv("train_qtb.csv")
db1<-db0 %>% select(RES_TRA ,PE_MET,Temp_lam,C:material_c)
head(db1);dim(db1)

# split into train/test
set.seed(2121)
splitt<-initial_split(db1,prop=0.8,strata=RES_TRA)
train<-training(splitt)
test<-testing(splitt)
combined <- bind_rows(
  train %>% mutate(set = "train"),
  test  %>% mutate(set = "test")
)
ggplot(combined, aes(x = RES_TRA, fill = set, color = set)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  theme_minimal() +
  labs(title = "Target density: train vs test", x = "target")

# BASIC CART TREE
#-------------------------------------------------------------------------------------------------------------------
# basic tree
fittb0 = rpart(RES_TRA  ~ material_c + Temp_lam, data = train)
fit_pt = parttree(fittb0)
plot(fit_pt)

# cart
treeb1 <- rpart(RES_TRA  ~ ., data = train, method = "anova")
treeb1
summary(treeb1)
rpart.plot(treeb1)

treeb2 <- rpart(RES_TRA ~ ., data = train, method = "anova",
              control = rpart.control(
                minsplit = 15,   # min obs in a node to attempt a split
                cp = 0.01       # complexity: lower = bigger tree
              ))
treeb2
summary(treeb2)
rpart.plot(treeb2)

### prunning
big_tree <- rpart(RES_TRA ~ ., data = train, method = "anova",
                  control = rpart.control(cp = 0))
printcp(big_tree)
plotcp(big_tree)

# Rule B (1-SE rule): the smallest tree whose xerror is within
# one standard error of the minimum — favors simpler trees
cptab <- big_tree$cptable
min_row <- which.min(cptab[, "xerror"])
threshold <- cptab[min_row, "xerror"] + cptab[min_row, "xstd"]
se_cp <- cptab[cptab[, "xerror"] <= threshold, "CP"][1]

# Prune using your chosen cp
pruned <- prune(big_tree, cp = se_cp)   # or se_cp
rpart.plot(pruned)
printcp(pruned)

# compare metrics
# default treee
dfres<-data.frame(model=c("basico","basico","pruned","pruned"),
                  RMSE=c("train","test","train","test"),
                  rmse=c(RMSE(predict(treeb1),train$RES_TRA),
                         RMSE(predict(treeb1,newdata = test),test$RES_TRA),
                         RMSE(predict(pruned,newdata = train),train$RES_TRA),
                         RMSE(predict(pruned,newdata = test),test$RES_TRA))) %>% 
      mutate(dif=c(0,diff(rmse))) %>% mutate(dif=ifelse(dif>=0,NA,round(dif,2)))

# using caret
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3
)
set.seed(21211)
tree_cp <- train(
  RES_TRA ~ ., 
  data = train,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = expand.grid(cp = seq(0.001, 0.05, by = 0.002)),
  metric = "RMSE"          # regression default; "MAE" also available
)
tree_cp
plot(tree_cp)              # RMSE vs cp
tree_cp$bestTune  
densityplot(tree_cp)
pred <- predict(tree_cp, newdata = test)
postResample(pred = pred, obs = test$RES_TRA)
rpart.plot(tree_cp$finalModel)

getTrainPerf(tree_cp) 
pred <- predict(tree_cp, newdata = test)
postResample(pred = pred, obs = test$RES_TRA)

hist(tree_cp$resample$RMSE,breaks="FD",main="train vs test error")
tsterr<-postResample(pred = pred, obs = test$RES_TRA) %>% as.numeric()
abline(v = tsterr[1], col = "red", lwd = 2)

# BASIC CONDICTIONAL REGRESSION TREE
#-------------------------------------------------------------------------------------------------------------------
ctree_fit <- ctree(RES_TRA ~ ., data = train)
ctree_fit
plot(ctree_fit)
ctree_fit_p <- ctree(RES_TRA ~ ., data = train,
                   control = ctree_control(
                     mincriterion = 0.99,   # 1 - 0.05; split only if p < 0.05
                     minsplit = 20,         # min obs in a node to consider splitting
                     minbucket = 7          # min obs in a terminal leaf
                   ))
plot(ctree_fit_p)
set.seed(21211)
ctree_cv <- train(
  RES_TRA ~ ., 
  data = train,
  method = "ctree",
  trControl = ctrl,
  tuneGrid = expand.grid(mincriterion = seq(0.90, 0.99, by = 0.01)),
  metric = "RMSE"
)
ctree_cv
plot(ctree_cv)
plot(ctree_cv$finalModel)

getTrainPerf(ctree_cv)
pred <- predict(ctree_cv, newdata = test)
postResample(pred = pred, obs = test$RES_TRA)

# RANDOM FOREST 
#-------------------------------------------------------------------------------------------------------------------
rfb0 <- ranger(
  RES_TRA ~ ., 
  data = train,
  num.trees = 500,
  importance = "permutation"   # enables variable importance
)
rfb0
importance(rfb0)
sqrt(rfb0$prediction.error) # OOB RMSE - comparable to test RMSE
RMSE(predict(rfb0, data = test)$predictions ,test$RES_TRA)

grid <- expand.grid(
  mtry = c(2, 3, 4, 6, 8, 10),
  splitrule = "variance",
  min.node.size = c(5, 10, 20)
)
set.seed(21211)
rf_cv <- train(
  RES_TRA ~ ., 
  data = train,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = grid,
  num.trees = 500,
  importance = "permutation",
  metric = "RMSE"
)
rf_cv
varImp(rf_cv)

oob_rmse <- sqrt(rf_cv$finalModel$prediction.error) ## OOB RMSE from the SAME model caret selected (best mtry / min.node.size)
getTrainPerf(rf_cv)
pred <- predict(rf_cv, newdata = test)
postResample(pred = pred, obs = test$RES_TRA)

# cforest
cf <- partykit::cforest(
  RES_TRA ~ .,
  data = train
)
cf
#varimp(cf,conditional=T)
varimp(cf)

grid <- expand.grid(mtry = c(2, 3, 5, 8, 12))
ct_cv <- train(
  RES_TRA ~ .,
  data = train,
  method = "cforest",
  trControl = ctrl,
  tuneGrid = grid,
  controls = party::cforest_unbiased(ntree = 500)  # note: caret's cforest uses 'controls'
)
ct_cv
plot(ct_cv)


## GBM
#------------------------------------------------------------------------------------------------------------------
ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(
  n.trees = c(300, 500, 1000),      # number of boosting iterations
  interaction.depth = c(1, 3, 5),   # tree depth — controls interaction order
  shrinkage = c(0.01, 0.05, 0.1),   # learning rate
  n.minobsinnode = c(5, 10)         # min obs per node
)
set.seed(123)
gbm_cv <- train(
  RES_TRA ~ ., 
  data = train,
  method = "gbm",
  trControl = ctrl,
  tuneGrid = grid,
  metric = "RMSE",
  verbose = FALSE                   # gbm is chatty without this
)
gbm_cv
gbm_cv$bestTune
plot(gbm_cv)

getTrainPerf(gbm_cv)
pred <- predict(gbm_cv, newdata = test)
postResample(pred = pred, obs = test$RES_TRA)

## Conditional GBM
cond_boost <- blackboost(
  RES_TRA ~ ., 
  data = train,
  control = boost_control(mstop = 500, nu = 0.1)   # nu = learning rate
)
pred <- predict(cond_boost, newdata = test)
sqrt(mean((pred - test$RES_TRA)^2))


# BART
#-------------------------------------------------------------------------------------------------------------------
set.seed(2112)
bart_fit <- bart2(
  RES_TRA ~ ., 
  data = train,
  test = test,
  n.trees = 200,      # many SMALL trees — note this is the default
  n.samples = 1000,   # posterior draws to keep
  n.burn = 500        # burn-in draws to discard
)
bart_fit
pred <- bart_fit$yhat.test.mean
sqrt(mean((pred - test$RES_TRA)^2)) 

pred <- apply(bart_fit$yhat.test, 3, mean)                          
ci   <- apply(bart_fit$yhat.test, 3, quantile,
              probs = c(0.025, 0.975))                              
df <- data.frame(
  actual = test$RES_TRA,
  pred   = pred,
  lower  = ci[1, ],
  upper  = ci[2, ]
)
head(df)
dfs<-df %>% slice_sample(n = 30)
plot(dfs$actual,type="l",ylim=c(570,650))
lines(dfs$pred,col="red")
lines(dfs$upper,col="blue")
lines(dfs$lower,col="blue")

## using caret
ctrl <- trainControl(method = "cv", number = 5)   # BART is slow; 5-fold
grid <- expand.grid(
  num_trees = c(50, 200),
  k = c(2, 3),
  alpha = 0.95, beta = 2, nu = 3
)

set.seed(123)
bart_cv <- train(
  RES_TRA ~ ., 
  data = train,
  method = "bartMachine",
  trControl = ctrl,
  tuneGrid = grid,
  metric = "RMSE"
)
bart_cv$finalModel
densityplot(bart_cv)
getTrainPerf(bart_cv)
pred <- predict(bart_cv, newdata = test)
postResample(pred = pred, obs = test$RES_TRA)
check_bart_error_assumptions(bart_cv$finalModel)  


round(calc_credible_intervals(bart_machine_cv, X[100, ],
                               ci_conf = 0.95), 2)
