library(tidyverse)
library(readr)
#install.packages("e1071")
library(e1071)
library(randomForest)
# install.packages("earth")
library(earth)
library(caret)
library(vip)
library(pdp)

boston <- read_csv("boston.csv")

boston.adj <- boston %>% 
  mutate(id = 1:nrow(boston))

summary(boston.adj)

pairs(boston.adj)

correlationMatrix <- cor(boston)
correlationMatrix

set.seed(497562)
train <- boston.adj %>% sample_frac(0.7)
test <- anti_join(boston.adj, train, by = "id")

summary(train$MEDV)
summary(test$MEDV)

svmfit = svm(MEDV~.-id, data = train, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

#plot(svmfit, train)

RFModel <- randomForest(MEDV~.-id, data=train, importance=T, proximity=T) 
print(RFModel)

RFModel2 <- randomForest(MEDV~NOX, data=train, importance=T, proximity=T) 
print(RFModel2)

LMreg <- lm(MEDV ~ LSTAT, data = train)
print(LMreg)
summary(LMreg)
plot(train$LSTAT, train$MEDV)
ggplot(data = train, aes(x = LSTAT, y = MEDV)) + geom_point() + geom_smooth(method = "lm", se = F)

LMreglog <- lm(MEDV ~ log(LSTAT), data = train)
print(LMreglog)
summary(LMreglog)
plot(log(train$LSTAT), train$MEDV)
ggplot(data = train, aes(x = log(LSTAT), y = MEDV)) + geom_point() + geom_smooth(method = "lm", se = F)

LMfull <- lm(MEDV ~ .-id-INDUS-AGE-TAX, data = train)
print(LMfull)
summary(LMfull)

LMsmall2 <- lm(MEDV ~ LSTAT + RM, data = train)
print(LMsmall2)
summary(LMsmall2)
ggplot(data = train, aes(x = LSTAT, y = MEDV)) + geom_point(aes(color = RM)) + geom_smooth(method = "lm", se = F) + facet_wrap(vars(CHAS))
summary(train)


ggplot(data = train) + geom_histogram(aes(x = PTRATIO))
ggplot(data = train) + geom_histogram(aes(x = B))
ggplot(data = train) + geom_histogram(aes(x = NOX))
ggplot(data = train) + geom_histogram(aes(x = log(DIS)))

LSTAT, RM, PTRATIO, DIS, B, NOX, ZN
PTRATIO, B, 

train.adj <- train
train.adj$DIS.group <- cut(log(train.adj$DIS), 3)
train.adj <- train.adj %>% 
  mutate(DIS.group2 = as.factor(DIS.group),
         DIS.group2 = if_else(DIS.group2 == "(0.12,0.872]", "first", 
                              if_else(DIS.group2 == "(0.872,1.62]", "second", "third")))
train.adj$PTR.group <- cut(train.adj$PTRATIO, 3)
train.adj <- train.adj %>% 
  mutate(PTR.group2 = as.factor(PTR.group),
         PTR.group2 = if_else(PTR.group2 == "(12.6,15.7]", "first", 
                              if_else(PTR.group2 == "(15.7,18.9]", "second", "third")))

ggplot(data = train.adj, aes(x = LSTAT, y = MEDV)) + 
  geom_point(aes(color = RM, shape = DIS.group2, size = NOX, alpha = PTRATIO)) + 
  geom_smooth(aes(group = DIS.group2, linetype = DIS.group2), method = "lm", se = F) + 
  facet_wrap(vars(CHAS, PTR.group)) +
  scale_color_distiller(palette = "Reds")



train.scale <- bind_cols(scale(train[1:13]), train[14:15])
summary(train.scale)

process <- preProcess(as.data.frame(train), method=c("range"))
train.norm <- bind_cols(predict(process, as.data.frame(train)) %>% select(-c(MEDV, id)), train[14:15])

LMnorm <- lm(pred ~ .-id-INDUS-AGE-TAX-MEDV, data = train.pred.norm)
print(LMnorm)
summary(LMnorm)

p1.norm <- vip(LMnorm, num_features = 40, geom = "point", value = "gcv") + ggtitle("GCV")
p2.norm <- vip(LMnorm, num_features = 40, geom = "point", value = "rss") + ggtitle("RSS")
gridExtra::grid.arrange(p1.norm, p2.norm, ncol = 2)


beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho

train.pred <- train %>% mutate(pred = predict(RFModel, train))
test.pred <- test %>% mutate(pred = predict(RFModel, test))

ggplot(data = train.pred, aes(x = MEDV, y = pred)) + geom_point() + geom_line(aes(x = MEDV, y = MEDV))
cor(train.pred$MEDV, train.pred$pred)
ggplot(data = test.pred, aes(x = MEDV, y = pred)) + geom_point() + geom_line(aes(x = MEDV, y = MEDV))
cor(test.pred$MEDV, test.pred$pred)

predFull <- bind_rows(train.pred, test.pred)
ggplot(data = predFull, aes(x = MEDV, y = pred)) + geom_point() + geom_line(aes(x = MEDV, y = MEDV))
cor(predFull$MEDV, predFull$pred)

surrModel1 <- lm(pred ~ .-id-MEDV, data = test.pred)
surrModel2 <- lm(pred ~ .-id-MEDV, data = train.pred)
summary(surrModel1)
summary(surrModel2)

surrModelFull <- lm(pred ~ .-id-MEDV-INDUS-AGE-TAX, data = predFull)
summary(surrModelFull)

surrModelFull.simple <- lm(pred ~ LSTAT, data = predFull)
summary(surrModelFull.simple)
ggplot(data = predFull, aes(x = LSTAT, y = pred)) + geom_point() + geom_smooth(method = "lm", se = F)

predFull <- predFull %>% 
  mutate(lmPred = predict(surrModelFull, predFull),
         lmPred.simple = predict(surrModelFull.simple, predFull))

ggplot(data = predFull, aes(x = MEDV, y = lmPred)) + geom_point() + geom_line(aes(x = MEDV, y = MEDV))
ggplot(data = predFull, aes(x = MEDV, y = lmPred.simple)) + geom_point() + geom_line(aes(x = MEDV, y = MEDV))


hyper_grid <- expand.grid(degree = 1:3,
                          nprune = seq(2, 50, length.out = 10) %>%
                            floor())

set.seed(94856)
marsModel <- train(
  x = subset(predFull, select = -c(id, pred, MEDV, lmPred, lmPred.simple)),
  y = predFull$pred,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid)

marsModel$results %>%
  filter(nprune==marsModel$bestTune$nprune, degree==marsModel$bestTune$degree)

ggplot(marsModel)

p1 <- vip(marsModel, num_features = 40, geom = "point", value = "gcv") + ggtitle("GCV")
p2 <- vip(marsModel, num_features = 40, geom = "point", value = "rss") + ggtitle("RSS")

gridExtra::grid.arrange(p1, p2, ncol = 2)

predFull <- predFull %>% 
  mutate(marsPred = predict(marsModel, predFull))

ggplot(data = predFull, aes(x = MEDV, y = marsPred)) + geom_point() + geom_line(aes(x = MEDV, y = MEDV))
ggplot(data = predFull, aes(x = LSTAT, y = marsPred)) + geom_point()
ggplot(data = predFull, aes(x = RM, y = marsPred)) + geom_point()

marsModel.simple <- train(
  x = subset(predFull, select = LSTAT),
  y = predFull$pred,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid)

predFull <- predFull %>% 
  mutate(marsPred.simple = predict(marsModel.simple, predFull))

ggplot(data = predFull, aes(x = MEDV, y = marsPred.simple)) + geom_point() + geom_line(aes(x = MEDV, y = MEDV))
ggplot(data = predFull, aes(x = LSTAT, y = marsPred.simple)) + geom_point()
ggplot(data = predFull, aes(x = LSTAT, y = MEDV)) + geom_point()

marsModel.simple2 <- train(
  x = subset(predFull, select = RM),
  y = predFull$pred,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid)

predFull <- predFull %>% 
  mutate(marsPred.simple2 = predict(marsModel.simple2, predFull))
ggplot(data = predFull, aes(x = RM, y = marsPred.simple2)) + geom_point()


library(rms)
checktrain <- train %>% select(-c(id,INDUS,AGE,TAX))
# OLSfull <- ols(formula = MEDV~.-id-INDUS-AGE-TAX, data = train)
OLSfull <- ols(formula = MEDV~., data = checktrain)
OLSfull$coefficients

ddist <- datadist(checktrain)
options(datadist = "ddist")
plot(nomogram(OLSfull))

OLSsegment <- ols(MEDV~LSTAT, data = train)
OLSsegment$coefficients
ddist <- datadist(train)
options(datadist = "ddist")
plot(nomogram(OLSsegment))
nom <- nomogram(OLSsegment)
OLSsegment

OLSsegment2 <- ols(MEDV~LSTAT + RM, data = train)
plot(nomogram(OLSsegment2))

splineTest = ols(MEDV~lsp(LSTAT, 6), data = train)
splineTest2 = ols(MEDV~lsp(LSTAT, 6) + RM, data = train)
splineTest
splineTest2
plot(nomogram(splineTest))
plot(nomogram(splineTest2))

#Looking for spline points to add
ggplot(data = train, aes(x = CRIM, y = MEDV)) + geom_point() + geom_smooth(method = "lm", se = F)


#####THIS ONE IS REALLY EXCITING!!!
##IT'S SCALED BY IMPORTANCE
OLSfull.norm <- ols(pred~., data = train.pred.norm %>% select(-c(id,INDUS,AGE,TAX,MEDV)))
OLSfull.norm$coefficients
ddist <- datadist(train.pred.norm %>% select(-c(id,INDUS,AGE,TAX,MEDV)))
options(datadist = "ddist")
plot(nomogram(OLSfull.norm))
OLSfull.norm

# p1.olsnorm <- vip(OLSfull.norm, num_features = 20, geom = "point", value = "gcv") + ggtitle("GCV")
# p2.olsnorm <- vip(OLSfull.norm, num_features = 20, geom = "point", value = "rss") + ggtitle("RSS")
# gridExtra::grid.arrange(p1.olsnorm, p2.olsnorm, ncol = 2)




process.pred <- preProcess(as.data.frame(train.pred), method=c("range"))
train.pred.norm <- bind_cols(predict(process.pred, as.data.frame(train.pred)) %>% select(-c(MEDV, id, pred)), train.pred[14:16])

marsModel.norm <- train(
  x = subset(train.pred.norm, select = -c(id,INDUS,AGE,TAX, MEDV, pred)),
  y = train.pred.norm$pred,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid)

marsModel.norm$results %>%
  filter(nprune==marsModel.norm$bestTune$nprune, degree==marsModel.norm$bestTune$degree)

ggplot(marsModel.norm)

p1.marsnorm <- vip(marsModel.norm, num_features = 40, geom = "point", value = "gcv") + ggtitle("GCV")
p2.marsnorm <- vip(marsModel.norm, num_features = 40, geom = "point", value = "rss") + ggtitle("RSS")

gridExtra::grid.arrange(p1.marsnorm, p2.marsnorm, ncol = 2)

marsModel.norm$modelInfo
marsModel.norm$finalModel

marsNormCoeffs <- summary(marsModel.norm)$coefficients

ggplot(train.pred.norm) + geom_point(aes(x=CRIM, y = pred)) + geom_abline(slope = -37.035513*(0.0549838-0.00000027461091), intercept = 9.145261)
ggplot(train.pred.norm) + geom_point(aes(x=RM, y = pred))
ggplot(train.pred.norm) + geom_point(aes(x=B, y = pred))
ggplot(train.pred.norm) + geom_point(aes(x=PTRATIO, y = pred))
ggplot(train.pred.norm) + geom_point(aes(x=ZN, y = pred))
ggplot(train.pred.norm) + geom_point(aes(x=CHAS, y = pred))
ggplot(train.pred.norm) + geom_point(aes(x=DIS, y = pred))

# THIS IS FOR THE EXPRESSIONS FOR THE ACTUAL WRITTEN OUT MARS FUNCTION
terms <- marsModel.norm$finalModel$coefficients %>%
  as.data.frame() %>%
  mutate( parameter = row.names(.)) %>%
  select( parameter, coef = y ) %>% 
  mutate(spline = as.numeric(str_extract(parameter, "\\d*\\.\\d*")),
         var = str_extract(parameter, "[A-Z]+"),
         var = if_else(var == "I", "Intercept", var),
         side = if_else(str_detect(parameter, "h\\([0-9]"), "left",
                        if_else(str_detect(parameter, "h\\([A-Z]"),"right","")),
         weight = if_else(side == "left", spline, 
                          if_else(side == "right", 1-spline, 0)),
         weight = if_else(var == "DIS" & weight == 0.1347920, 0.0838145, weight),
         weight = if_else(var == "CRIM" & weight == 0.9450162, 0.1653472, weight),
         splineWeight = coef*weight)
rownames(terms) <- NULL

ggplot(terms, aes(x = var, y = splineWeight)) + 
  geom_segment(aes(x=var, xend=var, y=0, yend=splineWeight)) + 
  geom_point(color = "coral", size = 3) + coord_flip() + theme_minimal()
ggplot(terms, aes(x = var, y = coef)) + 
  geom_segment(aes(x=var, xend=var, y=0, yend=coef)) + 
  geom_point(color = "coral", size = 3) + geom_hline(aes(yintercept = 0)) +  coord_flip() + theme_minimal()


equatiomatic::extract_eq(marsModel.norm$finalModel)

#THIS IS ALSO IMPORTANT
library(plotmo)
imp = varImp(marsModel.norm) 
imp = imp$importance %>%
  as.data.frame() %>%
  mutate( variable = row.names(.) ) %>%
  filter( Overall > 0 )

plotmo(marsModel.norm, all1 = T) 
