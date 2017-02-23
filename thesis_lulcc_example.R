
devtools::load_all("lulcc2")

## Not run:

## Plum Island Ecosystems

data(pie)

## Observed maps
lu <- DiscreteLulcRasterStack(x=stack(pie[1:3]),
                              categories=c(1,2,3),
                              labels=c("Forest","Built","Other"),
                              t=c(0,6,14))
plot(lu)

crossTabulate(x=lu, times=c(0,14))

## Explanatory variables
idx <- data.frame(var=c("ef_001","ef_002","ef_003"),
                  yr=c(0,0,0),
                  dynamic=c(FALSE,FALSE,FALSE))
idx

ef <- ExpVarRasterStack(x=stack(pie[4:6]), index=idx)

part <- partition(x=lu, size=0.1, spatial=TRUE, t=0)
train.data <- getPredictiveModelInputData(lu=lu,
                                          ef=ef,
                                          cells=part[["train"]],
                                          t=0)

## predictive modelling
forest.form <- as.formula("Forest ~ ef_001 + ef_002")
built.form <- as.formula("Built ~ ef_001 + ef_002 + ef_003")
other.form <- as.formula("Other ~ ef_001 + ef_002")

library(randomForest)
library(rpart)

forest.glm <- glm(forest.form, family=binomial, data=train.data)
forest.rprt <- rpart(forest.form, data=train.data)
forest.rf <- randomForest(forest.form, method="class", data=train.data)

built.glm <- glm(built.form, family=binomial, data=train.data)
built.rprt <- rpart(built.form, data=train.data)
built.rf <- randomForest(built.form, method="class", data=train.data)

other.glm <- glm(other.form, family=binomial, data=train.data)
other.rprt <- rpart(other.form, data=train.data)
other.rf <- randomForest(other.form, method="class", data=train.data)

## Binomial logistic regression
glm.mods <- PredictiveModelList(list(forest.glm, built.glm, other.glm),
                                  categories=lu@categories,
                                  labels=lu@labels)

## Recursive partitioning and regression trees
rprt.mods <- PredictiveModelList(list(forest.rprt, built.rprt, other.rprt),
                                 categories=lu@categories,
                                 labels=lu@labels)

## Random forests
rf.mods <- PredictiveModelList(list(forest.rf, built.rf, other.rf),
                               categories=lu@categories,
                               labels=lu@labels)

test.data <- getPredictiveModelInputData(lu=lu,
                                         ef=ef,
                                         cells=part[["test"]],
                                         t=0)

glm.pred <- PredictionList(models=glm.mods, newdata=test.data) 
glm.perf <- PerformanceList(pred=glm.pred, measure="rch") 

rprt.pred <- PredictionList(models=rprt.mods, newdata=test.data) 
rprt.perf <- PerformanceList(pred=rprt.pred, measure="rch") 

rf.pred <- PredictionList(models=rf.mods, newdata=test.data) 
rf.perf <- PerformanceList(pred=rf.pred, measure="rch") 

p <- plot(list(glm=glm.perf, rpart=rprt.perf, rf=rf.perf))

## Probability maps
all.data <- as.data.frame(x=ef, cells=part[["all"]]) 
probmaps <- predict(object=glm.mods, 
                    newdata=all.data, 
                    data.frame=TRUE) 

points <- rasterToPoints(lu[[1]], spatial=TRUE) 
probmaps <- SpatialPointsDataFrame(points, probmaps) 
probmaps <- rasterize(x=probmaps, y=lu[[1]], 
                      field=names(probmaps)) 

p <- levelplot(probmaps, layout=c(2,2), margin=FALSE)

## Demand scenario
dmd <- approxExtrapDemand(lu=lu, tout=0:14)

## CLUE-S modelling
clues.model <- CluesModel(observed.lulc=lu, 
                          explanatory.variables=ef,
                          predictive.models=glm.mods,
                          time=0:14,
                          demand=dmd,
                          history=NULL,
                          mask=NULL,
                          neighbourhood=NULL,
                          transition.rules=matrix(data=1, nrow=3, ncol=3),
                          neighbourhood.rules=NULL,
                          elasticity=c(0.2,0.2,0.2),
                          iteration.factor=0.00001,
                          max.iteration=1000,
                          max.difference=5,
                          ave.difference=5)

clues.result <- allocate(clues.model)

## Ordered modelling
ordered.model <- OrderedModel(observed.lulc=lu, 
                              explanatory.variables=ef,
                              predictive.models=glm.mods,
                              time=0:14,
                              demand=dmd,
                              transition.rules=matrix(data=1, 3, 3),
                              order=c(2,1,3))

ordered.result <- allocate(ordered.model, stochastic=FALSE)

## Validation

clues.tabs <- ThreeMapComparison(x=lu[[1]],
                                   x1=lu[[3]],
                                   y1=clues.result[[15]],
                                   factors=2^(1:8), 
                                   categories=lu@categories,
                                   labels=lu@labels) 

clues.agr <- AgreementBudget(x=clues.tabs) 
clues.fom <- FigureOfMerit(x=clues.tabs) 
ordered.tabs <- ThreeMapComparison(x=lu[[1]],
                                   x1=lu[[3]],
                                   y1=ordered.result[[15]],
                                   factors=2^(1:8),
                                   categories=lu@categories,
                                   labels=lu@labels)
                                 
ordered.agr <- AgreementBudget(x=ordered.tabs)
ordered.fom <- FigureOfMerit(x=ordered.tabs)

p1 <- plot(clues.agr, from=1, to=2)
p2 <- plot(ordered.agr, from=1, to=2)

agr.p <- c("CLUE-S"=p1, Ordered=p2, layout=c(1,2))
agr.p

p1 <- plot(clues.fom, from=1, to=2)
p2 <- plot(ordered.fom, from=1, to=2)

fom.p <- c("CLUE-S"=p1, Ordered=p2, layout=c(1,2))
fom.p
