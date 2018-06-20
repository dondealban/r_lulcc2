
devtools::load_all("lulcc", quiet=TRUE)

data(pie)
lu <- DiscreteLulcRasterStack(x=stack(pie[1:3]),
                               categories=c(1,2,3),
                               labels=c("Forest","Built","Other"),
                               t=c(0,6,14))

crossTabulate(lu, t=c(0,14))

ix <- data.frame(var=c("ef_001","ef_002","ef_003"),
                 yr=c(0,0,0),
                 dynamic=c(F,F,F))

ef <- ExpVarRasterStack(x=stack(pie[4:6]), index=ix)

ef <- resample(ef, lu) ## TODO: write wrapper for resample in lulcc2

part <- partition(x=lu, size=0.1, spatial=TRUE, t=0)

train.data <- getPredictiveModelInputData(lu=lu,
                                          ef=ef,
                                          cells=part[["train"]],
                                          t=0)

test.data  <- getPredictiveModelInputData(lu=lu,
                                          ef=ef,
                                          cells=part[["test"]],
                                          t=0)

## train.data$Built <- as.factor(train.data$Built)
## train.data$Forest <- as.factor(train.data$Forest)
## train.data$Other <- as.factor(train.data$Other)

library(randomForest)
library(rpart)

built.form  <- as.formula("Built ~ ef_001 + ef_002 + ef_003")
## built.rf    <- randomForest(built.form, data=train.data)
built.glm   <- glm(built.form, family=binomial, data=train.data)
## built.rpart <- rpart(built.form, method="class", data=train.data)

forest.form <- as.formula("Forest ~ ef_001 + ef_002")
## forest.rf   <- randomForest(forest.form, data=train.data)
forest.glm  <- glm(forest.form, data=train.data)

other.form  <- as.formula("Other ~ ef_001 + ef_002")
## other.rf    <- randomForest(other.form, data=train.data)
other.glm   <- glm(forest.glm, data=train.data)

## rf.mods  <- new("PredictiveModelList",
##                 models=list(forest.rf, built.rf, other.rf),
##                 categories=lu@categories,
##                 labels=lu@labels)

glm.mods <- PredictiveModelList(models=list(forest.glm, built.glm, other.glm),
                                categories=lu@categories,
                                labels=lu@labels)

## rf.pred  <- PredictionList(models=rf.mods, newdata=test.data)
## rf.perf  <- PerformanceList(pred=rf.pred, measure="rch")

glm.pred <- PredictionList(models=glm.mods, newdata=test.data)
glm.perf <- PerformanceList(pred=glm.pred, measure="rch")

dmd <- approxExtrapDemand(lu, tout=0:14)
matplot(dmd, type="l", ylim=c(0,50000), lty=1, col=c("magenta","blue","green"), ylab="Demand (No. cells)", xlab="Time", xlim=c(1,15), axes=FALSE)
box()
axis(2)
axis(1, at=seq(1,15,by=2), labels=seq(0,14,2))
legend("bottomright", legend=c("Forest","Built","Other"), lty=1, col=c("magenta","blue","green"))

w <- matrix(data=1, nrow=3, ncol=3)
nb <- NeighbRasterStack(x=lu[[1]], weights=w, categories=c(1,2,3))

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

## ordered.model <- OrderedModel(observed.lulc=lu, 
##                               explanatory.variables=ef, 
##                               predictive.models=glm.mods, 
##                               time=0:14, 
##                               demand=dmd,                              
##                               transition.rules=matrix(data=1, nrow=3, ncol=3),                              
##                               order=c(2,1,3)) 
## ordered.result <- allocate(ordered.model, stochastic=TRUE)

## ## ordered
## ordered.tabs <- ThreeMapComparison(x=lu[[1]],
##                                    x1=lu[[3]],
##                                    y1=ordered.result[[15]],
##                                    factors=2^(1:8), 
##                                    categories=lu@categories,
##                                    labels=lu@labels) 

## ordered.agr  <- AgreementBudget(x=ordered.tabs) 
## ordered.fom  <- FigureOfMerit(x=ordered.tabs) 

## plot(ordered.agr, from=1, to=2) 
## plot(ordered.fom, from=1, to=2) 

## CLUE-S
clues.tabs <- ThreeMapComparison(x=lu[[1]],
                                 x1=lu[[3]],
                                 y1=clues.result[[15]],
                                 factors=2^(1:8),
                                 categories=lu@categories,
                                 labels=lu@labels)

## ## plot three dimensional tables in different ways (figures not shown in paper)
## plot(clues.tabs)
## plot(clues.tabs, category=1, factors=2^(1:8)[c(1,3,5,7)])

## ## Ordered
## ordered.tabs <- ThreeMapComparison(x=ordered.model,
##                                    factors=2^(1:8),
##                                    timestep=14)

## ## as above for CLUE-S model
## plot(ordered.tabs)
## plot(ordered.tabs, category=1, factors=2^(1:9)[c(1,3,5,7)])

## calculate agreement budget and plot

## CLUE-S
clues.agr <- AgreementBudget(x=clues.tabs)
p1 <- plot(clues.agr,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,0.08),
           scales=list(y=list(at=c(seq(from=0,to=0.08,by=0.02)), cex=0.6, tck=0.6), x=list(cex=0.6, tck=0.6)),
           key=list(cex=0.6))
## p1

## Ordered
## ordered.agr <- AgreementBudget(x=ordered.tabs)
p2 <- plot(ordered.agr,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,0.08),
           scales=list(y=list(at=c(seq(from=0,to=0.08,by=0.02)), cex=0.6, tck=0.6), x=list(cex=0.6, tck=0.6)),
           key=list(cex=0.6))
## p2

## figure 7
agr.p <- c("CLUE-S"=p1, Ordered=p2, layout=c(1,2))
agr.p

## CLUE-S
clues.fom <- FigureOfMerit(x=clues.tabs)
p1 <- plot(clues.fom,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,1),
           scales=list(y=list(at=(seq(from=0,to=1,by=0.2)), cex=0.6), x=list(cex=0.6)),
           key=NULL)
p1

## Ordered
## ordered.fom <- FigureOfMerit(x=ordered.tabs)
p2 <- plot(ordered.fom,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,1),
           scales=list(y=list(at=(seq(from=0,to=1,by=0.2)), cex=0.6), x=list(cex=0.6)),
           key=NULL)
p2

fom.p <- c("CLUE-S"=p1, Ordered=p2, layout=c(1,2))
fom.p
