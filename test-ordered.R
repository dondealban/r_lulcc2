
devtools::load_all("/home/simon/projects/r_lulcc2/lulcc", quiet=TRUE)

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

ordered.model <- OrderedModel(observed.lulc=lu, 
                              explanatory.variables=ef, 
                              predictive.models=glm.mods, 
                              time=0:14, 
                              demand=dmd,                              
                              transition.rules=matrix(data=1, nrow=3, ncol=3),                              
                              order=c(2,1,3)) 

ordered.result <- orderedFun(ordered.model, stochastic=TRUE)

model=ordered.model

sourceCpp("ordered.cpp")

orderedfun = function(model, ...) {
    
    t0 <- model@time[1]
    lu0 <- model@observed.lulc[[which(model@observed.lulc@t %in% t0)]]
    lu0 <- as(lu0, "RasterLayer")
    cells <- which(complete.cases(raster::getValues(lu0)))
    lu0.vals <- extract(lu0, cells)
    newdata <- as.data.frame(x=model@explanatory.variables, cells=cells, t=t0)
    prob <- predict(object=model@predictive.models, newdata=newdata)

    if (!is.null(model@history)) {
        hist.vals <- raster::extract(model@history, cells)
    } else {
        hist.vals <- NULL
    }

    if (!is.null(model@mask)) {
        mask.vals <- raster::extract(model@mask, cells)
    } else {
        mask.vals <- NULL
    }

    ncell <- length(cells)
    ncode <- length(model@categories)
    nt <- length(model@time)
    any.dynamic <- any(model@explanatory.variables@index[,3])

    maps <- vector(mode="list", length=nt)
    maps[[1]] <- lu0

    for (i in 2:nt) {

        t1 <- model@time[i]
        d0 = model@demand[i-1,,drop=TRUE]
        d1 = model@demand[i,,drop=TRUE]
        d = d1 - d0

        if (any.dynamic && i > 2) {
            newdata <- updateDataFrame(x=model@explanatory.variables, y=newdata, cells=cells, time=t1)
            prob <- predict(object=model@predictive.models, newdata=newdata)
        }
        tprob <- prob

        lu1.vals = orderedCpp(lu0.vals, tprob, d1, model@order, model@categories, FALSE, 1000000)
        
        ## lu1.vals <- ordered(lu0=lu0,
        ##                     lu0.vals=lu0.vals,
        ##                     tprob=tprob,
        ##                     nb=model@neighbourhood,
        ##                     nb.rules=model@neighbourhood.rules,
        ##                     transition.rules=model@transition.rules,
        ##                     hist.vals=hist.vals,
        ##                     mask.vals=mask.vals,
        ##                     demand=model@demand[c(i-1,i),,drop=FALSE],
        ##                     categories=model@categories,
        ##                     order=model@order,
        ##                     stochastic=stochastic)

        lu1 <- raster::raster(lu0, ...) 
        lu1[cells] <- lu1.vals
        maps[[i]] <- lu1

        if (i < nt) {

            if (!is.null(hist.vals)) {
                hist.vals <- .updatehist(lu0.vals, lu1.vals, hist.vals)
            }

            lu0 <- lu1
            lu0.vals <- lu1.vals 
        }
    }

    DiscreteLulcRasterStack(x=raster::stack(maps),
                            categories=model@categories,
                            labels=model@labels,
                            t=model@time)
}

