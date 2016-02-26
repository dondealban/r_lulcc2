#' @include class-Model.R
NULL

#' Allocate land use change spatially
#'
#' Perform spatially explicit allocation of land use change using different
#' models. Currently the function provides an implementation of the Change in
#' Land Use and its Effects at Small regional extent (CLUE-S) model (Verburg et
#' al., 2002) and an ordered procedure based on the algorithm described by Fuchs
#' et al., (2013), modified to allow stochastic transitions.
#'
#' @param model an object inheriting from class \code{Model}
#' @param \dots additional arguments for specific methods
#'
#' @seealso \code{\link{CluesModel}}
#' @return LulcRasterStack.
#' @export
#' @rdname allocate
#'
#' @references
#' Fuchs, R., Herold, M., Verburg, P.H., and Clevers, J.G.P.W. (2013). A
#' high-resolution and harmonized model approach for reconstructing and analysing
#' historic land changes in Europe, Biogeosciences, 10:1543-1559.
#'
#' Verburg, P.H., Soepboer, W., Veldkamp, A., Limpiada, R., Espaldon, V., Mastura,
#' S.S. (2002). Modeling the spatial dynamics of regional land use: the CLUE-S
#' model. Environmental management, 30(3):391-405.
#'
#' @examples
#'
#' ## see lulcc-package examples

setGeneric("allocate", function(model, ...)
           standardGeneric("allocate"))

#' @rdname allocate
#' @aliases allocate,CluesModel-method
setMethod("allocate", signature(model = "CluesModel"),
          function(model, ...) {

              t0 <- model@time[1]
              lu0 <- model@observed.lulc[[(model@observed.lulc@time %in% t0)]]
              lu0 <- as(lu0, "RasterLayer")
              cells <- complete.cases(raster::getValues(lu0))
              lu0.vals.vals <- extract(lu0, cells)
              newdata <- as.data.frame(x=model@explanatory.factor, cells=cells, t=t0)
              prob <- predict(object=model@predictive.models, newdata=newdata)

              if (!is.null(model@hist)) {
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
                  d <- model@demand[i,]

                  if (any.dynamic && i > 2) {
                      newdata <- updateDataFrame(x=model@explanatory.variables, y=newdata, cells=cells, time=t1)
                      prob <- predict(object=model@predictive.models, newdata=newdata)
                  }
                  tprob <- prob

                  for (j in 1:ncode) {
                      ix <- lu0.vals %in% model@categories[j]
                      tprob[ix,j] <- tprob[ix,j] + model@elasticity[j]
                  }

                  tprob <- .applyNeighbDecisionRules(model=model, x=lu0, tprob=tprob)
                  change.direction <- d - model@demand[(i-1),]
                  tprob <- .applyDecisionRules(model=model, x=lu0.vals, hist=hist.vals, cd=change.direction, tprob=tprob)
                  auto  <- .autoConvert(x=lu0.vals, prob=tprob, categories=model@categories, mask=mask.vals)
                  lu0.vals[auto$ix] <- auto$vals
                  tprob[auto$ix,] <- NA

                  lu1.vals <- .clues(tprob=tprob,
                                     lu0.vals=lu0.vals,
                                     demand=d,
                                     categories=model@categories,
                                     scale.factor=model@scale.factor,
                                     max.iteration=model@max.iteration,
                                     max.difference=model@max.difference,
                                     average.difference=model@average.difference)

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

              raster::stack(maps)
              ## model@output <- raster::stack(maps)
              ## model     

          }
          )
 
#' @rdname allocate
#' @aliases allocate,ClueModel-method
setMethod("allocate", signature(model = "ClueModel"),
          function(model, ...) {

              t0 <- model@time[1]
              lu0 <- model@observed.lulc[[(model@observed.lulc@t %in% t0)]]
              lu0 <- as(lu0, "RasterStack")
              cells <- which(complete.cases(raster::getValues(lu0)))
              cells <- xyFromCell(lu0, cells, spatial=TRUE) ######
              
              lu0.vals <- extract(lu0, cells)
              newdata <- as.data.frame(x=model@explanatory.variables, cells=cells, t=t0)
              regr <- predict(object=model@predictive.models, newdata=newdata)

              ncell <- length(cells)
              ncode <- length(model@categories)
              nt <- length(model@time)
              any.dynamic <- any(model@explanatory.variables@index[,3])
              
              maps <- vector(mode="list", length=nt)
              maps[[1]] <- lu0

              for (i in 2:nt) {

                  t1 <- model@time[i]; print(t1)
                  d <- model@demand[i,]

                  if (any.dynamic && (i > 1)) {
                      ## newdata <- updateDataFrame(x=model@explanatory.variables, y=newdata, cells=cells, time=model@time[(i+1)])
                      newdata <- updateDataFrame(x=model@explanatory.variables, y=newdata, cells=cells, time=model@time[i])
                      regr <- predict(object=model@predictive.models, newdata=newdata)
                  }

                  ## do something here to allow for fill?
                  if (!all(model@labels %in% colnames(regr))) {
                      regr1 <- matrix(data=0.5, nrow=nrow(regr), ncol=length(model@labels))
                      colnames(regr1) <- model@labels
                      ix <- match(colnames(regr), model@labels)
                      regr1[,ix] <- regr
                      regr <- regr1
                  }   

                  lu1.vals <- .clue(regr=regr,
                                    lu0.vals=lu0.vals,
                                    demand=d,
                                    elasticity=model@elasticity,
                                    change.rule=model@change.rule,
                                    min.elasticity=model@min.elasticity,
                                    max.elasticity=model@max.elasticity,
                                    min.change=model@min.change,
                                    max.change=model@max.change,
                                    min.value=model@min.value,
                                    max.value=model@max.value,
                                    max.iteration=model@max.iteration,
                                    max.difference=model@max.difference,
                                    cell.area=model@cell.area,
                                    ncell=ncell,
                                    ncode=ncode)

                  lu1 <- lu0
                  lu1[cells] <- lu1.vals
                  maps[[i]] <- lu1

                  if (i < nt) {
                      lu0.vals <- matrix(data=lu1.vals, nrow=ncell)
                  }
              }

              ContinuousLulcRasterStack(x=raster::stack(maps),
                                        labels=model@labels,
                                        t=model@time)
              ## model@output <- ContinuousLulcRasterStack(x=raster::stack(maps),
              ##                                           labels=model@labels,
              ##                                           t=model@time)
              ## model
              
          }
)

#' @useDynLib lulcc2
.clues <- function(tprob, lu0.vals, demand, categories, scale.factor, max.iteration, max.difference, average.difference) {
    .Call("allocateclues", tprob, lu0.vals, demand, categories, scale.factor, max.iteration, max.difference, average.difference)
}

#' @useDynLib lulcc2
.clue <- function(regr, lu0.vals, demand, elasticity, change.rule, cellarea, min.elasticity, max.elasticity, min.change, max.change, min.value, max.value, max.iteration, max.difference, cell.area, ncell, ncode) {
    
    .Call("allocateclue", regr, lu0.vals, demand, elasticity, change.rule, cell.area, min.elasticity, max.elasticity, min.change, max.change, min.value, max.value, max.iteration, max.difference, ncell, ncode)

}

## # rdname allocate
## # aliases allocate,OrderedModel-method
## setMethod("allocate", signature(model = "OrderedModel"),
##           function(model, stochastic=TRUE, ...) {
##               map0 <- model@obs[[1]]
##               cells <- which(!is.na(raster::getValues(map0)))
##               map0.vals <- raster::extract(map0, cells)
##               if (!is.null(model@hist)) hist.vals <- raster::extract(model@hist, cells) else NULL
##               if (!is.null(model@mask)) mask.vals <- raster::extract(model@mask, cells) else NULL
##               newdata <- as.data.frame(x=model@ef, cells=cells)
##               prob <- predict(object=model@models, newdata=newdata)
##               maps <- raster::stack(map0)

##               for (i in 1:(nrow(model@demand) - 1)) {

##                    d <- model@demand[(i+1),]

##                    ## 1. update land use suitability matrix if dynamic factors exist
##                    if (model@ef@dynamic && i > 1) {
##                        newdata <- .update.data.frame(x=newdata, y=model@ef, map=map0, cells=cells, timestep=(i-1))
##                        prob <- predict(object=model@models, newdata=newdata)
##                    }
##                    tprob <- prob
                   
##                    ## 2. implement neighbourhood decision rules
##                    tprob <- .applyNeighbDecisionRules(model=model, x=map0, tprob=tprob)

##                    ## 3. implement other decision rules
##                    cd <- d - model@demand[i,] ## change direction
##                    tprob <- .applyDecisionRules(model=model, x=map0.vals, hist=hist.vals, cd=cd, tprob=tprob)

##                    ## 4. make automatic conversions if necessary
##                    auto <- .autoConvert(x=map0.vals, prob=tprob, categories=model@categories, mask=mask.vals)
##                    map0.vals[auto$ix] <- auto$vals
##                    tprob[auto$ix,] <- NA

##                    ## 5. allocation
##                    map1.vals <- do.call(.ordered, c(list(tprob=tprob, map0.vals=map0.vals, demand=d, categories=model@categories, order=model@order, stochastic=stochastic), model@params))
##                    map1 <- raster::raster(map0, ...) 
##                    map1[cells] <- map1.vals
##                    maps <- raster::stack(maps, map1)

##                    ## 6. prepare model for next timestep
##                    if (i < nrow(model@demand)) {
##                        if (!is.null(model@hist)) hist.vals <- .updatehist(map0.vals, map1.vals, hist.vals) 
##                        map0 <- map1
##                        map0.vals <- map1.vals 
##                    }
##                }
               
##                model@output <- maps
##                model     
##           }
## )

## # useDynLib lulcc2
## .ordered <- function(tprob, map0.vals, demand, categories, order, stochastic) {

##     map0.area <- .Call("total", map0.vals, categories)        ## initial condition
##     diff <- demand - map0.area
##     if (sum(abs(diff)) == 0) return(map0.vals)                
##     map1.vals <- map0.vals
    
##     for (i in 1:length(order)) {
        
##         ix <- which(categories %in% order[i])
##         cat <- categories[ix]
##         n <- demand[ix] - length(which(map1.vals %in% cat))   ## number of cells to convert

##         ## static demand
##         if (n == 0) {
##             ixx <- which(map0.vals %in% cat)                  ## index of all cells belonging to lu
##             tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
##         }
        
##         ## increasing demand
##         if (n > 0) {
##             ixx <- which(!map1.vals %in% cat)                 ## index of all cells not currently belonging to lu
##             p <- tprob[ixx,ix]                                ## suitability of all cells not currently belonging to lu (NB will include NAs)
##             p.ix <- order(p, na.last=TRUE, decreasing=TRUE)   ## index of cells when arranged from high to low
##             p <- p[p.ix]                                      ## suitability arranged from high to low
##             p.ix <- p.ix[which(!is.na(p))]                    ## index with NAs removed
##             p <- p[which(!is.na(p))]                          ## suitability with NAs removed
##             ixx <- ixx[p.ix]                                  ## actual index of cells (as they appear in map1.vals)     
##             #p.range <- range(p, na.rm=TRUE); print(p.range)                   
##             #p <- (p - p.range[1]) / diff(p.range)             ## normalise suitability (0-1)

##             ## repeat {
##             ##     select.ix <- which(p >= runif(length(p)))     ## compare suitability to numbers drawn from random normal distribution
##             ##     if (length(select.ix) >= abs(n)) break()      ## only exit loop if select.ix includes enough cells to meet demand
##             ## }

##             if (stochastic) {
##                 counter <- 0
##                 repeat {
##                     counter <- counter + 1
##                     select.ix <- which(p >= runif(length(p)))     ## compare suitability to numbers drawn from random normal distribution
##                     if (length(select.ix) >= abs(n) | counter > 1000) break()      ## only exit loop if select.ix includes enough cells to meet demand
##                 }

##             } else {
##                 select.ix <- seq(1, length(p))
##             }
            
##             select.ix <- select.ix[1:n]                       ## select cells with the highest suitability
##             ixx <- ixx[select.ix]                             ## index
##             map1.vals[ixx] <- cat                             ## allocate change
##             ixx <- which(map1.vals %in% cat)                  ## index of cells belonging to lu
##             tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
##         }

##         ## decreasing demand
##         if (n < 0) {
##             ixx <- which(map0.vals %in% cat)                  ## index of all cells currently belonging to lu
##             p <- tprob[ixx,ix]                                ## suitability of all cells currently belonging to lu (will include NAs)
##             p.ix <- order(p, na.last=TRUE, decreasing=FALSE)   ## index of cells when arranged low to high
##             p <- p[p.ix]                                      ## suitability arranged from low to high
##             p.ix <- p.ix[which(!is.na(p))]                    ## index with NAs removed
##             p <- p[which(!is.na(p))]                          ## suitability with NAs removed
##             ixx <- ixx[p.ix]                                  ## actual index of cells (as they appear in map1.vals)  
##             ## p.range <- range(p, na.rm=TRUE)                   
##             ## p <- (p - p.range[1]) / diff(p.range)             ## normalise suitability
##             if (stochastic) {
##                 counter <- 0
##                 repeat {
##                     counter <- counter + 1
##                     select.ix <- which(p < runif(length(p)))      ## compare suitability to numbers drawn from random normal distribution 
##                     if (length(select.ix) >= abs(n) | counter > 1000) break()      ## only exit loop if select.ix includes enough cells to meet demand
##                 }
##             } else {
##                 select.ix <- seq(1, length(p))
##             }

##             select.ix <- select.ix[1:abs(n)]                       ## select cells with lowest suitability
##             ixx <- ixx[select.ix]                             ## index 
##             map1.vals[ixx] <- -1                              ## unclassified
##             ixx <- which(map1.vals %in% cat)                  ## index of cells belonging to lu
##             tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
##         }
##     }
##     map1.vals
## }

################################################################################

## helper functions

.applyNeighbDecisionRules <- function(model, x, tprob) {
    if (!is.null(model@neighb) && !is.null(model@nb.rules)) {
        nb.allow <- allowNeighb(neighb=model@neighb, x=x, categories=model@categories, rules=model@nb.rules)
        tprob <- tprob * nb.allow
    } 
    tprob
}

.applyDecisionRules <- function(model, x, hist, cd, tprob) {
    if (!is.null(model@rules)) {
        allow <- allow(x=x, hist=hist, categories=model@categories, cd=cd, rules=model@rules)
        tprob <- tprob * allow
    }
    tprob
}

#' @useDynLib lulcc2
.updatehist <- function(lu0, lu1, hist) {
    hist <- .Call("updatehist", lu0, lu1, hist)
}

.maxtprob <- function(x) {    
    if (length(which(!is.na(x)) > 0)) {
        out <- max(x, na.rm=TRUE)
    } else {
        out <- NA
    }
}

#' @useDynLib lulcc2
.autoConvert <- function(x, prob, categories, mask=NULL, ...) {
    if (!is.null(mask) && length(x) != length(mask)) stop("mask must have same length as x")
    if (is.null(mask)) mask <- rep(1, length(x))
    ## TODO: change autoconvert function so mask is optional
    vals <- .Call("autoconvert", x, mask, prob, categories)
    ix <- which(!is.na(vals))
    vals <- vals[ix]
    out <- list(ix=ix, vals=vals)
}


## .allocate <- function(model, fun, ...) {              

##     map0 <- model@obs[[1]]
##     cells <- which(!is.na(raster::getValues(map0)))
##     map0.vals <- raster::extract(map0, cells)
##     hist.vals <- raster::extract(model@hist, cells)
##     mask.vals <- raster::extract(model@mask, cells)
##     newdata <- as.data.frame(x=model@pred, cells=cells)
##     prob <- predict(object=model@models, newdata=newdata)
##     maps <- raster::stack(map0)
              
##     for (i in 1:(nrow(model@demand) - 1)) {
##          print(i)                                    
##          d <- model@demand[(i+1),] ## demand for current timestep
##          if (model@pred@dynamic && i > 1) {
##              newdata <- .update.data.frame(x=newdata, y=model@pred, map=map0, cells=cells, timestep=(i-1))
##              prob <- predict(object=model@models, newdata=newdata)
##          }
##          tprob <- prob

##          ## elas only included in some models, so check whether model model has slot
##          if (.hasSlot(model, "elas")) { 
##              for (j in 1:length(model@categories)) {
##                  ix <- map0.vals %in% model@categories[j]
##                  tprob[ix,j] <- tprob[ix,j] + model@elas[j] ## add elasticity
##              }
##          }
                  
##          if (!is.null(model@neighb)) {
##              nb.allow <- allowNeighb(x=model@neighb, cells=cells, categories=model@categories, rules=model@nb.rules)
##              tprob <- tprob * nb.allow ## neighbourhood decision rules
##          }
                  
##          ## implement other decision rules
##          if (!is.null(model@rules)) {
##              cd <- d - model@demand[i,] ## change direction
##              allow <- allow(x=map0.vals, hist=hist.vals, categories=model@categories, cd=cd,rules=model@rules)
##              tprob <- tprob * allow
##          }

##          ## make automatic conversions if necessary
##          auto <- .autoConvert(x=map0.vals, mask=mask.vals, prob=tprob, categories=model@categories)
##          map0.vals[auto$ix] <- auto$vals
##          tprob[auto$ix,] <- NA
                  
##          ## allocation
##          args <- c(list(tprob=tprob, map0.vals=map0.vals, demand=d, categories=model@categories), model@params)
##          map1.vals <- do.call(fun, args)
##          map1 <- raster::raster(map0, ...) 
##          map1[cells] <- map1.vals
##          maps <- raster::stack(maps, map1)
    
##          ## prepare model for next timestep
##          if (i < nrow(model@demand)) {
##              hist.vals <- .updatehist(map0.vals, map1.vals, hist.vals) ## update
##              map0.vals <- map1.vals 
##              if (!is.null(model@neighb)) model@neighb <- NeighbRasterStack(x=map1, neighb=model@neighb)
##          }
##      }    
##      out <- maps              
## }


