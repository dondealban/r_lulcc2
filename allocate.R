#' Ordered allocation
#'
#' Allocate land use change using the ordered algorithm.
#'
#' @param lu0 RasterLayer showing initial land use
#' @param lu0.vals numeric containing non-NA values from \code{lu0}
#' @param tprob matrix with land use suitability values. Columns should
#'   correspond to \code{categories}, rows should correspond with \code{cells}
#' @param nb neighbourhood map. See CluesModel
#' @param nb.rules neighbourhood rules. See CluesModel documentation
#' @param transition.rules transition rules. See CluesModel documentation
#' @param hist.vals numeric vector detailing the number of consecutive time steps
#'   each cell has been allocated to its current land use
#' @param mask.vals numeric vector containing binary values where 0 indicates
#'   cells that are not allowed to change
#' @param demand matrix with demand for each land use category in terms of number
#'   of cells to be allocated. The first row should be the number of cells
#'   allocated to the initial land use map, the second row should be the number
#'   of cells to allocate in the subsequent time point
#' @param categories numeric vector containing land use categories
#' @param order numeric vector of land use categories in the order that change
#'   should be allocated
#' @param stochastic Logical indicating whether or not the allocation routine
#'   should be run in stochastic mode
#' @param \dots additional arguments (none)
#'
#' @return numeric vector with updated land use values.
#'
#' @useDynLib lulcc
#'
#' @export
#' @rdname ordered
#'
#' @examples
#'
#' ## See lulcc-package examples
ordered <- function(lu0, lu0.vals, tprob, nb=NULL, nb.rules=NULL, transition.rules=NULL, hist.vals=NULL, mask.vals=NULL, demand, categories, order, stochastic) {

    ## apply neighbourhood rules
    tprob <- .applyNeighbDecisionRules(nb=nb, nb.rules=nb.rules, x=lu0, tprob=tprob, categories=categories)

    ## apply transition rules
    change.direction <- demand[2,] - demand[1,]
    tprob <- .applyDecisionRules(transition.rules=transition.rules, x=lu0.vals, hist=hist.vals, cd=change.direction, tprob=tprob)

    ## make automatic changes and set the probability of these cells to NA
    auto  <- .autoConvert(x=lu0.vals, prob=tprob, categories=categories, mask=mask.vals)
    lu0.vals[auto$ix] <- auto$vals
    tprob[auto$ix,] <- NA

    demand <- demand[2,,drop=TRUE]

    ## initial condition
    lu0.area <- .Call("total", lu0.vals, categories)
    diff <- demand - lu0.area
    if (sum(abs(diff)) == 0) return(lu0.vals)                
    lu1.vals <- lu0.vals
    
    for (i in 1:length(order)) {
        
        ix <- which(categories %in% order[i])
        cat <- categories[ix]
        n <- demand[ix] - length(which(lu1.vals %in% cat))   ## number of cells to convert

        ## static demand
        if (n == 0) {
            ixx <- which(lu0.vals %in% cat)                  ## index of all cells belonging to lu
            tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
        }
        
        ## increasing demand
        if (n > 0) {
            ixx <- which(!lu1.vals %in% cat)                 ## index of all cells not currently belonging to lu
            p <- tprob[ixx,ix]                                ## suitability of all cells not currently belonging to lu (NB will include NAs)
            p.ix <- order(p, na.last=TRUE, decreasing=TRUE)   ## index of cells when arranged from high to low
            p <- p[p.ix]                                      ## suitability arranged from high to low
            p.ix <- p.ix[which(!is.na(p))]                    ## index with NAs removed
            p <- p[which(!is.na(p))]                          ## suitability with NAs removed
            ixx <- ixx[p.ix]                                  ## actual index of cells (as they appear in lu1.vals)     
            #p.range <- range(p, na.rm=TRUE); print(p.range)                   
            #p <- (p - p.range[1]) / diff(p.range)             ## normalise suitability (0-1)

            ## repeat {
            ##     select.ix <- which(p >= runif(length(p)))     ## compare suitability to numbers drawn from random normal distribution
            ##     if (length(select.ix) >= abs(n)) break()      ## only exit loop if select.ix includes enough cells to meet demand
            ## }

            if (stochastic) {
                counter <- 0
                repeat {
                    counter <- counter + 1
                    select.ix <- which(p >= runif(length(p)))     ## compare suitability to numbers drawn from random normal distribution
                    if (length(select.ix) >= abs(n) | counter > 1000) break()      ## only exit loop if select.ix includes enough cells to meet demand
                }

            } else {
                select.ix <- seq(1, length(p))
            }
            
            select.ix <- select.ix[1:n]                       ## select cells with the highest suitability
            ixx <- ixx[select.ix]                             ## index
            lu1.vals[ixx] <- cat                             ## allocate change
            ixx <- which(lu1.vals %in% cat)                  ## index of cells belonging to lu
            tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
        }

        ## decreasing demand
        if (n < 0) {
            ixx <- which(lu0.vals %in% cat)                  ## index of all cells currently belonging to lu
            p <- tprob[ixx,ix]                                ## suitability of all cells currently belonging to lu (will include NAs)
            p.ix <- order(p, na.last=TRUE, decreasing=FALSE)   ## index of cells when arranged low to high
            p <- p[p.ix]                                      ## suitability arranged from low to high
            p.ix <- p.ix[which(!is.na(p))]                    ## index with NAs removed
            p <- p[which(!is.na(p))]                          ## suitability with NAs removed
            ixx <- ixx[p.ix]                                  ## actual index of cells (as they appear in lu1.vals)  
            ## p.range <- range(p, na.rm=TRUE)                   
            ## p <- (p - p.range[1]) / diff(p.range)             ## normalise suitability
            if (stochastic) {
                counter <- 0
                repeat {
                    counter <- counter + 1
                    select.ix <- which(p < runif(length(p)))      ## compare suitability to numbers drawn from random normal distribution 
                    if (length(select.ix) >= abs(n) | counter > 1000) break()      ## only exit loop if select.ix includes enough cells to meet demand
                }
            } else {
                select.ix <- seq(1, length(p))
            }

            select.ix <- select.ix[1:abs(n)]                       ## select cells with lowest suitability
            ixx <- ixx[select.ix]                             ## index 
            lu1.vals[ixx] <- -1                              ## unclassified
            ixx <- which(lu1.vals %in% cat)                  ## index of cells belonging to lu
            tprob[ixx,] <- NA                                 ## set suitability of these cells to NA
        }
    }
    lu1.vals
}

################################################################################

## helper functions

.applyNeighbDecisionRules <- function(nb, nb.rules, x, tprob, categories) {
    if (!is.null(nb) && !is.null(nb.rules)) {
        nb.allow <- allowNeighb(neighb=nb, x=x, categories=categories, rules=nb.rules)
        tprob <- tprob * nb.allow
    } 
    tprob
}

.applyDecisionRules <- function(transition.rules, x, hist, cd, tprob, categories) {
    if (!is.null(transition.rules) && !is.null(hist)) {
        allow <- allow(x=x, hist=hist, categories=categories, cd=cd, rules=transition.rules)
        tprob <- tprob * allow
    }
    tprob
}

#' @useDynLib lulcc
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

#' @useDynLib lulcc
.autoConvert <- function(x, prob, categories, mask=NULL, ...) {
    if (!is.null(mask) && length(x) != length(mask)) stop("mask must have same length as x")
    if (is.null(mask)) mask <- rep(1, length(x))
    ## TODO: change autoconvert function so mask is optional
    vals <- .Call("autoconvert", x, mask, prob, categories)
    ix <- which(!is.na(vals))
    vals <- vals[ix]
    out <- list(ix=ix, vals=vals)
}

## .applyNeighbDecisionRules <- function(model, x, tprob) {
##     if (!is.null(model@neighbourhood) && !is.null(model@neighbourhood.rules)) {
##         nb.allow <- allowNeighb(neighb=model@neighbourhood, x=x, categories=model@categories, rules=model@neighbourhood.rules)
##         tprob <- tprob * nb.allow
##     } 
##     tprob
## }

## .applyDecisionRules <- function(model, x, hist, cd, tprob) {
##     if (!is.null(model@transition.rules)) {
##         allow <- allow(x=x, hist=hist, categories=model@categories, cd=cd, rules=model@transition.rules)
##         tprob <- tprob * allow
##     }
##     tprob
## }

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


library(Rcpp)
cppFunction('int myfun(IntegerVector x, int a) {
  IntegerVector v = Rcpp::seq(0, x.size()-1);
  IntegerVector index = v[x == a];
  if (index.size() != 1) {
    stop("error");
  }
  return index[0];
}')

cppFunction('NumericVector myfun2(NumericVector x) {
  // NumericVector sorted = clone(x).sort();
  NumericVector y = clone(x);
  return std::sort(y.begin(), y.end(), std::greater<double>());
  // return match(sorted, x);
}')

cppFunction('NumericVector sortIt(NumericVector x){
  NumericVector v = clone(x);
  std::sort(v.begin(), v.end(), std::greater<double>()); // does not work returns ascending
  return v;
}')

cppFunction('NumericVector myfun2(NumericVector x) {
  NumericVector y = clone(x);
  if (y.size() > 5) {
    y[5] = NA_REAL;
  }
  return y;
}')

cppFunction('bool myfun3(IntegerVector x, int a) {
  return std::find(x.begin(), x.end(), a) != x.end();
}')

cppFunction('IntegerVector myfun4(IntegerVector x, int a) {
  IntegerVector y = clone(x);
  LogicalVector index = y == a;
  y[index] = 10;
  return(y);
}')

cppFunction('NumericVector myfun2(NumericVector x) {
  NumericVector y = clone(x);
  NumericVector absy = abs(y);
  return y;
}')

cppFunction('NumericVector myfun(NumericMatrix x, IntegerVector rows, int col) {
  NumericVector xx = x(_,col);
  xx = xx[rows];
  return xx;
}')
