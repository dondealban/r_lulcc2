#' @include class-ExpVarRasterStack.R
NULL

#' Extract by index
#'
#' \code{object[[i]]} can be used to extract individual objects from container
#' classes such as \code{ExpVarRasterStack}, \code{PredictiveModelList}, \code{PredictionList} and
#' \code{PerformanceList}.
#'
#' @param x TODO
#' @param i layer number (if 'x' inherits from a RasterStack) or list index (if
#'   'x' stores data as a list)
#' @param j numeric (not used)
#' @param ... additional arguments (none)
#'
#' @export
#' @name Extract by index
#' @rdname extractIndex
#'
#' @examples
#'
#' ## Plum Island Ecosystems
#' 
#' ## Load observed land use maps
#' obs <- LulcRasterStack(x=pie,
#'                    pattern="lu",
#'                    categories=c(1,2,3),
#'                    labels=c("forest","built","other"),
#'                    t=c(0,6,14))
#' 
#' summary(obs[[1]])
#' summary(obs[[1:2]])
#'

## # rdname extractIndex
## # aliases [[,ExpVarRasterStack,ANY,ANY-method
## setMethod("[[", "ExpVarRasterStack",
##           function(x,i,j,...) {
              
## 	      if ( missing(i)) { 
##                   stop('you must provide an index') 
## 	      }
              
##  	      if (! missing(j)) { 
## 	          warning('second index is ignored') 
## 	      }
              
## 	      if (is.numeric(i)) {
## 	          sgn <- sign(i)
## 		  sgn[sgn==0] <- 1
## 		  if (! all(sgn == 1) ) {
##                       if (! all(sgn == -1) ) {
##                           stop("only 0's may be mixed with negative subscripts")
##                       } else {
##                           i <- (1:length(x))[i]
##                       }
##                   }
##               }
##               subset(x, i)
##           }
##           )

#' @rdname extractIndex
#' @aliases [[,DiscreteLulcRasterStack,ANY,ANY-method
setMethod("[[", "DiscreteLulcRasterStack",
          function(x,i,j,...) {
	      if ( missing(i)) { 
                  stop('you must provide an index') 
	      }
              
 	      if (! missing(j)) { 
	          warning('second index is ignored') 
	      }
              
	      if (is.numeric(i)) {
	          sgn <- sign(i)
		  sgn[sgn==0] <- 1
		  if (! all(sgn == 1) ) {
                      if (! all(sgn == -1) ) {
                          stop("only 0's may be mixed with negative subscripts")
                      } else {
                          i <- (1:length(x))[i]
                      }
                  }
              }
              subset(x, i)
          }
          )

#' @rdname extractIndex
#' @aliases [[,PerformanceList,ANY,ANY-method
setMethod("[[", "PerformanceList",
          function(x,i,j,...) {
	      if ( missing(i)) { 
                  stop('you must provide an index') 
	      }
              
 	      if (! missing(j)) { 
	          warning('second index is ignored') 
	      }
              
	      if (is.numeric(i)) {
	          sgn <- sign(i)
		  sgn[sgn==0] <- 1
		  if (! all(sgn == 1) ) {
                      if (! all(sgn == -1) ) {
                          stop("only 0's may be mixed with negative subscripts")
                      } else {
                          i <- (1:length(x))[i]
                      }
                  }
              }
              subset(x, i)
          }
          )

#' @rdname extractIndex
#' @aliases [[,PredictionList,ANY,ANY-method
setMethod("[[", "PredictionList",
          function(x,i,j,...) {
	      if ( missing(i)) { 
                  stop('you must provide an index') 
	      }
              
 	      if (! missing(j)) { 
	          warning('second index is ignored') 
	      }
              
	      if (is.numeric(i)) {
	          sgn <- sign(i)
		  sgn[sgn==0] <- 1
		  if (! all(sgn == 1) ) {
                      if (! all(sgn == -1) ) {
                          stop("only 0's may be mixed with negative subscripts")
                      } else {
                          i <- (1:length(x))[i]
                      }
                  }
              }
              subset(x, i)
          }
          )

#' @rdname extractIndex
#' @aliases [[,PredictiveModelList,ANY,ANY-method
setMethod("[[", "PredictiveModelList",
          function(x,i,j,...) {
	      if ( missing(i)) { 
                  stop('you must provide an index') 
	      }
              
 	      if (! missing(j)) { 
	          warning('second index is ignored') 
	      }
              
	      if (is.numeric(i)) {
	          sgn <- sign(i)
		  sgn[sgn==0] <- 1
		  if (! all(sgn == 1) ) {
                      if (! all(sgn == -1) ) {
                          stop("only 0's may be mixed with negative subscripts")
                      } else {
                          i <- (1:length(x))[i]
                      }
                  }
              }
              subset(x, i)
          }
          )

#' @rdname extractIndex
#' @aliases [[,ContinuousLulcRasterStack,ANY,ANY-method
setMethod("[[", "ContinuousLulcRasterStack",
          function(x,i,j,...) {
	      if ( missing(i)) { 
                  stop('you must provide an index') 
	      }
              
 	      if (! missing(j)) { 
	          warning('second index is ignored') 
	      }

              nt <- length(x@t)
              nl <- raster::nlayers(x)
              nc <- length(x@categories)

              ix <- as.numeric(sapply(i, FUN=function(index) seq(from=((index-1) * nc + 1), length.out=nc)))
              ## subset(x, seq(from=((i-1) * nc + 1), length.out=nc))

              new("ContinuousLulcRasterStack",
                  subset(x, ix),
                  labels=x@labels,
                  categories=x@categories,
                  t=x@t[i])

	      ## if (is.numeric(i)) {
	      ##     sgn <- sign(i)
	      ##     sgn[sgn==0] <- 1
	      ##     if (! all(sgn == 1) ) {
              ##         if (! all(sgn == -1) ) {
              ##             stop("only 0's may be mixed with negative subscripts")
              ##         } else {
              ##             i <- (1:length(x))[i]
              ##         }
              ##     }
              ## }
              ## subset(x, i)
          }
          )
