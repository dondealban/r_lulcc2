#' @include length.R names.R ExpVarRasterStack.R PerformanceList.R
NULL

#' Subset
#'
#' Extract a subset of objects from container classes such as
#' \code{ExpVarRasterStack}, \code{PredictiveModelList}, \code{PredictionList} and
#' \code{PerformanceList}.
#'
#' @param x an object of class \code{ExpVarRasterStack},
#'   \code{PredictiveModelList}, \code{PredictionList} or \code{PerformanceList}
#' @param subset integer or character indicating the objects to be extracted
#' @param ... additional arguments (none)
#'
#' @export
#' @rdname subset-methods
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Sibuyan Island
#'
#' ## load observed land use data
#' obs <- LulcRasterStack(x=sibuyan$maps,
#'                     pattern="lu",
#'                     categories=c(1,2,3,4,5),
#'                     labels=c("Forest","Coconut","Grass","Rice","Other"),
#'                     t=c(0,14))
#' 
#' summary(obs)
#' obs <- subset(obs, subset=names(obs)[1])
#' summary(obs)
#' 
#' ## load explanatory variables
#' ef <- ExpVarRasterStack(x=sibuyan$maps, pattern="ef")
#' 
#' summary(ef)
#' ef <- subset(ef, subset=1:5)
#' summary(ef)
#'
#' }

## # rdname subset-methods
## # aliases subset,ExpVarRasterStack-method
## setMethod("subset", signature(x="ExpVarRasterStack"), 
##           function(x, subset, ...) {
##               subset <- .getsubset(x, subset)
##               x[[subset]]
##           }
##           )

#' @rdname subset-methods
#' @aliases subset,PredictiveModelList-method
setMethod("subset", signature(x="PredictiveModelList"), 
          function(x, subset, ...) {
              subset <- .getsubset(x, subset)
              if (length(subset) == 1) {
                  x <- x@models[[subset]]
                  ## x <- new("PredictiveModelList",  ## is this the behaviour we want?
                  ##          models=list(x@models[[subset]]),
                  ##          categories=x@categories[subset],
                  ##          labels=x@labels[subset])
              } else {
                  x <- new("PredictiveModelList",
                           models=x@models[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              }
              return(x)	
          }
          )

#' @rdname subset-methods
#' @aliases subset,PerformanceList-method
setMethod("subset", signature(x="PerformanceList"), 
          function(x, subset, ...) {
              subset <- .getsubset(x, subset)
              ## if (is.character(subset)) {
              ##     i <- na.omit(match(subset, names(x)))
              ##     if (length(i)==0) {
              ##         stop('invalid performance object names')
              ##     } else if (length(i) < length(subset)) {
              ##         warning('invalid performance object names omitted')
              ##     }
              ##     subset <- i
              ## }
              ## subset <- as.integer(subset)
              ## if (! all(subset %in% 1:length(x))) {
              ##     stop('not a valid subset')
              ## }
              if (length(subset) == 1) {
                  x <- new("PerformanceList",
                           performance=list(x@performance[[subset]]),
                           auc=x@auc[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              } else {
                  x <- new("PerformanceList",
                           performance=x@performance[subset],
                           auc=x@auc[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              }
              return(x)	
          }
          )

#' @rdname subset-methods
#' @aliases subset,PredictionList-method
setMethod("subset", signature(x="PredictionList"), 
          function(x, subset, ...) {
              subset <- .getsubset(x, subset)
              ## if (is.character(subset)) {
              ##     i <- na.omit(match(subset, names(x)))
              ##     if (length(i)==0) {
              ##         stop('invalid prediction object names')
              ##     } else if (length(i) < length(subset)) {
              ##         warning('invalid prediction object names omitted')
              ##     }
              ##     subset <- i
              ## }
              ## subset <- as.integer(subset)
              ## if (! all(subset %in% 1:length(x))) {
              ##     stop('not a valid subset')
              ## }
              if (length(subset) == 1) {
                  x <- new("PredictionList",
                           prediction=list(x@prediction[[subset]]),
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              } else {
                  x <- new("PredictionList",
                           prediction=x@prediction[subset],
                           categories=x@categories[subset],
                           labels=x@labels[subset])
              }
              return(x)	
          }
          )


.getsubset <- function(x, subset) {
    if (is.character(subset)) {
        i <- na.omit(match(subset, names(x)))
        if (length(i)==0) {
            stop('invalid object names')
        } else if (length(i) < length(subset)) {
            warning('invalid object names omitted')
        }
        subset <- i
    }
    subset <- as.integer(subset)
    if (! all(subset %in% 1:length(x))) {
        stop('not a valid subset')
    }
    subset
}
