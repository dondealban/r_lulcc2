#' @include class-PredictiveModelList.R class-ObsLulcRasterStack.R class-ExpVarRasterList.R as.data.frame.R
NULL

#' Fit predictive models 
#'
#' These functions fit parametric and non-parametric models to data.
#'
#' @param lu ObsLulcRasterStack
#' @param ef ExpVarRasterStack
#' @param time numeric
#' @param subset SpatialPoints
#' @param formula list containing formula objects
## # param family see \code{\link[stats]{glm}}. Default is 'binomial'. Only used by
## #   \code{glmModels}
#' @param ... additional arguments to specific functions
## # @param obs an ObsLulcRasterStack object
## # @param categories numeric vector of land use categories in observed maps.
## #   Only required if 'obs' is missing
## # @param labels character vector (optional) with labels corresponding to
## #   \code{categories}. Only required if 'obs' is missing
#'
#' @seealso \code{\link[stats]{glm}}, \code{rpart::\link[rpart]{rpart}},
#'   \code{randomForest::\link[randomForest]{randomForest}}
#' @return A PredictiveModelList object.
#'
#' @name Model fitting
#' @rdname Model-fitting
#'
#' @examples
#'
#' ## see lulcc-package examples
NULL

#' Method lmModels
#' @rdname Model-fitting
#' @exportMethod lmModels
setGeneric("lmModels", function(lu, ef, ...)
           standardGeneric("lmModels"))

#' @rdname Model-fitting
#' @aliases lmModels,ContinuousObsLulcRasterStack,list-method
setMethod("lmModels", c("ContinuousObsLulcRasterStack","ExpVarRasterStack"),
          function(lu, ef, time, subset, formula, ...) {

              lm.models <- vector(mode="list", length=length(formula))
              formula <- .checkFormula(formula, x@categories, x@labels)

              lu <- lu[[which(lu@t %in% time)]]
              ludf <- as.data.frame(lu, cells=subset)
              efdf <- as.data.frame(ef, cells=subset, time=time)
              data <- cbind(ludf, efdf)
              
              for (i in 1:length(formula)) {
                  form <- formula[[i]]
                  lm.models[[i]] <- lm(form, data=data, ...)
              }

              out <- new("PredictiveModelList",
                         models=lm.models,
                         categories=x@categories,
                         labels=x@labels)
          }
          )

#' Method glmModels
#' @rdname Model-fitting
#' @exportMethod glmModels
setGeneric("glmModels", function(x, formula, ...)
           standardGeneric("glmModels"))

#' @rdname Model-fitting
#' @aliases glmModels,DiscreteObsLulcRasterStack,list-method
setMethod("glmModels", c("DiscreteObsLulcRasterStack","list"),
          function(x, formula, family=binomial, model=FALSE, ...) {

              glm.models <- vector(mode="list", length=length(formula))
              formula <- .checkFormula(formula, x@categories, x@labels)

              for (i in 1:length(formula)) {
                  form <- formula[[i]]
                  glm.models[[i]] <- glm(form, family=family, model=model, ...)
              }

              out <- new("PredictiveModelList",
                         models=glm.models,
                         categories=x@categories,
                         labels=x@labels)
          }

## # export
## # rdname Model-fitting
## randomForestModels <- function(formula, ..., obs, categories=NA, labels=NA) {

##     rf.models <- list()

##     if (!missing(obs)) {
##         categories <- obs@categories
##         labels <- obs@labels
##     }
##     formula <- .checkFormula(formula, categories, labels)
    
##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         rf.models[[i]] <- randomForest::randomForest(form, ...)
##     }

##     out <- new("PredictiveModelList",
##                models=rf.models,
##                categories=categories,
##                labels=labels)
## }

## # export
## # rdname Model-fitting
## rpartModels <- function(formula, ..., obs, categories=NA, labels=NA) {

##     rpart.models <- list()

##     if (!missing(obs)) {
##         categories <- obs@categories
##         labels <- obs@labels
##     }
##     formula <- .checkFormula(formula, categories, labels)
    
##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         rpart.models[[i]] <- rpart::rpart(form, method="class", ...)
##     }

##     out <- new("PredictiveModelList",
##                models=rpart.models,
##                categories=categories,
##                labels=labels)
## }


## # export
## # rdname Model-fitting
## lmModels <- function(formula, ..., obs, categories=NA, labels=NA) {

##     lm.models <- vector(mode="list", length=length(formula))
##     if (!missing(obs)) {
##         categories <- obs@categories
##         labels <- obs@labels
##     }
##     formula <- .checkFormula(formula, categories, labels)

##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         lm.models[[i]] <- lm(form, model=model, ...)
##     }

##     out <- new("PredictiveModelList",
##                models=lm.models,
##                categories=categories,
##                labels=labels)
## }
    

## # export 
## # rdname Model-fitting
## glmModels <- function(formula, family=binomial, ..., obs, categories=NA, labels=NA) {
    
##     glm.models <- list()

##     if (!missing(obs)) {
##         categories <- obs@categories
##         labels <- obs@labels
##     }
##     formula <- .checkFormula(formula, categories, labels)
    
##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         glm.models[[i]] <- glm(form, family=family, model=model, ...)
##     }

##     out <- new("PredictiveModelList",
##                models=glm.models,
##                categories=categories,
##                labels=labels)
## }

## # export
## # rdname Model-fitting
## randomForestModels <- function(formula, ..., obs, categories=NA, labels=NA) {

##     rf.models <- list()

##     if (!missing(obs)) {
##         categories <- obs@categories
##         labels <- obs@labels
##     }
##     formula <- .checkFormula(formula, categories, labels)
    
##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         rf.models[[i]] <- randomForest::randomForest(form, ...)
##     }

##     out <- new("PredictiveModelList",
##                models=rf.models,
##                categories=categories,
##                labels=labels)
## }

## # export
## # rdname Model-fitting
## rpartModels <- function(formula, ..., obs, categories=NA, labels=NA) {

##     rpart.models <- list()

##     if (!missing(obs)) {
##         categories <- obs@categories
##         labels <- obs@labels
##     }
##     formula <- .checkFormula(formula, categories, labels)
    
##     for (i in 1:length(formula)) {
##         form <- formula[[i]]
##         rpart.models[[i]] <- rpart::rpart(form, method="class", ...)
##     }

##     out <- new("PredictiveModelList",
##                models=rpart.models,
##                categories=categories,
##                labels=labels)
## }


.checkFormula <- function(formula, categories, labels) {
    
    dep <- sapply(formula, function(x) as.character(x)[2])
    if (length(categories) != length(labels))
      stop("'labels' must correspond to 'categories'")

    if (!all(labels %in% dep))
      stop("a formula must be supplied for each land use type")

    formula <- formula[match(dep, labels)]
}
