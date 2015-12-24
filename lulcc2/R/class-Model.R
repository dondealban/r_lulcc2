setClassUnion("NeighbRasterStackOrNULL", c("NeighbRasterStack", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("RasterLayerOrNULL", c("RasterLayer", "NULL"))
setClassUnion("RasterStackOrNULL", c("RasterStack", "NULL"))

#' @include class-NeighbRasterStack.R class-ExpVarRasterStack.R class-PredictiveModelList.R class-ObsLulcRasterStack.R
NULL

#' Virtual class Model
#'
#' A virtual S4 class to represent land use change models.
#'
#' @slot output RasterStack containing simulated land use maps or NULL
#'
#' @export
#' @exportClass Model
#' @rdname Model-class
setClass("Model",
         contains = c("VIRTUAL"),
         slots = c(output = "RasterStackOrNULL"),
         validity = function(object) {
             ## TODO
             return(TRUE)
         }
)

#' Class CluesModel
#'
#' An S4 class to represent inputs to the CLUE-S land use change model.
#'
#' @slot observed.lulc an ObsLulcRasterStack object 
#' @slot explanatory.factors an ExpVarRasterStack object
#' @slot predictive.models a PredictiveModelList object
#' @slot time numeric vector of timesteps over which simulation will occur
#' @slot demand matrix containing demand scenario
#' @slot hist RasterLayer showing land use history or NULL
#' @slot mask RasterLayer showing masked areas or NULL
#' @slot neighbourhood NeighbRasterStack object or NULL
#' @slot transition.rules matrix with land use change decision rules
#' @slot neighbourhood.rules numeric with neighbourhood decision rules
#' @slot elasticity numeric indicating elasticity to change (only required for
#' @slot iteration.factor TODO
#' @slot max.iteration TODO
#' @slot max.difference TODO
#' @slot ave.difference TODO
#' @slot categories numeric vector of land use categories 
#' @slot labels character vector corresponding to \code{categories}
#'
#' @export
#' @exportClass CluesModel
#' @rdname CluesModel-class

setClass("CluesModel",
         contains = c("Model"),
         slots = c(
           observed.lulc = "ObsLulcRasterStack",               
           explanatory.factors = "ExpVarRasterStack",
           predictive.models = "PredictiveModelList",
           time = "numeric",
           demand = "matrix",
           hist = "RasterLayerOrNULL",           
           mask = "RasterLayerOrNULL",           
           neighbourhood = "NeighbRasterStackOrNULL",
           transition.rules = "matrix",
           neighbourhood.rules = "matrixOrNULL",
           elasticity = "numeric",
           iteration.factor = "numeric",
           max.iteration = "numeric",
           max.difference = "numeric",
           ave.difference = "numeric",
           categories = "numeric",
           labels = "character"),
         validity = function(object) {

             ## TODO: add these checks to validity function
             ## if (!all(obs@categories == models@categories))
             ##   stop()

             ## if (ncol(demand) != length(models))
             ##   stop("number of columns in argument 'demand' must equal number of predictive models")

             ## if (nrow(demand) != length(time))
             ##   stop("number of rows in argument 'demand' must equal time points in 'time'")

             ## if (!is.null(rules)) {
             ##     if (!all(dim(rules) %in% length(obs@categories))) {
             ##         stop("'rules' must be square matrix with dimensions equal to number of land use categories")
             ##     }
             ## } 

             ## compare(object@obs, object@ef, object@mask, object@hist)
             ## if (!is.null(object@neighb) {
             ##     compare(object@obs, object@neighb)
             ## }
             
             ## if (!is.null(neighb) && !is.null(nb.rules)) {
             ##     if (length(nb.rules) != length(neighb)) {
             ##         stop("rule should be provided for each neighbourhood map")
             ##     }

             ## } else if (is.null(neighb) && !is.null(nb.rules)) {
             ##     warning("neighb is NULL: neighbourhood decision rules not implemented")
             ##     nb.rules <- NULL
             ## }

             ## if (length(elas) != length(obs@categories)) {
             ##     stop("'elas' must be numeric vector with length equal to number of land use categories")
             ## }
             return(TRUE)
         }
)

## # Class OrderedModel
## #
## # An S4 class to represent inputs to the Ordered allocation procedure
## # 
## # slot obs an ObsLulcRasterStack object 
## # slot ef an ExpVarRasterStack object
## # slot models a PredictiveModelList object
## # slot time numeric vector of timesteps over which simulation will occur
## # slot demand matrix containing demand scenario
## # slot hist RasterLayer showing land use history or NULL
## # slot mask RasterLayer showing masked areas or NULL
## # slot neighb NeighbRasterStack object or NULL
## # slot categories numeric vector of land use categories 
## # slot labels character vector corresponding to \code{categories}
## # slot rules matrix with land use change decision rules
## # slot nb.rules numeric with neighbourhood decision rules
## # slot order numeric vector of land use categories in the order that change
## #   should be allocated
## # slot params list with model parameters
## # slot output RasterStack containing simulated land use maps or NULL
## #
## # export
## # exportClass OrderedModel
## # rdname OrderedModel-class
## setClass("OrderedModel",
##          contains = c("Model",
##          slots = c(obs = "ObsLulcRasterStack",           
##            ef = "ExpVarRasterStack",
##            models = "PredictiveModelList",
##            time = "numeric",
##            demand = "matrix",
##            hist = "RasterLayerOrNULL",           
##            mask = "RasterLayerOrNULL",           
##            neighb = "NeighbRasterStackOrNULL",         
##            categories = "numeric",
##            labels = "character",
##            rules = "matrixOrNULL",
##            nb.rules = "numericOrNULL",
##            order = "numeric",
##            params = "list"),
##          validity = function(object) {
##              ## TODO
##              ## check order only contains values in categories
##              return(TRUE)
##          }
## )         
