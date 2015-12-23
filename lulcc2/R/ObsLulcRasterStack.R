#' @include class-ObsLulcRasterStack.R class-CategoryLabel.R
NULL

#' Create an ObsLulcRasterStack object
#'
#' Methods to create an ObsLulcRasterStack object, which may be created from file, an
#' existing Raster* object or a list of Raster* objects.
#'
#' Observed land use maps should have the same extent and resolution. The
#' location of non-NA cells in \code{ObsLulcRasterStack} objects defines the region for
#' subsequent analysis.
#' 
#' @param x path (character), Raster* object or list of Raster* objects. Default
#'   behaviour is to search for files in the working directory
#' @param pattern regular expression (character). Only filenames (if \code{x} is
#'   a path) or Raster* objects (if \code{x} is a list) matching the regular
#'   expression will be returned. See \cr
#'   \code{raster::\link[raster]{raster}} for more information about supported filetypes
#' @param categories numeric vector of land use categories in observed maps
#' @param labels character vector (optional) with labels corresponding to
#'   \code{categories}
#' @param t numeric vector containing the timestep of each observed map. The 
#'   first timestep must be 0
#' @param \dots additional arguments to \code{raster::\link[raster]{stack}}
#'
#' @return An ObsLulcRasterStack object.
#'
#' @seealso \code{\link{ObsLulcRasterStack-class}}, \code{raster::\link[raster]{stack}}
#'
#' @name ObsLulcRasterStack
#' @rdname ObsLulcRasterStack-methods
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Plum Island Ecosystems
#' obs <- ObsLulcRasterStack(x=pie,
#'                    pattern="lu",
#'                    categories=c(1,2,3),
#'                    labels=c("forest","built","other"),
#'                    t=c(0,6,14))
#' 
#' ## Sibuyan Island
#' obs <- ObsLulcRasterStack(x=sibuyan$maps,
#'                    pattern="lu",
#'                    categories=c(1,2,3,4,5),
#'                    labels=c("forest","coconut","grass","rice","other"),
#'                    t=c(0,14))
#'
#' }
NULL

#' @rdname ObsLulcRasterStack-methods
#' @exportMethod DiscreteObsLulcRasterStack
setGeneric("DiscreteObsLulcRasterStack", function(x, ...)
           standardGeneric("DiscreteObsLulcRasterStack"))

#' @rdname ObsLulcRasterStack-methods
#' @aliases DiscreteObsLulcRasterStack,Raster-method
setMethod("DiscreteObsLulcRasterStack", signature(x = "Raster"),
          function(x, ...) {
              out <- DiscreteObsLulcRasterStack(x=stack(x), ...)
          }
          )

#' @rdname ObsLulcRasterStack-methods
#' @aliases DiscreteObsLulcRasterStack,RasterStack-method
setMethod("DiscreteObsLulcRasterStack", signature(x = "RasterStack"),
          function(x, pattern, categories, labels, t) {
              if (missing(categories)) categories <- sort(unique(as.numeric(raster::getValues(x))))
              ix <- order(categories)
              categories <- categories[ix] ## check categories are in correct order
              labels <- labels[ix]
              out <- new("DiscreteObsLulcRasterStack", x, t=t, categories=categories, labels=labels)
          }
          )

#' @rdname ObsLulcRasterStack-methods
#' @exportMethod ContinuousObsLulcRasterStack
setGeneric("ContinuousObsLulcRasterStack", function(x, ...)
           standardGeneric("ContinuousObsLulcRasterStack"))

#' @rdname ObsLulcRasterStack-methods
#' @aliases ContinuousObsLulcRasterStack,Raster-method
setMethod("ContinuousObsLulcRasterStack", signature(x = "Raster"),
          function(x, ...) {
              out <- ContinuousObsLulcRasterStack(x=stack(x), ...)
          }
          )

#' @rdname ObsLulcRasterStack-methods
#' @aliases ContinuousObsLulcRasterStack,RasterStack-method
setMethod("ContinuousObsLulcRasterStack", signature(x = "RasterStack"),
          function(x, pattern, categories, labels, t) {

              if (missing(categories))
                categories <- seq_len(length(labels))

              ix <- order(categories)
              categories <- categories[ix] ## check categories are in correct order
              labels <- labels[ix]
              x <- x[[ix]]
              out <- new("ContinuousObsLulcRasterStack", x, t=t, categories=categories, labels=labels)
          }
          )
