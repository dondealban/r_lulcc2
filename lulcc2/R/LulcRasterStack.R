#' @include class-LulcRasterStack.R
NULL

#' Create an LulcRasterStack object
#'
#' Methods to create an LulcRasterStack object, which may be created from file, an
#' existing Raster* object or a list of Raster* objects.
#'
#' Observed land use maps should have the same extent and resolution. The
#' location of non-NA cells in \code{LulcRasterStack} objects defines the region for
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
#' @return An LulcRasterStack object.
#'
#' @seealso \code{\link{LulcRasterStack-class}}, \code{raster::\link[raster]{stack}}
#'
#' @name LulcRasterStack
#' @rdname LulcRasterStack-methods
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Plum Island Ecosystems
#' obs <- LulcRasterStack(x=pie,
#'                    pattern="lu",
#'                    categories=c(1,2,3),
#'                    labels=c("forest","built","other"),
#'                    t=c(0,6,14))
#' 
#' ## Sibuyan Island
#' obs <- LulcRasterStack(x=sibuyan$maps,
#'                    pattern="lu",
#'                    categories=c(1,2,3,4,5),
#'                    labels=c("forest","coconut","grass","rice","other"),
#'                    t=c(0,14))
#'
#' }
NULL

#' @rdname LulcRasterStack-methods
#' @exportMethod DiscreteLulcRasterStack
setGeneric("DiscreteLulcRasterStack", function(x, ...)
           standardGeneric("DiscreteLulcRasterStack"))

#' @rdname LulcRasterStack-methods
#' @aliases DiscreteLulcRasterStack,Raster-method
setMethod("DiscreteLulcRasterStack", signature(x = "Raster"),
          function(x, ...) {
              out <- DiscreteLulcRasterStack(x=stack(x), ...)
          }
          )

#' @rdname LulcRasterStack-methods
#' @aliases DiscreteLulcRasterStack,RasterStack-method
setMethod("DiscreteLulcRasterStack", signature(x = "RasterStack"),
          function(x, pattern, categories, labels, t) {
              if (missing(categories)) categories <- sort(unique(as.numeric(raster::getValues(x))))
              ix <- order(categories)
              categories <- categories[ix] ## check categories are in correct order
              labels <- labels[ix]
              new("DiscreteLulcRasterStack", x, t=t, categories=categories, labels=labels)
          }
          )

#' @rdname LulcRasterStack-methods
#' @exportMethod ContinuousLulcRasterStack
setGeneric("ContinuousLulcRasterStack", function(x, ...)
           standardGeneric("ContinuousLulcRasterStack"))

#' @rdname LulcRasterStack-methods
#' @aliases ContinuousLulcRasterStack,Raster-method
setMethod("ContinuousLulcRasterStack", signature(x = "Raster"),
          function(x, ...) {
              out <- ContinuousLulcRasterStack(x=stack(x), ...)
          }
          )

#' @rdname LulcRasterStack-methods
#' @aliases ContinuousLulcRasterStack,RasterStack-method
setMethod("ContinuousLulcRasterStack", signature(x = "RasterStack"),
          function(x, categories, labels, t) {

              ## if (missing(categories))
              categories <- seq_len(length(labels))

              ## ix <- order(categories)
              ## categories <- categories[ix] ## check categories are in correct order
              ## labels <- labels[ix]
              ## x <- x[[ix]]
              new("ContinuousLulcRasterStack", x, t=t, categories=categories, labels=labels)
          }
          )
