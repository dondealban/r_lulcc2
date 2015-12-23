#' @include class-ExpVarRasterStack.R
NULL

#' Create an ExpVarRasterStack object
#'
#' Methods to ...
#' 
#' @param x Raster* object
#' @param index data.frame
#' @param \dots additional arguments to \code{raster::\link[raster]{stack}}
#'
#' @seealso \code{raster::\link[raster]{stack}}
#' @return An ExpVarRasterStack object.
#'
#' @export
#' @rdname ExpVarRasterStack-methods
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Plum Island Ecosystems
#' 
#' ef <- ExpVarRasterStack(x=pie, pattern="ef")
#' 
#' ## Sibuyan
#' ef <- ExpVarRasterStack(x=sibuyan$maps, pattern="ef")
#'
#' }

setGeneric("ExpVarRasterStack", function(x, ...)
           standardGeneric("ExpVarRasterStack"))

#' @rdname ExpVarRasterStack-methods
#' @aliases ExpVarRasterStack,character-method
setMethod("ExpVarRasterStack", signature(x = "character"),
          function(x, ...) {
              ExpVarRasterStack(x=stack(x), ...)
          }
          )

#' @rdname ExpVarRasterStack-methods
#' @aliases ExpVarRasterStack,RasterStack-method
setMethod("ExpVarRasterStack", signature(x = "RasterStack"),
          function(x, index, ...) {
              new("ExpVarRasterStack", x, index=index)
          }
          )

