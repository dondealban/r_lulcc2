#' @include class-ObsLulcRasterStack.R class-ExpVarRasterStack.R as.data.frame.R
NULL

#' Extract data to fit predictive models
#'
#' Extract a data.frame containing variables required for fitting predictive
#' models.
#'
#' @param lu an ObsLulcRasterStack object
#' @param ef an ExpVarRasterStack object
#' @param cells index of cells to be extracted, which may be a
#'   \code{SpatialPoints*} object or a numeric vector representing cell numbers
#'   (see \code{raster::\link[raster]{extract}})
#' @param ... additional arguments to \link{as.data.frame}
#'
#' @seealso \code{\link[base]{as.data.frame}}, \code{\link{ObsLulcRasterStack}},
#' \code{\link{ExpVarRasterStack}}, \code{\link{partition}}
#'
#' @return A data.frame.
#'
#' @export
#' @rdname getPredictiveModelInputData
#' 
#' @examples
#'
#' ## TODO
getPredictiveModelInputData <- function(lu, ef, cells, ...) {
    ludf <- as.data.frame(lu, cells=cells, ...)
    efdf  <- as.data.frame(ef, cells=cells, ...)
    cbind(ludf, efdf)
}
