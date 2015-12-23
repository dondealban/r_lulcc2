#' @include class-ExpVarRasterStack.R class-Model.R
NULL

#' Coerce objects to data.frame
#'
#' This function extracts data from all raster objects in
#' \code{\link{ObsLulcRasterStack}} or \code{\link{ExpVarRasterStack}} objects
#' for a specified timestep.
#'
#' If x is an ObsLulcRasterStack object the raster corresponding to t is first
#' transformed to a RasterBrick with a boolean layer for each class with
#' \code{raster::\link[raster]{layerize}}.
#'
#' @param x an ExpVarRasterStack or ObsLulcRasterStack object
#' @param row.names NULL or a character vector giving the row.names for the
#'   data.frame. Missing values are not allowed
#' @param optional logical. If TRUE, setting row names and converting column
#'   names (to syntactic names: see make.names) is optional
#' @param cells index of cells to be extracted, which may be a
#'   \code{SpatialPoints*} object or a numeric vector representing cell numbers
#'   (see \code{raster::\link[raster]{extract}})
#' @param t numeric indicating the time under consideration
#' @param \dots additional arguments (none)
#'
#' @seealso \code{\link[base]{as.data.frame}}, \code{\link{ObsLulcRasterStack}},
#' \code{\link{ExpVarRasterStack}}, \code{\link{partition}}
#'
#' @return A data.frame.
#'
#' @export
#' @rdname as.data.frame
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Plum Island Ecosystems
#' 
#' ## observed maps
#' obs <- ObsLulcRasterStack(x=pie,
#'                           pattern="lu", 
#'                           categories=c(1,2,3), 
#'                           labels=c("Forest","Built","Other"), 
#'                           t=c(0,6,14))
#' 
#' ## explanatory variables
#' ef <- ExpVarRasterStack(x=pie, pattern="ef")
#' 
#' ## separate data into training and testing partitions
#' part <- partition(x=obs[[1]], size=0.1, spatial=TRUE)
#' df1 <- as.data.frame(x=obs, cells=part[["all"]], t=0)
#' df2 <- as.data.frame(x=ef, cells=part[["all"]], t=0)
#'
#' }

#' @rdname as.data.frame
#' @method as.data.frame ExpVarRasterStack
#' @export
as.data.frame.ExpVarRasterStack <- function(x, row.names=NULL, optional=FALSE, cells, t, ...) {
    x <- .getExpVarRasterStack(x, time=t)
    st <- as(x, "RasterStack")
    as.data.frame(extract(x, cells, ...))
}

#' @rdname as.data.frame
#' @method as.data.frame DiscreteObsLulcRasterStack
#' @export
as.data.frame.DiscreteObsLulcRasterStack <- function(x, row.names=NULL, optional=FALSE, cells, t, ...) {

    if (!t %in% x@t) stop()
    ix <- which(x@t %in% t)
    br <- raster::layerize(x[[ix]])
    names(br) <- x@labels
    as.data.frame(raster::extract(x=br, y=cells))
}
    
#' @rdname as.data.frame
#' @method as.data.frame ContinuousObsLulcRasterStack
#' @export
as.data.frame.ContinuousObsLulcRasterStack <- function(x, row.names=NULL, optional=FALSE, cells, t, ...) {

    if (!t %in% x@t) stop()
    ix <- which(x@t %in% t)
    br <- as(x[[ix]], "RasterStack")
    names(br) <- x@labels
    as.data.frame(raster::extract(x=br, y=cells, ...))
}


#' @rdname as.data.frame
#' @aliases as.data.frame,ExpVarRasterStack-method
setMethod("as.data.frame","ExpVarRasterStack",as.data.frame.ExpVarRasterStack)

#' @rdname as.data.frame
#' @aliases as.data.frame,DiscreteObsLulcRasterStack-method
setMethod("as.data.frame","DiscreteObsLulcRasterStack",as.data.frame.DiscreteObsLulcRasterStack)

#' @rdname as.data.frame
#' @aliases as.data.frame,ContinuousObsLulcRasterStack-method
setMethod("as.data.frame","ContinuousObsLulcRasterStack",as.data.frame.ContinuousObsLulcRasterStack)


.update.data.frame <- function(x, y, map, cells, t, ...) {
    ## hidden function to update a data.frame containing dynamic explanatory variables
    ##
    ## Args:
    ##   x: a data.frame
    ##   y: an ExpVarRasterStack object
    ##   map: ???
    ##   cells: ???
    ##   t: the time for which dynamic explanatory variables should be updated
    ##
    ## Returns:
    ##   a data.frame
    
    ix <- t + 1
    nms <- names(x)
    if (length(y@maps) > 0) {
        dynamic.ix <- which(as.logical(sapply(y@maps, function(x) (nlayers(x) > 1))))
        if (length(dynamic.ix) > 0) {
            s <- raster::stack(lapply(y@maps[dynamic.ix], function(x) x[[ix]]))
            update.vals <- s[cells]
            x[,dynamic.ix] <- update.vals
        }
    }

    names(x) <- nms
    x
}
    
.getExpVarRasterStack <- function(x, time) {
    index <- x@index
    static.ix <- !index[,3]
    dyn.vars <- unique(index[!static.ix,1])
    dyn.ix <- sapply(seq_len(length(dyn.vars)), FUN=function(i) {
        var <- dyn.vars[i]
        t <- sort(index[(index[,1] %in% var), 2])
        t <- t[findInterval(time, t, all.inside=TRUE)]
        which(index[,1] %in% var & index[,2] %in% t)})

    ix <- sort(c(which(static.ix), dyn.ix))
    setNames(x[[ix]], index[ix,1])
}


