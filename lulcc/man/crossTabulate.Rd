% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossTabulate.R
\docType{methods}
\name{crossTabulate}
\alias{crossTabulate}
\alias{crossTabulate,RasterLayer,RasterLayer-method}
\alias{crossTabulate,DiscreteLulcRasterStack,ANY-method}
\title{Cross tabulate land use transitions}
\usage{
crossTabulate(x, y, ...)

\S4method{crossTabulate}{RasterLayer,RasterLayer}(x, y, categories,
  labels = as.character(categories), ...)

\S4method{crossTabulate}{DiscreteLulcRasterStack,ANY}(x, y, times, ...)
}
\arguments{
\item{x}{RasterLayer representing land use map from an earlier timestep or an
LulcRasterStack object containing at least two land use maps for different
points in time}

\item{y}{RasterLayer representing land use map from a later timestep. Not used
if \code{x} is an LulcRasterStack object}

\item{\dots}{additional arguments to \code{raster::\link[raster]{crosstab}}}

\item{categories}{numeric vector containing land use categories to consider.
Not used if \code{x} is an LulcRasterStack object}

\item{labels}{character vector (optional) with labels corresponding to
\code{categories}. Not used if \code{x} is an LulcRasterStack object}

\item{times}{numeric vector representing the time points of two land use maps
from LulcRasterStack}
}
\value{
A data.frame.
}
\description{
Cross tabulate land use transitions using
\code{raster::\link[raster]{crosstab}}. This step should form the basis of
further research into the processes driving the most important transitions in
the study region (Pontius et al., 2004).
}
\examples{

\dontrun{

## Plum Island Ecosystems 

## load observed land use maps
lu <- DiscreteLulcRasterStack(x=stack(pie[1:3]),
                              categories=c(1,2,3),
                              labels=c("Forest","Built","Other"),
                              t=c(0,6,14))

crossTabulate(x=lu, times=c(0,6))
crossTabulate(x=lu, times=c(0,14))

## RasterLayer input
crossTabulate(x=lu[[1]],
              y=lu[[3]],
              categories=c(1,2,3),
              labels=c("forest","built","other"))

}
}
\references{
Pontius Jr, R.G., Shusas, E., McEachern, M. (2004). Detecting important
categorical land changes while accounting for persistence. Agriculture,
Ecosystems & Environment 101(2):251-268.
}
\seealso{
\code{\link{LulcRasterStack}}, \code{raster::\link[raster]{crosstab}}
}
