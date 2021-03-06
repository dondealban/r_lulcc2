% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allocate.R
\docType{methods}
\name{allocate}
\alias{allocate}
\alias{allocate,CluesModel-method}
\alias{allocate,ClueModel-method}
\alias{allocate,OrderedModel-method}
\title{Allocate land use change spatially}
\usage{
allocate(model, ...)

\S4method{allocate}{CluesModel}(model, ...)

\S4method{allocate}{ClueModel}(model, ...)

\S4method{allocate}{OrderedModel}(model, stochastic = TRUE, ...)
}
\arguments{
\item{model}{an object inheriting from class \code{Model}}

\item{\dots}{additional arguments for specific methods}

\item{stochastic}{logical}
}
\value{
LulcRasterStack.
}
\description{
Perform spatial allocation of land use change using different models.
Currently the function provides an implementation of the Change in Land Use
and its Effects (CLUE; Veldkamp and Fresco, 1996, Verburg et al., 1996),
CLUE at Small regional extent (CLUE-S; Verburg et al., 2002) and an ordered
procedure based on the algorithm described by Fuchs et al., (2013), modified
to allow stochastic transitions.
}
\examples{

## see lulcc-package examples
}
\references{
Fuchs, R., Herold, M., Verburg, P.H., and Clevers, J.G.P.W. (2013). A
high-resolution and harmonized model approach for reconstructing and analysing
historic land changes in Europe, Biogeosciences, 10:1543-1559.

Veldkamp, A., & Fresco, L. O. (1996). CLUE-CR: an integrated multi-scale model
to simulate land use change scenarios in Costa Rica. Ecological modelling,
91(1), 231-248.

Verburg, P.H., & Bouma, J. (1999). Land use change under conditions of high
population pressure: the case of Java. Global environmental change, 9(4),
303-312.

Verburg, P.H., Soepboer, W., Veldkamp, A., Limpiada, R., Espaldon, V., Mastura,
S.S. (2002). Modeling the spatial dynamics of regional land use: the CLUE-S
model. Environmental management, 30(3):391-405.
}
\seealso{
\code{\link{CluesModel}}
}
