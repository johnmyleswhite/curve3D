\name{curve3D-package}
\alias{curve3D-package}
\docType{package}
\title{
An analogue to curve() for functions of two variables.
}
\description{
An analogue to curve() for functions of two variables. Functions can be
visualized using heatmaps or 3D surfaces.
}
\details{
\tabular{ll}{
Package: \tab curve3D\cr
Type: \tab Package\cr
Version: \tab 0.1-1\cr
Date: \tab 2011-03-21\cr
License: \tab Artistic-2.0\cr
LazyLoad: \tab yes\cr
}
}
\author{
John Myles White

Maintainer: John Myles White <jmw@johnmyleswhite.com>
}
\keyword{ package }
\examples{
library('curve3D')

g <- function(x, y) {(1 + y * 2) ^ (-x / y) * (1 + y * 1) ^ (x / y)}

curve3D(g, from.x = 0.01, to.x = 1, from.y = 0.01, to.y = 1, heatmap = FALSE)
curve3D(g, from.x = 0.01, to.x = 1, from.y = 0.01, to.y = 1, heatmap = TRUE)

h <- function(x, y) {x ^ 2 + y ^ 2}

curve3D(h, from.x = 0.01, to.x = 1, from.y = 0.01, to.y = 1, heatmap = FALSE)
curve3D(h, from.x = 0.01, to.x = 1, from.y = 0.01, to.y = 1, heatmap = TRUE)

i <- function(x, y) {sin(x) + sin(y)}

curve3D(i, from.x = 1, to.x = 10, from.y = 1, to.y = 10, heatmap = FALSE)
curve3D(i, from.x = 1, to.x = 10, from.y = 1, to.y = 10, heatmap = TRUE)

j <- function(x, y) {x + y + x * y}

curve3D(j, from.x = 1, to.x = 10, from.y = 1, to.y = 10, heatmap = FALSE)
curve3D(j, from.x = 1, to.x = 10, from.y = 1, to.y = 10, heatmap = TRUE)
}
