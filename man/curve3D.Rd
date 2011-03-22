\name{curve3D}
\alias{curve3D}
\title{
An analogue to curve() for functions of two variables.
}
\description{
An analogue to curve() for functions of two variables. Functions can be
visualized using heatmaps or 3D surfaces.
}
\usage{
curve3D(f, from.x, to.x, from.y, to.y, heatmap = FALSE, n = 101)
}
\arguments{
  \item{f}{
	Function to visualize. Must expect arguments named 'x' and 'y'.
}
  \item{from.x}{
	Minimal value of 'x'.
}
  \item{to.x}{
	Maximal value of 'x'.
}
  \item{from.y}{
	Minimal value of 'y'.
}
  \item{to.y}{
	Maximal value of 'y'.
}
  \item{heatmap}{
	Boolean indicates whether or not to visualize function using a 3D surface or a heatmap. Defaults to FALSE.
}
  \item{n}{
	How many points should f be evaluated at along each dimension?
}
}
\value{
	None. Called for side effects.
}
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
