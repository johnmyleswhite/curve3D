curve3D <- function(f, from.x = 0, to.x = 1, from.y = 0, to.y = 1, heatmap = FALSE, n = 101)
{
  library('lattice')
  library('ggplot2')
  
	x.seq <- seq(from.x, to.x, (to.x - from.x) / n)
	y.seq <- seq(from.y, to.y, (to.y - from.y) / n)
	eval.points <- expand.grid(x.seq, y.seq)
	names(eval.points) <- c('x', 'y')
  eval.points <- transform(eval.points,
                           z = apply(eval.points,
                                     1,
                                     function (r) {f(r['x'], r['y'])}))
  if (heatmap)
  {
    p <- ggplot(eval.points, aes(x = x, y = y, fill = z)) + geom_tile()
    print(p)
  }
  else
  {
    print(wireframe(z ~ x * y, eval.points))
  }
}
