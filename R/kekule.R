# triangles?
n <- 2e2
pts <- cbind(runif(n), runif(n))

#' Generic X-Y Plotting
t0 <- function(x, y = NULL, ..., col = rgb(0, 0, 0, 0.1)) {
  x <- xy.coords(x, y)
  x <- do.call(cbind, x[c("x", "y")])
  index <- decido::earcut(x)

  ## we aren't grouping by NAs at all
  tri <- matrix(index, nrow = 3)
  xy <- x[rbind(tri, tri[1L, ], NA), ][-1L, ] ## close the triangle, separate from its neighbour, drop dummy NA
  plot(x, type = "n", ...)
  polygon(xy, col = col)
}


t0(pts)

s0 <- function(x, y = NULL, ..., col = rgb(0, 0, 0, 0.1)) {
  x <- do.call(cbind, xy.coords(x, y)[c("x", "y")])
  sq <- seq_len(dim(x)[1L])
  index <- cbind(sq[-length(sq)], sq[-1L])
  plot(x, type = "n")
  segments(x[index[, 1], 1], x[index[, 1], 2],
           x[index[, 2], 1], x[index[, 2], 2], col = col, ...)  ## par args need thought
}
s0(pts, col = sample(rainbow(n-1, alpha  = 0.5)), lwd = 5)


d0 <- function(x, y = NULL, ..., seg = NULL, col = rgb(0, 0, 0, 0.1),
               a = NULL, q = NULL, Y = FALSE, j = FALSE, D = FALSE,
               S = Inf, V = 0, Q = TRUE) {
  x <- xy.coords(x, y)
  x <- do.call(cbind, x[c("x", "y")])
  RTri <- RTriangle::triangulate(RTriangle::pslg(P = x, S = seg), a = a, q = q, Y = Y,  D = D, S = S, V = V, Q = Q)
  tri <- RTri$T
  ## warning, new vertices (potentially)
  x <- RTri$P
  xy <- x[t(cbind(tri, tri[, 1L], NA)), ]## close the triangle, separate from its neighbour
  plot(x, type = "n", ...)
  polygon(xy, col = col)

}

d0(pts, a= 0.001, D = TRUE)


## now a polygon


n <- 100
xx <- c(0:n, n:0)
yy <- c(c(0, cumsum(stats::rnorm(n))), rev(c(0, cumsum(stats::rnorm(n)))))
p <- cbind(xx, yy)
p <- p[!duplicated(as.data.frame(p)), ]
## we cannot duplicate the vertices
sq <- seq_len(dim(p)[1L])
index <- cbind(sq[-length(sq)], sq[-1L])
index <- rbind(index, c(index[length(index)], 1))  # Kekule
d0(p, seg = index)
d0(p, seg = index, a = 2, D = T)
lines(xx, yy, lwd  = 5, col = "firebrick")
