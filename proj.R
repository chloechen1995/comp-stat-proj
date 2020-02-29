head(iris)
setosa <- iris[iris$Species == 'setosa',]
# develop some kde algorithm
# we have options of: data, bandwidth, kernel choice
# for now, we can use just the Gaussian kernel
# we can try to attempt bandwidth choice / cross-validation

# step 1, apply KDE with arbitrary bandwidth and Gauss kernel
# try to see if plot looks right

hist(setosa$Sepal.Length)
normal <- function(z) {
  exp((-z^2)/2) / sqrt(2 * pi)
}

kde <- function(x = setosa$Sepal.Length, h, K = normal) {
  n <- length(x)
  
  range_delta <- (range(x)[2] - range(x)[1]) / 3
  x_limits <- c(range(x)[1] - range_delta, range(x)[2] + range_delta)

  # Reference: https://stackoverflow.com/questions/4785657/how-to-draw-an-empty-plot
  plot(1, type="n", xlab="", ylab="", xlim=x_limits, ylim=c(-0.05, 1))
  abline(h = 0)
  points(x)
}


