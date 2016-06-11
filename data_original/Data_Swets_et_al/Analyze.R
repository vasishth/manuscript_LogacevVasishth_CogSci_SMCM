
bootstrap.ci.halfwidth <- function (scores, n.sample) {
  sd.of.mean <- sd(scores)
  return(qt(0.975, df = n.sample - 1) * sd.of.mean)
}
bootstrap.ci.t <- function(scores, n.sample) {
  m <- mean(scores)
  w <- bootstrap.ci.halfwidth(scores, n.sample)
  upper <- m + w
  lower <- m - w
  return(data.frame(lower = lower, mean=m, upper = upper))
}

bootstrap.ci.quant <- function(scores) {
  m <- mean(scores)
  upper <- quantile(scores, .975)[[1]]
  lower <- quantile(scores, .025)[[1]]
  return(data.frame(lower = lower, mean=m, upper = upper))
}
is.in.bootstrap.ci.quant <- function(scores, val) {
  ci <- bootstrap.ci.quant(scores)
  val >= ci$lower & val <= ci$upper
}
  
run.once <- function(n, sim.n) {
  a <- rnorm(n)
  b <- rnorm(n)
  c <- pmin(rnorm(n), rnorm(n))
  d <- sapply(1:sim.n, function(idx) mean( pmin(sample(a, n, replace=T), sample(b, n, replace=T))) )
#  print(quantile(d, c(.025, .975)))
#  ci <- bootstrap.ci.quant(d)
  ci <- bootstrap.ci.t(d, n)
  ci.data <- ci( c )
  (mean(c) < ci$lower || mean(c) > ci$upper) & (mean(d) < ci.data$lower || mean(d) > ci.data$upper)
}


n <- 12; sim.n <- 10^4
x <- sapply(1:10^3, function(idx) run.once(n, sim.n))

mean(x)