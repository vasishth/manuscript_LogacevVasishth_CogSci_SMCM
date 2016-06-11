
fitLognormal2 <- function(RTs) {
  fn <- function(n) rlnorm(n, meanlog=LogMean(mean(RTs), sd(RTs)), sdlog=LogSD(mean(RTs), sd(RTs)))
  return(fn)
}

fitExGaus <- function(RTs, method="Nelder-Mead") {
  if(length(method)==0) 
    return(NULL)
  tiny.number <- .1^10
  logLikExGaus <- function(p, RTs) sum(gamlss.dist::dexGAUS(RTs, mu=exp(p[['mu']]), sigma=exp(p[['sigma']])+tiny.number, nu=exp(p[['nu']]), log=T))
  
  # starting values from Lacouture&Cousineau (2008)
  library(modeest)
  nu <- sd(RTs)*.8;
  mu <- mean(RTs)-skewness(RTs)
  sigma <- sqrt(var(RTs)-nu^2)
  init.par <- c(mu=mu, sigma=sigma, nu=nu)
  res <- optim(log(init.par), logLikExGaus, RTs=RTs, control=list(fnscale=-1, maxit=10^5), method=method[1])
  res <- optim(res$par, logLikExGaus, RTs=RTs, control=list(fnscale=-1, maxit=10^5), method=method[1])
  par <- exp(res$par); par[['sigma']] <- par[['sigma']]+tiny.number
  if(res$convergence!=0) {
    print(res)
    warning("fitExGaus: optim() did not converge.")
    fn <- function(n) rep(NA, n)
    return(fn)
  } else {
    print(par)
  }
  fn <- function(n) rnorm(n, mean=par['mu'], sd=par['sigma']) + rexp(n,1/par['nu'])
  return(fn)
}
