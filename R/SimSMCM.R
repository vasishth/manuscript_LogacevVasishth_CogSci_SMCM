

# Botstraps the UPM model and determines a statistic (mean by default). If fn.distfit is provided, it does parametric bootstrapping
simulateSMCMStatistic <- function(data, fn.choose, fn.distfit=NULL, fit.samedist=F, sim.n=10^6, fn.summary=mean) {
  d.sim <- ddply(data, .(subj), function(d) {
    print(d$subj[1])
    high.RT <- subset(d, att=="NP1")$RT
    low.RT <- subset(d, att=="NP2")$RT
    if(!is.null(fn.distfit)) {
      if(!fit.samedist) {
        rhigh <- fn.distfit(high.RT)
        rlow <- fn.distfit(low.RT)
      } else {
        rhigh <- fn.distfit(c(high.RT,low.RT))
        rlow <- rhigh
      }
    } else {
      rhigh <- function(n) sample(high.RT, size=n, replace=T)
      rlow  <- function(n) sample(low.RT, size=n, replace=T);
    }
    prg <- progress_text()
    print(date())
    prg$init(sim.n)
    N <- nrow(subset(d, att=="NP1/NP2"))
    sim.RT <- sapply(1:sim.n, function(i) {
      boot.RTs <- fn.choose(rhigh(N), rlow(N))
      prg$step()
      mean( boot.RTs )
    })
    prg$term()
    print(date())
    sim.RT
  })
}
