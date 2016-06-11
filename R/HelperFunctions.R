load_exp_SPR <- function(dir) {

 # load data from list 1
  d <- read.table(paste(dir, "data1.dat", sep="/"), header=F)
  colnames(d) <- c('subj', 'exp', 'item', 'cond', 'pos', 'word', 'corr', 'RT')
  d$list <- 1
  d.all <- d

  # load data from list 2
  d <- read.table(paste(dir, "data2.dat", sep="/"), header=F)
  colnames(d) <- c('subj', 'exp', 'item', 'cond', 'pos', 'word', 'corr', 'RT')
  d$list <- 2

  d$subj <- d$subj+100
  d.all <- rbind(d.all, d)
  
  # load data from list 3
  d <- read.table(paste(dir, "data3.dat", sep="/"), header=F)
  colnames(d) <- c('subj', 'exp', 'item', 'cond', 'pos', 'word', 'corr', 'RT')
  d$list <- 3
  
  #d <- subset(d, subj < 13)
  d$subj <- d$subj+200
  d.all <- rbind(d.all, d)
  
  # join these datasets
  d <- d.all

  # add total trial number
  trial.absName <- paste(d$exp, d$item)
  for(s in unique(d$subj)) {
    x <- trial.absName[d$subj==s]
    d[d$subj==s, 'trial.abs'] <- map(x, unique(x), 1:length(unique(x)))
  }

  d <- subset(d, exp!="practice")

  library(reshape)
  
  # COMPUTE false alarms, hits, D' and such, per subject, per experiment
  question <- "?"
  questions <- subset(d, pos==question & exp=="filler")
  accuracy <- with(questions, aggregate(list(count=corr), list(subj=subj, exp=exp, resp=asc(word), corr=corr), length))

  (accuracy <- accuracy[with(accuracy, order(subj, exp, resp, corr)),])
  map(with(accuracy, paste(resp, corr)), c("Y 1", "Y 0", "N 1", "N 0"), c('hit', 'FA', 'CR', 'miss')) -> accuracy$resp.status
  map(accuracy$resp.status, c('hit', 'FA', 'CR', 'miss'), c("Y", "N", "N", "Y")) -> accuracy$corr.resp

  (totals <- with(accuracy, aggregate(list(total=count), list(subj=subj, exp=exp, corr.resp=corr.resp), sum)))
  accuracy <- merge(accuracy, totals)
  accuracy$percentage <- accuracy$count/accuracy$total

  
  accuracy <- accuracy[,c('subj','exp','resp.status','percentage')]
  accuracy <- cast(accuracy, subj+exp ~ resp.status, mean)
  
  correction <- .005
  scaled.hits <- accuracy$hit*(1-2*correction)+correction
  scaled.FAs <- accuracy$FA*(1-2*correction)+correction
  accuracy$dprime <- qnorm(scaled.hits)-qnorm(scaled.FAs)

  accuracy1 <- accuracy[,c('subj','exp','dprime')]
  accuracy2 <- accuracy[,c('subj','exp','hit')]
  accuracy3 <- accuracy[,c('subj','exp','FA')]
  
  accuracy1 <- cast(accuracy1, subj ~ exp, mean)
  colnames(accuracy1)[2] <- c("filler.dprime")
  accuracy2 <- cast(accuracy2, subj ~ exp, mean)
  colnames(accuracy2)[2] <- c("filler.hit")
  accuracy3 <- cast(accuracy3, subj ~ exp, mean)
  colnames(accuracy3)[2] <- c("filler.FA")

  d <- merge(merge(merge(d, accuracy1), accuracy2), accuracy3)
  
  # extract race model experiment
  d.fillers <- subset(d, exp=="filler")
  d <- subset(d, exp=="RaceModels")

  # add total trial number
  trial.relName <- paste(d$exp, d$item)
  for(s in unique(d$subj)) {
    x <- trial.relName[d$subj==s]
    d[d$subj==s, 'trial.rel'] <- map(x, unique(x), 1:length(unique(x)))
  }

  d$trial.abs <- asi(d$trial.abs)
  d$trial.rel <- asi(d$trial.rel)
  d$trialGroup <- map(d$trial.rel, 1:36, c(rep(1,12), rep(2,12), rep(3,12)))

  # split sentence condition and question type
  cond <- unlist(strsplit(asc(d$cond), "-"))
  d$cond <-  cond[seq(1, length(cond), 2)]
  d$qtype <- cond[seq(2, length(cond), 2)]

  # map conditions to factors
  d$gen <- ifelse(d$cond%in%c('a','b','c'), 'fem', 'masc')
  d$att <- ifelse(d$cond%in%c('a','d'), 'NP1/NP2', ifelse(d$cond%in%c('b','e'), 'NP2', 'NP1'))

  # diagnostic: show number of item-condition pairs 
  with(subset(d, pos==0), tapply(RT, list(cond, item), length))


  # extract questions and join in a wide format with reading data
  q <- subset(d, pos=="?")
  q$corr <- asi(asc(q$corr))
  d <- subset(d, pos!="?")
  d$pos <- asi(asc(d$pos))

  with(d, tapply(RT, list(att, pos), function(x)round(mean((x)),0)))

  q <- q[,c('subj','item','cond','word', 'corr', 'RT')]
  colnames(q)[4:6] <- c('pressed.key', 'answ.corr', 'answ.RT')
  asi(map(asc(q$pressed.key), c('Y', 'N'), c(1,0))) -> q$pressed.key
  
  d <- merge(d, q, by=c('subj','item','cond'))
  
  # recode the answers to question np chosen NP
  asi(map(d$qtype, c('A', 'B', 'C', 'D'), c(0,1,0,1))) -> d$q.np2 
  !xor(d$q.np2, d$pressed.key) -> d$answ.np2

  # in NP1/NP2iguous sentences only Y is the right answer 
  d[d$qtype%in%c('A','B') & d$att=="NP1/NP2",'answ.corr'] <- d[d$qtype%in%c('A','B') & d$att=="NP1/NP2",'pressed.key']

  list(exp=d, fillers=d.fillers)
}

loadExp2 <- function(dir) {

 # load data 
  d <- read.table(paste(dir, "data.dat", sep="/"), header=F)
  colnames(d) <- c('subj', 'exp', 'item', 'cond', 'pos', 'word', 'corr', 'RT')


  # COMPUTE false alarms, hits, D' and such, per subject, per experiment 
  map(asc(d$exp), c('RaceModels', 'FakeRace'), rep('Race',2)) -> d$accuracy.exp
  questions <- subset(d, pos==question)
  accuracy <- with(questions, aggregate(list(count=corr), list(subj=subj, exp=accuracy.exp, resp=asc(word), corr=corr), length))

  (accuracy <- accuracy[with(accuracy, order(subj, exp, resp, corr)),])
  map(with(accuracy, paste(resp, corr)), c("Y 1", "Y 0", "N 1", "N 0"), c('hit', 'FA', 'CR', 'miss')) -> accuracy$resp.status
  map(accuracy$resp.status, c('hit', 'FA', 'CR', 'miss'), c("Y", "N", "N", "Y")) -> accuracy$corr.resp

  (totals <- with(accuracy, aggregate(list(total=count), list(subj=subj, exp=exp, corr.resp=corr.resp), sum)))
  accuracy <- merge(accuracy, totals)
  accuracy$percentage <- accuracy$count/accuracy$total

  accuracy <- accuracy[,c('subj','exp','resp.status','percentage')]
  accuracy <- cast(accuracy, subj+exp ~ resp.status, mean)
  accuracy$dprime <- qnorm(accuracy$hit)-qnorm(accuracy$FA)

  accuracy1 <- accuracy[,c('subj','exp','dprime')]
  accuracy2 <- accuracy[,c('subj','exp','hit')]
  accuracy3 <- accuracy[,c('subj','exp','FA')]
  
  accuracy1 <- cast(accuracy1, subj ~ exp, mean)
  colnames(accuracy1)[2:3] <- c("Race.dprime", "filler.dprime")
  accuracy2 <- cast(accuracy2, subj ~ exp, mean)
  colnames(accuracy2)[2:3] <- c("Race.hit", "filler.hit")
  accuracy3 <- cast(accuracy3, subj ~ exp, mean)
  colnames(accuracy3)[2:3] <- c("Race.FA", "filler.FA")

  d <- merge(merge(merge(d, accuracy1), accuracy2), accuracy3)
  
   # add total trial number
  trial.absName <- paste(d$exp, d$item)
  for(s in unique(d$subj)) {
    x <- trial.absName[d$subj==s]
    d[d$subj==s, 'trial.abs'] <- map(x, unique(x), 1:length(unique(x)))
  }
    
  # extract race model experiment
  d.not.race <- subset(d, exp=="RaceModels")
  d <- subset(d, exp=="RaceModels")

  # add total trial number
  trial.relName <- paste(d$exp, d$item)
  for(s in unique(d$subj)) {
    x <- trial.relName[d$subj==s]
    d[d$subj==s, 'trial.rel'] <- map(x, unique(x), 1:length(unique(x)))
  }

  d$trial.abs <- asi(d$trial.abs)
  d$trial.rel <- asi(d$trial.rel)
  d$trialGroup <- map(d$trial.rel, 1:36, c(rep(1,12), rep(2,12), rep(3,12)))

  # map conditions to factors
  d$gen <- ifelse(d$cond%in%c('a','b','c'), 'fem', 'masc')
  d$att <- ifelse(d$cond%in%c('a','d'), 'NP1/NP2', ifelse(d$cond%in%c('b','e'), 'NP2', 'NP1'))

  # diagnostic: show number of item-condition pairs 
  with(subset(d, pos==0), tapply(RT, list(asc(cond), item), length))

  # extract questions and join in a wide format with reading data
  q <- subset(d, pos=="?Grammatikalisch")
  q$corr <- asi(asc(q$corr))
  d <- subset(d, pos!="?Grammatikalisch" & asi(asc(pos)) < 10)
  d$pos <- asi(asc(d$pos))

  with(d, tapply(RT, list(att, pos), function(x)round(mean((x)),0)))

  q <- q[,c('subj','item','cond','word', 'corr', 'RT')]
  colnames(q)[4:6] <- c('pressed.key', 'answ.corr', 'answ.RT')
  asi(map(asc(q$pressed.key), c('Y', 'N'), c(1,0))) -> q$pressed.key
  
  d <- merge(d, q, by=c('subj','item','cond'))
  
  d
}

plotQuantileDiffs <- function(d, title, lwd, cex) {
  library(gplots)
  library(reshape)
  diff.m <- melt(with(d, tapply((RT), list(condition, percentile), mean)))
  colnames(diff.m) <- c("cond", "percentile", "RT.mean")
  diff.ci <- melt(with(d, tapply((RT), list(condition, percentile), ci.width)))
  colnames(diff.ci) <- c("cond", "percentile", "RT.ci")
  diff.x <- merge(diff.m, diff.ci)
  
  par(cex.lab=cex, cex.main=cex, cex.axis=cex)
  ymax <- max(diff.x$RT.mean); # ymax <- ymax + max(subset(diff.x, cond==0 & RT.mean==ymax)$RT.ci)
  ymin <- min(diff.x$RT.mean); # ymin <- ymin - min(subset(diff.x, cond==1 & RT.mean==ymin)$RT.ci)
  ylim <- c(ymin, ymax)
  
  with(subset(diff.x, cond%in%c('NP1/NP2iguous')),
  print(plotCI(RT.mean, percentile, , uiw=RT.ci, type="l", col="black",  ylim=ylim, lwd=lwd, main=title))); par(new=T)
  with(subset(diff.x, cond=='posessive'),
  print(plotCI(percentile, RT.mean, uiw=RT.ci, type="l", col="blue", ylim=ylim, lwd=lwd))); par(new=T)
  with(subset(diff.x, cond=='IO'),
  print(plotCI(percentile, RT.mean, uiw=RT.ci, type="l", col="green", ylim=ylim, lwd=lwd)));
}


quantilePlots <- function(d, intervals) {
  
  groups <- map(as.character(d$condition), letters[1:4], c('NP1/NP2iguous','NP1/NP2iguous','posessive','IO'))
  quantiles <- NULL
  quantile.steps <- seq(0.0, 1, 1/intervals)
  for(i in quantile.steps){
    spr <- melt(with(d, tapply(RT, list(subject, groups), quantile, probs=i)), 
            varnames=c("subject", "condition"))
    quantiles <- rbind(quantiles, cbind(percentile=i, spr, measure="SPR"))
  }

  colnames(quantiles)[4] <- "RT"
  quantiles <- subset(quantiles, !is.na(RT))

  #spr.samples <- with(word.det, tapply(RT, list(subject, condition), length))
  #lwd <- 7; cex <- 2;
  lwd <- 1; cex <- 1;
  plotQuantileDiffs(subset(quantiles, measure=="SPR"), title="SPR", lwd=lwd, cex=cex)
}


# From: http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

