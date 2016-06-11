d <- read.table("data1.dat", header=F)
colnames(d) <- c('subj', 'exp', 'item', 'cond', 'pos', 'word', 'corr', 'RT')
d <- subset(d, subj < 13)
d.all <- d

d <- read.table("data2.dat", header=F)
colnames(d) <- c('subj', 'exp', 'item', 'cond', 'pos', 'word', 'corr', 'RT')
d <- subset(d, subj < 13)
d$subj <- d$subj+12

d.all <- rbind(d.all, d)
d <- d.all

# add total trial number
trial.abs <- paste(d$exp, d$item, d$cond)
d$trial.abs <- asi(map(trial.abs, unique(trial.abs), 1:length(unique(trial.abs))))

# extract race model experiment
d <- subset(d, exp=="RaceModels")

# add relative trial number
trial.rel <- paste(d$exp, d$item, d$cond)
d$trial.rel <- asi(map(trial.rel, unique(trial.rel), 1:length(unique(trial.rel))))

# split sentence condition and question type
cond <- unlist(strsplit(asc(d$cond), "-"))
d$cond <-  cond[seq(1, length(cond), 2)]
d$qtype <- cond[seq(2, length(cond), 2)]

# map conditions to factors
d$gen <- ifelse(d$cond%in%c('a','b','c'), 'fem', 'masc')
d$att <- ifelse(d$cond%in%c('a','d'), 'amb', ifelse(d$cond%in%c('b','e'), 'high', 'low'))

with(subset(d, pos==0), tapply(RT, list(cond, item), length))

# split questions and reading data
q <- subset(d, pos=="?")
q$corr <- asi(asc(q$corr))
d <- subset(d, pos!="?" & asi(asc(pos)) < 10)
d$pos <- asi(asc(d$pos))

with(d, tapply(RT, list(att, pos), function(x)round(mean((x)),0)))

q <- q[,c('subj','item','cond','word', 'corr', 'RT')]
colnames(q)[4:6] <- c('answ.key', 'answ.corr', 'answ.RT')
d <- merge(d, q, by=c('subj','item','cond'))


round(with(subset(d, pos==0), tapply(answ.RT, list(att, qtype), mean)))

map(d$qtype, c('A', 'B', 'C', 'D'), c(0,1,0,1)) -> d$q.np
map(d$answ.key, c('Y', 'N'), c(1,0)) -> d$q.np


with(d, tapply(RT, list(cond, pos), function(x)round(mean(x))))

library(lme4)

# relative pronoun
(m <- lmer(log(RT) ~ att+(1|gen)+(1|subj)+(1|item), d, subset=pos==4))

# auxiliary (end of RC)
(m <- lmer(log(RT) ~ att.amb+att.hilo+(1|gen)+(1|subj)+(1|item), d, subset=pos==4))

(m <- lmer(log(RT) ~ att+(1|gen)+(1|subj)+(1|item), d, subset=pos==8))



x <- with(d, tapply(RT, list(att, pos), round.mean))

x <- with(d, tapply(RT, list(gen, pos), round.mean))


quartz()
x <- with(d, tapply(RT, list(paste(ifelse(trial.rel<12, '<12', ifelse(trial.rel<24, '<24', '<36')), att), pos), round.mean))
matplot(t(x), type="l", lty=c(1,2,3), col=c('black','black','black','blue','blue','blue','green','green','green'), lwd=2)

d4 <- subset(d, pos==4)
d8 <- subset(d, pos==8)

# pos 8 (aux), speed
xlim <- c(0,.006)
plot(ecdf(1/subset(d8, att=='high')$RT),xlim=xlim, col="green"); par(new=T)
plot(ecdf(1/subset(d8, att=='low')$RT),xlim=xlim, col="blue"); par(new=T)
plot(ecdf(1/subset(d8a, att=='amb')$RT),xlim=xlim, col="black"); 

# pos 8 (aux), RTs
xlim <- c(0, 6600)
plot(ecdf(subset(d8, att=='high')$RT),xlim=xlim, col="green"); par(new=T)
plot(ecdf(subset(d8, att=='low')$RT),xlim=xlim, col="blue"); par(new=T)
plot(ecdf(subset(d8, att=='amb')$RT),xlim=xlim, col="black");

# pos 4 (RP), RTs by attachment type
xlim <- c(0, 2500)
plot(ecdf(subset(d4, att=='high')$RT),xlim=xlim, col="green"); par(new=T)
plot(ecdf(subset(d4, att=='low')$RT),xlim=xlim, col="blue"); par(new=T)
plot(ecdf(subset(d4, att=='amb')$RT),xlim=xlim, col="black"); 

# pos 4 (RP), RTs by gender
xlim <- c(0, 2500)
plot(ecdf(subset(d4, gen=='masc')$RT),xlim=xlim, col="green"); par(new=T)
plot(ecdf(subset(d4, gen=='fem')$RT), xlim=xlim, col="black");


plot(ecdf(subset(d, pos==8 & att.amb==-1)$RT))

#p1s <- c()
#p2s <- c()
#for(i in 1:1000) {
#  q <- rnorm(10000)
#  p <- rnorm(10000)
#  z <- rnorm(10000)
#  t <- t.test(p, q)
#  p1s <- c(p1s, t[['p.value']])
#  t <- t.test(p+z, q+z)
#  p2s <- c(p2s, t[['p.value']])
#}
