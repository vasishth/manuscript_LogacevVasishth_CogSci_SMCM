
d <- read.table("all.txt")
colnames(d) <- c("item", "cond", "qitem")

restrict <- function(x, max) {
  ((x-1) %% max) +1
}
assign.versions <- function(item, condition, versions) {
  restrict( (item-1)+condition, versions )
}

d$version <- assign.versions(d$item, as.integer(d$cond), length(unique(d$cond)))
d <- d[,c('item','cond','version','qitem')]
d$exp <- 'RC'
d$answ <- NA


f <- read.table("Fillers.txt")
colnames(f) <- c("exp", "item", "answ", "qitem")
f$cond <- '-'
head(f)

library(plyr)

ddply(d, .(version), function(d) {
  d.cur <- rbind(d[,c('exp','item','cond','answ','qitem')], f[,c('exp','item','cond','answ','qitem')])
  d.rand <- d.cur[sample(1:nrow(d.cur)),]
  write.table(d.rand, file=sprintf("version_%d.txt", d$version[1]), row.names=F, col.names=F)
  write.table(d.rand[,'qitem'], file=sprintf("version_%d.tex", d$version[1]), row.names=F, col.names=F)
})


