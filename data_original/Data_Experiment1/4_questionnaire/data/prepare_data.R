
participants <- paste0("version_", 1:12, ".txt.csv")

library(plyr)

cnames <- c('exp', 'item', 'cond', 'corr.answ', 'stim', 'resp.subj1', 'resp.subj2')

d <- ldply(participants, function(fname) {
  d <- read.csv(fname, sep="\t", header=F)
  colnames(d) <- cnames[1:ncol(d)]
  if( ncol(d) < length(cnames) )
    d$resp.subj2 <- NA
  d$stim <- NULL
  d
})
      
d1 <- d[,c('exp', 'item', 'cond', 'corr.answ', 'resp.subj1')]
d2 <- d[,c('exp', 'item', 'cond', 'corr.answ', 'resp.subj2')]
colnames(d1)[5] <- 'resp'
colnames(d2)[5] <- 'resp'
d <- rbind(d1, d2)


subjects <- 1:(nrow(d)/72) 
d$subj <- rep(subjects, each=72)

d$resp <- ifelse(d$resp==1, 1, 0)
d.RC <- subset(d, exp=="RC")
d.fillers <- subset(d, exp=="filler")
d.fillers$corr.answ <- ifelse(d.fillers$corr.answ=="Y", 1, 0)

with(d.fillers, tapply(corr.answ==resp, subj, mean))

cond.attachment <- sapply(strsplit(asc(d.RC$cond), split="-"), function(x) x[1])
attachment <- nmap(asc(cond.attachment), c('a'='amb', 'b'='NP2', 'c'='NP1', 'd'='amb', 'e'='NP2', 'f'='NP1') )
rp.gender <- nmap(asc(cond.attachment), c('a'='f', 'b'='f', 'c'='f', 'd'='m', 'e'='m', 'f'='m') )
cond.question <- sapply(strsplit(asc(d.RC$cond), split="-"), function(x) x[2])
question.NP <- ifelse(cond.question=="A", "NP1", "NP2")

d.RC$attachment <- attachment
d.RC$rp.gender <- rp.gender
d.RC$question.NP <- question.NP

with(subset(d.RC, attachment!="amb"), tapply(resp, list(paste("Q",question.NP), attachment, subj), mean))

# remove subjects with bad data
# subj 1: 83% 'yes' to NP1-questions in NP2-attachment sentences, 0% 'yes' to NP2-questions in NP2-attachment sentences
# subj 10: 83% 'yes' to NP2-questions in NP1-attachment sentences.
# subj 24: 17% 'yes' to NP2-questions in NP2-attachment sentences, 67% 'yes' to NP1-questions in NP2-attachment sentences

with(subset(d.RC, !(subj %in%c(1,10,24))), tapply(resp, list(question.NP, attachment), mean))

save(d.RC, d.fillers, file="questionaire_data.rda")
