do.call('rbind', lapply(readLines(pipe('pbpaste')), function(i) strsplit(i, '\t')[[1]])) -> a
a<-as.data.frame(a)
for (i in 1:ncol(a)) { a[,i] <- as.numeric(as.character(a[,i]))}
t.test(a$V1,a$V2)