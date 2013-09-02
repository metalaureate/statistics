setwd('/Users/shill/documents/R scripts')

library(psych)
data <- read.csv("uninstall_msgs.csv", header=T)
data<-data[2:10]
c<-cor(data[], use="complete.obs", method="pearson") 
write.table(c,file="msg_uninstall_cor.csv",sep=",",row.names=T)

d<-corr.test(data, use="complete")

write.table(d,file="msg_uninstall_cor_sig.csv",sep=",",row.names=T)
