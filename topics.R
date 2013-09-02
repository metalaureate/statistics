Sys.setenv(NOAWT=TRUE)
library("tm")
library("topicmodels")
library("XML")
library("Snowball")
library("statnet")
library("slam")
library("plyr")

Sys.setlocale("LC_COLLATE", "C")
rm(Terms1)
docs <- "/Users/shill/Desktop/SMM/feed.xml"

#install.packages("corpus.JSS.papers",repos = "http://datacube.wu.ac.at/", type = "source")
#data("JSS_papers", package = "corpus.JSS.papers")
#docdata<-JSS_papers
docdata<-xmlToDataFrame(docs) #my xml file

#docdata <- docdata[docdata[,"date"] < "2010-08-05",]
#docdata <- docdata[sapply(docdata[, "description"],Encoding) == "unknown",]
docdata<-as.data.frame(docdata)


remove_HTML_markup <-
  function(s) tryCatch({
    doc <- htmlTreeParse(paste("<!DOCTYPE html>", s),asText = TRUE, trim = FALSE)
    xmlValue(xmlRoot(doc))
  }, error = function(s) s)

corpus <- Corpus(VectorSource(sapply(docdata[, "description"], remove_HTML_markup)))

JSS_dtm <- DocumentTermMatrix(corpus,
                              control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
                                             removeNumbers = TRUE, removePunctuation = TRUE))

dim(JSS_dtm)

summary(col_sums(JSS_dtm))


term_tfidf <-
  tapply(JSS_dtm$v/row_sums(JSS_dtm)[JSS_dtm$i], JSS_dtm$j, mean) *log2(nDocs(JSS_dtm)/col_sums(JSS_dtm > 0))

med<-summary(term_tfidf)
med['Median']

JSS_dtm <- JSS_dtm[,term_tfidf >= (med['Median']*0.8)] #0.1 was the default (bit less than median term_tfidf)
JSS_dtm <- JSS_dtm[row_sums(JSS_dtm) > 0,]
summary(col_sums(JSS_dtm))
dim(JSS_dtm)

k <- 30 #number of topics
SEED <- 2010
jss_TM <- list(VEM = LDA(JSS_dtm, k = k, control = list(seed = SEED)),
               VEM_fixed = LDA(JSS_dtm, k = k,
                               control = list(estimate.alpha = FALSE, seed = SEED)),
               Gibbs = LDA(JSS_dtm, k = k, method = "Gibbs",
                           control = list(seed = SEED, burnin = 1000,
                                          thin = 100, iter = 1000)),
               CTM = CTM(JSS_dtm, k = k,
                         control = list(seed = SEED,
                                        var = list(tol = 10^-4), em = list(tol = 10^-3))))

sapply(jss_TM[1:2], slot, "alpha")
sapply(jss_TM, function(x)
  mean(apply(posterior(x)$topics,
             1, function(z) - sum(z * log(z)))))

Topic <- topics(jss_TM[["VEM"]], 1)
Terms1 <- terms(jss_TM[["VEM"]], 20)
Terms1




