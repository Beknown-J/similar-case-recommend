a1 <- c('刑法','人民')
a2 <- c(1,2)
sentiment <- data.frame(a1,a2)

sentiment$doc=apply(sentiment,1,function(x) paste("http://192.168.45.150:9010/?words=",x[1],sep = ""))
similar=lapply(sentiment$doc, function(x) head(read.table(x,header = FALSE,quote="\"", comment.char="", stringsAsFactors=FALSE),6))
similar1=lapply(similar,function(x) gsub("\\[", "", x$V1))
similar1=lapply(similar1,function(x) gsub("\\{", "", x))
sentiment$similar=similar1
sentiment$similarLen=apply(sentiment,1,function(x) length(x[4][[1]]))
word=rep(sentiment[,1],times=sentiment$similarLen)
type=rep(sentiment[,2],times=sentiment$similarLen)
similar2=unlist(similar1)
result=data.frame(word=word,type=type,similar=similar2)
