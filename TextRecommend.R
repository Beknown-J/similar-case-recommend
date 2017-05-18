########## Initialization ##########
rm(list = ls())
library(jiebaR)
library(tm)
library(plyr)
library(dplyr)
library(text2vec)
library(LDAvis)
library(stringr)
source('little.func.R')

########## Data(text) read and clean ##########
train <- read.table("wb1.txt", header = FALSE, quote = "", sep = "\t", fill = T, colClasses = "character")
train <- data.frame(ids = rownames(train), train)
pre_train <- ddply(train, .(ids), re_gsub)[, -2]  ## re_gsub from 'little.func.R'
colnames(pre_train) <- c('ids', 'query')

########## Regular expression preprocessing ##########
regular1 <- "([0-9]{4}|同)年(([0-9]{1,2})月)?([0-9]{1,2}日|初|末)?"
regular2 <-  "(人民币)?(\\d+(\\.\\d+)?|[百千万亿]{1,2})+元"
regular3 <-  ".+省.+市.+人民法院.+书（\\d{4}）.{1}\\d+.{1}[初更终]{1}\\d+号"
regular4 <- "[一二].{3}年[一二三四五六七八九十]{1,2}月[一二三四五六七八九十]{1,2}日"
regular5 <- "(审判长.*)?审判员.*"

regular <- c(regular1, regular2, regular3, regular4, regular5)
# regular1_vect <- str_extract_all(pre_train$query, regular1)
# regular4_vect <- str_extract_all(pre_train$query, regular4)

re_train <- ddply(pre_train, .(ids), regular.gsub, reg = regular)[, -2]  ## re_gsub from 'little.func.R'
colnames(re_train) <- c('ids', 'query')

########## Text processing ##########
cutter <- worker(type = 'tag', bylines = TRUE, stop_word = 'stop_word.txt', user = 'usr_word.txt')   
train_seg <- segment(re_train$query, cutter) 

train_seg_clean <- llply(train_seg, reglar.del)  ## delete one character
##
labels <- c("n", "v", "n\r")
train_seg_clean <- llply(train_seg_clean, n.sel, label = labels)

# ##
# numb <- 5
# filefreq_init <- freq(train_seg_clean[[numb]])   ## initial word frequence
# filefreq_init <- filefreq_init[order(filefreq_init$freq, decreasing = T), ]
# 
# ##

##
train_it <- itoken(train_seg_clean, tokenizer = word_tokenizer, ids = re_train$ids)
train_v <- create_vocabulary(train_it) 
train_clean_v <- prune_vocabulary(train_v, doc_proportion_max = 0.3, doc_proportion_min = 2/nrow(train), term_count_min = 2)
train_vectorizer <- vocab_vectorizer(train_clean_v)
train_dtm <- create_dtm(train_it, train_vectorizer)

### add weights for special words
major_name <- c("妨害公务罪", "行政执法")  ## the words which is keys words for recommendation
major_vect <- which(colnames(train_dtm) %in% major_name)
###

########## TF-IDF model training and create keywords corpus###########
model_tfidf <- TfIdf$new()
model_tfidf$fit(train_dtm)
dtm_r <- model_tfidf$transform(train_dtm)

k <- 100  ## the number of keywords
dtm_tfidf_sort <- alply(dtm_r, 1, sort.dat, nt= k)
sim_it <- itoken(dtm_tfidf_sort,tokenizer = word_tokenizer,ids=train$ids)
sim_v <- create_vocabulary(sim_it)
sim_dtm <- create_dtm(sim_it,vocab_vectorizer(sim_v)) ## ?type = "lda_c"

########### Text recommendation ------ In sample ###########
file_numb1 <- sim_dtm[2,]
sim_new<- sim2(sim_dtm,t(file_numb1),method = "cosine")   ## use cosin distance
sim_index <- order(sim_new,decreasing = TRUE)[1:10]  #提取相似度比较高的前10篇文章对应的文本编号
doc_recommend <- train[sim_index,]

## show recommendations
doc_recommend[[1]]
dtm_tfidf_sort[[2]]
dtm_tfidf_sort[[160]]
dtm_tfidf_sort[[1672]]
dtm_tfidf_sort[[1629]]
dtm_tfidf_sort[[2255]]
dtm_tfidf_sort[1454,2]



####------------------- Out of sample -----------------------
########## Initialization ##########
rm(list = ls())
library(jiebaR)
library(tm)
library(plyr)
library(dplyr)
library(text2vec)
library(LDAvis)

source('little.func.R')

########## Data(text) read and clean ##########
#### training data
train_dat <- read.table("wb2.txt", header = FALSE, quote = "", sep = "\t", fill = T, colClasses = "character")
train_dat <- data.frame(ids = rownames(train_dat), train_dat)
retrain_dat <- ddply(train_dat, .(ids), re_gsub)[, -2]  ## re_gsub from 'little.func.R'
colnames(retrain_dat) <- c('ids', 'query')

#### testing data
test_dat <- read.table("wb1.txt", header = FALSE, quote = "", sep = "\t", fill = T, colClasses = "character")
test_dat <- data.frame(ids = rownames(test_dat), test_dat)
retest_dat <- ddply(test_dat, .(ids), re_gsub)[, -2]  ## re_gsub from 'little.func.R'
colnames(retest_dat) <- c('ids', 'query')

########## Text processing ##########
#### training data
cutter <- worker(type = 'tag', bylines = TRUE, stop_word = 'stop_word.txt', user = 'usr_word.txt')   
train_seg <- segment(retrain_dat$query, cutter)

train_seg_clean <- llply(train_seg, reglar.del)
train_it <- itoken(train_seg_clean, tokenizer = word_tokenizer, ids = retrain_dat$ids)
train_v <- create_vocabulary(train_it) 
train_clean_v <- prune_vocabulary(train_v, doc_proportion_max = 0.5, doc_proportion_min = 1/nrow(retrain_dat), term_count_min = 2)
train_dtm <- create_dtm(train_it, vocab_vectorizer(train_clean_v))

########## TF-IDF model training and create keywords corpus###########
model_tfidf <- TfIdf$new()
model_tfidf$fit(train_dtm)
dtm_r <- model_tfidf$transform(train_dtm)

k <- 100  ## the number of keywords
dtm_tfidf_sort <- alply(dtm_r, 1, sort50, nt= k)   ## get the keywords of the text
sim_it <- itoken(dtm_tfidf_sort,tokenizer = word_tokenizer,ids=retrain_dat$ids)
sim_v <- create_vocabulary(sim_it)
sim_dtm <- create_dtm(sim_it,vocab_vectorizer(sim_v))

########## preparing testing data
cutter2 <- worker(type = 'tag', bylines = TRUE, stop_word = 'stop_word.txt', user = 'usr_word.txt')   
test_seg <- segment(retest_dat$query, cutter2)
test_seg_clean <- llply(test_seg, reglar.del)

test_it <- itoken(test_seg_clean, tokenizer = word_tokenizer, ids = retest_dat$ids)
test_dtm <- create_dtm(test_it, vocab_vectorizer(train_clean_v))  ## attention: here use the cropus of train_dat 
dtm_test <- model_tfidf$transform(test_dtm)  ## attention: here use the fitted model_tfidf model to calculate test data's tf-idf value
k <- 100
dtm_test_tfidf <- alply(dtm_test, 1, sort50, nt= k)   ## get the keywords of the text
sim_test_it <- itoken(dtm_test_tfidf, tokenizer = word_tokenizer,ids=retest_dat$ids)
sim_test_dtm <- create_dtm(sim_test_it,vocab_vectorizer(sim_v))

########### Text recommendation ------ out of sample ###########
file1_sim_test_dtm <- sim_test_dtm[1,]
sim_cosin <- sim2(sim_dtm, t(file1_sim_test_dtm), method = "cosine")   ## use cosin distance

sim_cosin_sort <- order(sim_cosin, decreasing = TRUE)[1:10]  #提取相似度比较高的前10篇文章对应的文本编号
doc_recommend <- retrain_dat[sim_cosin_sort, ]

## show recommendations
doc_recommend[[1]]
retest_dat[1, 2]
retrain_dat[12384, 2]
retrain_dat[1473, 2]
retrain_dat[14176, 2]
retrain_dat[10355, 2]
retrain_dat[14865, 2]
retrain_dat[1454,2]




