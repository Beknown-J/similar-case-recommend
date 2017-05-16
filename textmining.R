library(jiebaR)
library(tm)
library(dplyr)
library(plyr)
library(text2vec)
library(LDAvis)

source('little.func.R')
#####################数据预处理###########################
## 输入：文本文件
## 输出：清洗后的字符向量/文本文件  
## 细节处理：文本切割问题    
###  特殊字符的清洗  原始文本保留
# 界面展现
# 优化问题解决（周二）
# setwd("E:/R/textmining/data") # set path
train <- read.table("wb1.txt", header = FALSE, quote = "", sep = "\t", fill = T, colClasses = "character")
train <- data.frame(ids = rownames(train), train)

re_train <- ddply(train, .(ids), re_gsub)[, -2]
colnames(re_train) <- c('ids', 'query')
######################文本分词#############################
## jiebaR包
## 输入：文本文件/字符向量
## 输出：分词向量list
## 分词优化：1.专业术语词库扩充
##           2.词性标记
##           3.停用词扩充
##           4.命名实体识别
##           5.同义词扩充和替换
## 
cutter <- worker(type = 'tag',bylines = TRUE) # 词性标记  
train_seg <- segment(re_train$query, cutter) # 产出格式为每行一个分词向量的list，词性标注是向量的names
# train_seg[[1]]
# freq(train_seg[[1]])

# 提取词性包含n的词

train_seg <- llply(train_seg, n.sel)
# train_seg[[1]]

########################语料库创建、单词过滤########################
# text2vec包
## 输入：分词向量list
## 输出：DTM矩阵/语料库
## 过程：创建语料库,DF词频过滤
### 过滤词的保存和检查
train_it <- itoken(train_seg, tokenizer = word_tokenizer, ids = train$ids) # text2vec包中用于创建迭代器到DTM矩阵或语料库中
train_v <- create_vocabulary(train_it) 
# subset(train_v$vocab, doc_counts > 100) # doc数量超过100个的词
train_clean_v <- prune_vocabulary(train_v, doc_proportion_max = 0.5, doc_proportion_min = 1/nrow(train)) #去掉出现的文章数占比太低和太高的词语
# subset(train_clean_v$vocab, doc_counts > 100)
train_vectorizer <- vocab_vectorizer(train_clean_v)  # 构建DTM/TCM/语料库
train_dtm <- create_dtm(train_it, train_vectorizer) # 计算词频矩阵
train_dtm_norm <- normalize(train_dtm,"l1")     ## 对TF归一化处理

####################文本特征提取###############################
### 过程：TF*IDF计算，设定N值提权特征词

## 计算TF值
#train_tf<- list()
#for(j in 1:length(train_seg))
#{
#  train_tf[[j]]<-freq(train_seg[[j]])
# }
## 计算DF值
#vab <- train_v$vocab
#train_df<-data_frame(vab$terms,vab$doc_counts) 

## 计算TF*IDF矩阵
dtm_tfidf <- transform_tfidf(train_dtm_norm,idf = get_idf(train_dtm)) 
## 提取关键词，对每篇文章的tf*idf值进行排序，提取每篇文章的前50个单词作为特征向量

dtm_tfidf_sort <- alply(dtm_tfidf, 1, sort50, nt= 100)  # 提取前50个词作为特征词


#####################相似度计算###################################
### 过程：基于TF*IDF值得到文档的向量空间，进而计算文档间的余弦值
sim_it<-itoken(dtm_tfidf_sort,tokenizer = word_tokenizer,ids=train$ids)
sim_v<-create_vocabulary(sim_it)
sim_vectorizer<- vocab_vectorizer(sim_v)
sim_dtm<- create_dtm(sim_it,sim_vectorizer)
## sim<- sim2(sim_dtm,method = "cosine") # 相似度计算


### 新进来一篇文章，提取了特征词向量a后，与文本库进行相关度计算比较
a <- sim_dtm[1,]
sim_new<- sim2(sim_dtm,t(a),method = "cosine") # 相似度计算
sort(sim_new,decreasing = TRUE)

#####################文档推荐###################################
### 过程：基于文档的相似度进行排序，选择相似度大的文章进行推荐
sim_index <- order(sim_new,decreasing = TRUE)[1:10]  #提取相似度比较高的前10篇文章对应的文本编号
doc_recommend <- train[sim_index,]
head(doc_recommend[[2]])

#####################LDA模型计算###################################
## 输入：DTM矩阵
## 输出: 
## 过程：基于dtm矩阵，计算doc-topic矩阵和world-topic矩阵

# train_dtm = create_dtm(train_it, train_vectorizer,'lda_c')
train_it_key <- itoken(dtm_tfidf_sort,tokenizer = word_tokenizer,ids=train$ids) ### text2vec包中用于创建迭代器到DTM矩阵或语料库中
train_v_key <- create_vocabulary(train_it_key) 

LDA_train_dtm <- create_dtm(train_it_key, vocab_vectorizer(train_v_key)) # 计算词频矩阵

n_topics=10
train_vocabulary_key = train_v_key
doc_topic_prior = (1/n_topics)
topic_word_prior = (1/n_topics)
train_ldamodel = LatentDirichletAllocation$new(n_topics = n_topics, 
                                               vocabulary = train_vocabulary_key,
                                               doc_topic_prior = doc_topic_prior,
                                               topic_word_prior = topic_word_prior
) ##搭个LDA的架子
##　构建文档—主题矩阵（先fit拟合lda模型，再tansform转换成doc—topic矩阵）
train_dtmatrix = train_ldamodel$fit_transform(LDA_train_dtm, n_iter =20, check_convergence_every_n = 5)  
train_dt=as.data.frame(t(apply(train_dtmatrix,1,function(x) x/sum(x)))) ###文档属于每个主题的概率
## 获取词-主题矩阵（get_world_vectors)
train_world_vector<- train_ldamodel$get_word_vectors()



