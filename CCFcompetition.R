
##############竞赛####################
library(jiebaR)
library(tm)
library(dplyr)
library(text2vec)
library(LDAvis)
library(xgboost)
library(ROCR)
###########################################训练集##################################################
setwd("K:/work/CCF/sogo") #home_dir

setwd("E:/R/programme/20160929CFFcompetition/data") ## office_dir
train=read.table("user_tag_query.2W.TRAIN",header = FALSE,quote="",sep = "\t",fill=T,col.names = paste("V", 1:500, sep = ""))
train$query=paste(train$V5,train$V6,train$V7,train$V8,train$V9,train$V10,train$V11,train$V12,train$V13,train$V14,train$V15,train$V16,train$V17,train$V18,train$V19,train$V20,train$V21,train$V22,train$V23,train$V24,train$V25,train$V26,train$V27,train$V28,train$V29,train$V30,train$V31,train$V32,train$V33,train$V34,train$V35,train$V36,train$V37,train$V38,train$V39,train$V40,train$V41,train$V42,train$V43,train$V44,train$V45,train$V46,train$V47,train$V48,train$V49,train$V50,train$V51,train$V52,train$V53,train$V54,train$V55,train$V56,train$V57,train$V58,train$V59,train$V60,train$V61,train$V62,train$V63,train$V64,train$V65,train$V66,train$V67,train$V68,train$V69,train$V70,train$V71,train$V72,train$V73,train$V74,train$V75,train$V76,train$V77,train$V78,train$V79,train$V80,train$V81,train$V82,train$V83,train$V84,train$V85,train$V86,train$V87,train$V88,train$V89,train$V90,train$V91,train$V92,train$V93,train$V94,train$V95,train$V96,train$V97,train$V98,train$V99,train$V100,train$V101,train$V102,train$V103,train$V104,train$V105,train$V106,train$V107,train$V108,train$V109,train$V110,train$V111,train$V112,train$V113,train$V114,train$V115,train$V116,train$V117,train$V118,train$V119,train$V120,train$V121,train$V122,train$V123,train$V124,train$V125,train$V126,train$V127,train$V128,train$V129,train$V130,train$V131,train$V132,train$V133,train$V134,train$V135,train$V136,train$V137,train$V138,train$V139,train$V140,train$V141,train$V142,train$V143,train$V144,train$V145,train$V146,train$V147,train$V148,train$V149,train$V150,train$V151,train$V152,train$V153,train$V154,train$V155,train$V156,train$V157,train$V158,train$V159,train$V160,train$V161,train$V162,train$V163,train$V164,train$V165,train$V166,train$V167,train$V168,train$V169,train$V170,train$V171,train$V172,train$V173,train$V174,train$V175,train$V176,train$V177,train$V178,train$V179,train$V180,train$V181,train$V182,train$V183,train$V184,train$V185,train$V186,train$V187,train$V188,train$V189,train$V190,train$V191,train$V192,train$V193,train$V194,train$V195,train$V196,train$V197,train$V198,train$V199,train$V200,train$V201,train$V202,train$V203,train$V204,train$V205,train$V206,train$V207,train$V208,train$V209,train$V210,train$V211,train$V212,train$V213,train$V214,train$V215,train$V216,train$V217,train$V218,train$V219,train$V220,train$V221,train$V222,train$V223,train$V224,train$V225,train$V226,train$V227,train$V228,train$V229,train$V230,train$V231,train$V232,train$V233,train$V234,train$V235,train$V236,train$V237,train$V238,train$V239,train$V240,train$V241,train$V242,train$V243,train$V244,train$V245,train$V246,train$V247,train$V248,train$V249,train$V250,train$V251,train$V252,train$V253,train$V254,train$V255,train$V256,train$V257,train$V258,train$V259,train$V260,train$V261,train$V262,train$V263,train$V264,train$V265,train$V266,train$V267,train$V268,train$V269,train$V270,train$V271,train$V272,train$V273,train$V274,train$V275,train$V276,train$V277,train$V278,train$V279,train$V280,train$V281,train$V282,train$V283,train$V284,train$V285,train$V286,train$V287,train$V288,train$V289,train$V290,train$V291,train$V292,train$V293,train$V294,train$V295,train$V296,train$V297,train$V298,train$V299,train$V300,train$V301,train$V302,train$V303,train$V304,train$V305,train$V306,train$V307,train$V308,train$V309,train$V310,train$V311,train$V312,train$V313,train$V314,train$V315,train$V316,train$V317,train$V318,train$V319,train$V320,train$V321,train$V322,train$V323,train$V324,train$V325,train$V326,train$V327,train$V328,train$V329,train$V330,train$V331,train$V332,train$V333,train$V334,train$V335,train$V336,train$V337,train$V338,train$V339,train$V340,train$V341,train$V342,train$V343,train$V344,train$V345,train$V346,train$V347,train$V348,train$V349,train$V350,train$V351,train$V352,train$V353,train$V354,train$V355,train$V356,train$V357,train$V358,train$V359,train$V360,train$V361,train$V362,train$V363,train$V364,train$V365,train$V366,train$V367,train$V368,train$V369,train$V370,train$V371,train$V372,train$V373,train$V374,train$V375,train$V376,train$V377,train$V378,train$V379,train$V380,train$V381,train$V382,train$V383,train$V384,train$V385,train$V386,train$V387,train$V388,train$V389,train$V390,train$V391,train$V392,train$V393,train$V394,train$V395,train$V396,train$V397,train$V398,train$V399,train$V400,train$V401,train$V402,train$V403,train$V404,train$V405,train$V406,train$V407,train$V408,train$V409,train$V410,train$V411,train$V412,train$V413,train$V414,train$V415,train$V416,train$V417,train$V418,train$V419,train$V420,train$V421,train$V422,train$V423,train$V424,train$V425,train$V426,train$V427,train$V428,train$V429,train$V430,train$V431,train$V432,train$V433,train$V434,train$V435,train$V436,train$V437,train$V438,train$V439,train$V440,train$V441,train$V442,train$V443,train$V444,train$V445,train$V446,train$V447,train$V448,train$V449,train$V450,train$V451,train$V452,train$V453,train$V454,train$V455,train$V456,train$V457,train$V458,train$V459,train$V460,train$V461,train$V462,train$V463,train$V464,train$V465,train$V466,train$V467,train$V468,train$V469,train$V470,train$V471,train$V472,train$V473,train$V474,train$V475,train$V476,train$V477,train$V478,train$V479,train$V480,train$V481,train$V482,train$V483,train$V484,train$V485,train$V486,train$V487,train$V488,train$V489,train$V490,train$V491,train$V492,train$V493,train$V494,train$V495,train$V496,train$V497,train$V498,train$V499,train$V500
                  ,sep = "") ##把原来是column的query连起来
train=data.frame(train$V1,train$V2,train$V3,train$V4,train$query)
colnames(train)=c("uid","age","gender","education","query")
train$query=sub(" +$", "", train$query) ##过滤query末尾空格
train$uid=as.character(train$uid)
train=subset(train,nchar(uid)==32)
train=train[order(train$uid),]

##age分布
# 0    1    2    3    4    5    6 
# 355  7900 5330 3603 2141 589  82

##gender分布
# 0     1      2 
# 424   11365  8211

##edu分布
# 0    1    2    3    4    5    6 
# 1878 65   119  3722 5579 7487 1150

##---------------------------------------------------------------load data
cutter = worker("tag") #标注词性
cutter$bylines = TRUE
train_seg=segment(train$query,cutter) ##产出格式为每行一个分词向量的list，词性标注是向量的names

i=1
while(i<=20000){
  train_seg[[i]]=enc2utf8(train_seg[[i]][which(names(train_seg[[i]])=="n")]);
  i=i+1
} ###只提取词性为n的词


train_it = itoken(train_seg,tokenizer = word_tokenizer,ids=train$uid)
train_v = create_vocabulary(train_it)
##subset(v$vocab,doc_counts>10) ## 观察词doc数量超过10个的词
train_clean_v=prune_vocabulary(train_v, doc_proportion_max = 0.2,doc_proportion_min=(2/nrow(train)))##去掉出现的文章数占比太低和太高的词语
###clean_v$vocab
train_vectorizer = vocab_vectorizer(train_clean_v)
train_dtm = create_dtm(it, vectorizer)
dtm_tfidf = transform_tfidf(train_dtm)
#dim(dtm_tfidf)
#head(dimnames(dtm_tfidf)[[2]])
##---------------------------------得到tf-idf矩阵（这个矩阵不适用于LDA）
train_dtm = create_dtm(train_it, train_vectorizer,'lda_c')
n_topics=10
train_vocabulary = train_clean_v
doc_topic_prior = (1/n_topics)
topic_word_prior = (1/n_topics)
train_ldamodel = LatentDirichletAllocation$new(n_topics = n_topics, 
                                               vocabulary = train_vocabulary,
                                               doc_topic_prior = doc_topic_prior,
                                               topic_word_prior = topic_word_prior
) ##搭个LDA的架子
train_dtmatrix = train_ldamodel$fit_transform(train_dtm, n_iter =20, check_convergence_every_n = 5)
##把矩阵扔进去fit,此时上一步建立的架子也已经改变，而train_dtmatrix则是一个文档-主题结果矩阵
##train_ldamodel$get_word_vectors()[1:5,]
##get_word_vectors 是词-主题矩阵
train_dt=as.data.frame(t(apply(train_dtmatrix,1,function(x) x/sum(x)))) ###文档属于每个主题的概率
train_dt=cbind(train_dt,train[,2:4]) ##加上年龄性别学历
##table(train_dt$age)
fix(train_dt)## 把age,gender,education调成numeric

#########################################测试集######################################################
setwd("E:/R/programme/20160929CFFcompetition/data") ## office_dir
test=read.table("user_tag_query.2W.TEST",header = FALSE,quote="",sep = "\t",fill=T,col.names = paste("V", 1:500, sep = ""))
test$query=paste(test$V2,test$V3,test$V4,test$V5,test$V6,test$V7,test$V8,test$V9,test$V10,test$V11,test$V12,test$V13,test$V14,test$V15,test$V16,test$V17,test$V18,test$V19,test$V20,test$V21,test$V22,test$V23,test$V24,test$V25,test$V26,test$V27,test$V28,test$V29,test$V30,test$V31,test$V32,test$V33,test$V34,test$V35,test$V36,test$V37,test$V38,test$V39,test$V40,test$V41,test$V42,test$V43,test$V44,test$V45,test$V46,test$V47,test$V48,test$V49,test$V50,test$V51,test$V52,test$V53,test$V54,test$V55,test$V56,test$V57,test$V58,test$V59,test$V60,test$V61,test$V62,test$V63,test$V64,test$V65,test$V66,test$V67,test$V68,test$V69,test$V70,test$V71,test$V72,test$V73,test$V74,test$V75,test$V76,test$V77,test$V78,test$V79,test$V80,test$V81,test$V82,test$V83,test$V84,test$V85,test$V86,test$V87,test$V88,test$V89,test$V90,test$V91,test$V92,test$V93,test$V94,test$V95,test$V96,test$V97,test$V98,test$V99,test$V100,test$V101,test$V102,test$V103,test$V104,test$V105,test$V106,test$V107,test$V108,test$V109,test$V110,test$V111,test$V112,test$V113,test$V114,test$V115,test$V116,test$V117,test$V118,test$V119,test$V120,test$V121,test$V122,test$V123,test$V124,test$V125,test$V126,test$V127,test$V128,test$V129,test$V130,test$V131,test$V132,test$V133,test$V134,test$V135,test$V136,test$V137,test$V138,test$V139,test$V140,test$V141,test$V142,test$V143,test$V144,test$V145,test$V146,test$V147,test$V148,test$V149,test$V150,test$V151,test$V152,test$V153,test$V154,test$V155,test$V156,test$V157,test$V158,test$V159,test$V160,test$V161,test$V162,test$V163,test$V164,test$V165,test$V166,test$V167,test$V168,test$V169,test$V170,test$V171,test$V172,test$V173,test$V174,test$V175,test$V176,test$V177,test$V178,test$V179,test$V180,test$V181,test$V182,test$V183,test$V184,test$V185,test$V186,test$V187,test$V188,test$V189,test$V190,test$V191,test$V192,test$V193,test$V194,test$V195,test$V196,test$V197,test$V198,test$V199,test$V200,test$V201,test$V202,test$V203,test$V204,test$V205,test$V206,test$V207,test$V208,test$V209,test$V210,test$V211,test$V212,test$V213,test$V214,test$V215,test$V216,test$V217,test$V218,test$V219,test$V220,test$V221,test$V222,test$V223,test$V224,test$V225,test$V226,test$V227,test$V228,test$V229,test$V230,test$V231,test$V232,test$V233,test$V234,test$V235,test$V236,test$V237,test$V238,test$V239,test$V240,test$V241,test$V242,test$V243,test$V244,test$V245,test$V246,test$V247,test$V248,test$V249,test$V250,test$V251,test$V252,test$V253,test$V254,test$V255,test$V256,test$V257,test$V258,test$V259,test$V260,test$V261,test$V262,test$V263,test$V264,test$V265,test$V266,test$V267,test$V268,test$V269,test$V270,test$V271,test$V272,test$V273,test$V274,test$V275,test$V276,test$V277,test$V278,test$V279,test$V280,test$V281,test$V282,test$V283,test$V284,test$V285,test$V286,test$V287,test$V288,test$V289,test$V290,test$V291,test$V292,test$V293,test$V294,test$V295,
                 test$V296,test$V297,test$V298,test$V299,test$V300,test$V301,test$V302,test$V303,test$V304,test$V305,test$V306,test$V307,test$V308,test$V309,test$V310,test$V311,test$V312,test$V313,test$V314,test$V315,test$V316,test$V317,test$V318,test$V319,test$V320,test$V321,test$V322,test$V323,test$V324,test$V325,test$V326,test$V327,test$V328,test$V329,test$V330,test$V331,test$V332,test$V333,test$V334,test$V335,test$V336,test$V337,test$V338,test$V339,test$V340,test$V341,test$V342,test$V343,test$V344,test$V345,test$V346,test$V347,test$V348,test$V349,test$V350,test$V351,test$V352,test$V353,test$V354,test$V355,test$V356,test$V357,test$V358,test$V359,test$V360,test$V361,test$V362,test$V363,test$V364,test$V365,test$V366,test$V367,test$V368,test$V369,test$V370,test$V371,test$V372,test$V373,test$V374,test$V375,test$V376,test$V377,test$V378,test$V379,test$V380,test$V381,test$V382,test$V383,test$V384,test$V385,test$V386,test$V387,test$V388,test$V389,test$V390,test$V391,test$V392,test$V393,test$V394,test$V395,test$V396,test$V397,test$V398,test$V399,test$V400,test$V401,test$V402,test$V403,test$V404,test$V405,test$V406,test$V407,test$V408,test$V409,test$V410,test$V411,test$V412,test$V413,test$V414,test$V415,test$V416,test$V417,test$V418,test$V419,test$V420,test$V421,test$V422,test$V423,test$V424,test$V425,test$V426,test$V427,test$V428,test$V429,test$V430,test$V431,test$V432,test$V433,test$V434,test$V435,test$V436,test$V437,test$V438,test$V439,test$V440,test$V441,test$V442,test$V443,test$V444,test$V445,test$V446,test$V447,test$V448,test$V449,test$V450,test$V451,test$V452,test$V453,test$V454,test$V455,test$V456,test$V457,test$V458,test$V459,test$V460,test$V461,test$V462,test$V463,test$V464,test$V465,test$V466,test$V467,test$V468,test$V469,test$V470,test$V471,test$V472,test$V473,test$V474,test$V475,test$V476,test$V477,test$V478,test$V479,test$V480,test$V481,test$V482,test$V483,test$V484,test$V485,test$V486,test$V487,test$V488,test$V489,test$V490,test$V491,test$V492,test$V493,test$V494,test$V495,test$V496,test$V497,test$V498,test$V499,test$V500,
                 sep="")

test=data.frame(test$V1,test$query)
colnames(test)=c("uid","query")
test$query=sub(" +$", "", test$query) ##替换末尾空格
test$uid=as.character(test$uid)
test=subset(test,nchar(uid)==32)
dim(test)
test=test[order(test$uid),]
##---------------------------------------------------------------load TEST
cutter = worker("tag") #标注词性
cutter$bylines = TRUE
test_seg=segment(test$query,cutter) ##产出格式为每行一个分词向量的list，词性标注是向量的names

i=1
while(i<=20000)
{test_seg[[i]]=enc2utf8(test_seg[[i]][which(names(test_seg[[i]])=="n")]);
i=i+1
} ###只提取词性为n的词
test_it = itoken(test_seg,tokenizer = word_tokenizer,ids=test$uid)
test_v = create_vocabulary(test_it)
##subset(test_v$vocab,doc_counts>10)
test_clean_v=prune_vocabulary(test_v, doc_proportion_max = 0.2,doc_proportion_min=(2/nrow(test)))##去掉出现的文章数占比太低和太高的词语
test_vectorizer = vocab_vectorizer(test_clean_v)
test_dtm = create_dtm(test_it, test_vectorizer,'lda_c')
n_topics=10
test_vocabulary = test_clean_v
doc_topic_prior = (1/n_topics)
topic_word_prior = (1/n_topics)
test_ldamodel = LatentDirichletAllocation$new(n_topics = n_topics, 
                                              vocabulary = test_vocabulary,
                                              doc_topic_prior = doc_topic_prior,
                                              topic_word_prior = topic_word_prior
) ##搭个LDA的架子
test_dtmatrix = test_ldamodel$fit_transform(test_dtm, n_iter =20, check_convergence_every_n = 5)
##把矩阵扔进去fit,此时上一步建立的架子也已经改变，而doc_topic_distr则是一个文档-主题结果矩阵
test_dt=as.data.frame(t(apply(test_dtmatrix,1,function(x) x/sum(x)))) ###文档属于每个主题的概率



###########################################用训练集拆分训练测试-看效果###################################
###gender
train_dt_g=filter(train_dt,gender!= 0) ##gender data
train_dt_g[which(train_dt_g$gender==2),]$gender=0
ntrain=10000
max.depth_g = 2
gamma_g=0.0001 
eta_g =0.05
nround_g = 400
objective_g = "binary:logistic"
booster_g = "gbtree"
bst <- xgboost(data = as.matrix(train_dt_g[1:ntrain,1:n_topics]), 
               label = train_dt_g$gender[1:ntrain], 
               max.depth = max.depth_g, 
               gamma=gamma_g, 
               eta =eta_g, 
               nround = nround_g, 
               objective = objective_g,
               booster = booster_g)
test1=train_dt_g[(ntrain+1):nrow(train_dt_g),1:n_topics]
pred <- predict(bst, as.matrix(test1))
test_list=prediction(predictions=pred,labels=train_dt_g[(ntrain+1):nrow(train_dt_g),]$gender)
performance(test_list, "auc", fpr.stop=1)


###age
train_dt_a=filter(train_dt,age!=0) ##age data
train_dt_a[which(train_dt_a$age==1),]$age=0
train_dt_a[which(train_dt_a$age==2),]$age=1
train_dt_a[which(train_dt_a$age==3),]$age=2
train_dt_a[which(train_dt_a$age==4),]$age=3
train_dt_a[which(train_dt_a$age==5),]$age=4
train_dt_a[which(train_dt_a$age==6),]$age=5
ntrain=12000
max.depth_a = 10
gamma_a=0.1 
eta_a =0.05
nround_a = 600
objective_a = "multi:softmax"
booster_a = "gbtree"
bst <- xgboost(data = as.matrix(train_dt_a[1:ntrain,1:n_topics]), 
               label = train_dt_a$age[1:ntrain], 
               max.depth = max.depth_a, 
               gamma=gamma_a, 
               eta =eta_a, 
               nround = nround_a, 
               objective = objective_a,
               booster = booster_a,
               num_class = 6)

test1=train_dt_a[(ntrain+1):nrow(train_dt_a),1:n_topics]
pred <- predict(bst, as.matrix(test1))
test1$pred=pred
test1$age=train_dt_a[(ntrain+1):nrow(train_dt_a),]$age
nrow(filter(test1,pred==age))/nrow(test1)
table(train_dt_a[(ntrain+1):nrow(train_dt_a),]$age)
table(test1$pred)

###education
train_dt_e=filter(train_dt,education!=0) ##age data
train_dt_e[which(train_dt_e$education==1),]$education=0
train_dt_e[which(train_dt_e$education==2),]$education=1
train_dt_e[which(train_dt_e$education==3),]$education=2
train_dt_e[which(train_dt_e$education==4),]$education=3
train_dt_e[which(train_dt_e$education==5),]$education=4
train_dt_e[which(train_dt_e$education==6),]$education=5
ntrain=10000
max.depth_e = 6 
gamma_e=0.001
eta_e =0.2
nround_e =300
objective_e = "multi:softmax"
booster_e="gbtree"
bst <- xgboost(data = as.matrix(train_dt_e[1:ntrain,1:n_topics]), 
               label = train_dt_e$education[1:ntrain], 
               max.depth = max.depth_e, 
               gamma=gamma_e, 
               eta =eta_e, 
               nround = nround_e, 
               objective = objective_e,
               booster = booster_e,
               num_class = 6)
test1=train_dt_e[(ntrain+1):nrow(train_dt_e),1:n_topics]
pred <- predict(bst, as.matrix(test1))
test1$pred=pred
test1$education=train_dt_e[(ntrain+1):nrow(train_dt_e),]$education
nrow(filter(test1,pred==education))/nrow(test1)
table(test1$pred)

##########################LDA绘图#################################
library(LDAvis)
json <- createJSON(phi = phi, theta = theta, 
                   doc.length = doc.length, vocab = clean_v,
                   term.frequency = term.frequency)
#json为作图需要数据，下面用servis生产html文件，通过out.dir设置保存位置
serVis(json, out.dir = './vis', open.browser = FALSE)
writeLines(iconv(readLines("./vis/lda.json"), from = "GBK", to = "UTF8"), 
           file("./vis/lda.json", encoding="UTF-8"))




#word_table=data.frame(id=1:169623,word=dimnames(dtm_tfidf)[[2]]) ##建立一个词对应编码的维表
#uid_table=data.frame(id=1:20000,uid=dimnames(dtm_tfidf)[[1]]) ##建立一个uid对应编码的维表
##dimnames(dtm_tfidf)=list(1:20000,1:169623)
#model=LDA(dtm_tfidf)
##library(topicmodels)
##k = 30
##SEED = 2010
##jss_TM = list(VEM = LDA(dtm, k = k, control = list(seed = SEED)), 
##              VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
##              Gibbs = LDA(dtm, k = k, method = "Gibbs",control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000)),
##             CTM = CTM(dtm, k = k,control = list(seed = SEED,var = list(tol = 10^-4), em = list(tol = 10^-3)))
##              )


#########################主成分分析################
library(jiebaR)
library(tm)
library(dplyr)
library(text2vec)
library(LDAvis)
library(xgboost)
library(ROCR)
library(irlba)
###########################################训练集##################################################
setwd("K:/work/CCF/sogo") #home_dir
setwd("E:/R/programme/20160929CFFcompetition/data") ## office_dir
train=read.table("user_tag_query.2W.TRAIN",header = FALSE,quote="",sep = "\t",fill=T,col.names = paste("V", 1:500, sep = ""))
train$query=paste(train$V5,train$V6,train$V7,train$V8,train$V9,train$V10,train$V11,train$V12,train$V13,train$V14,train$V15,train$V16,train$V17,train$V18,train$V19,train$V20,train$V21,train$V22,train$V23,train$V24,train$V25,train$V26,train$V27,train$V28,train$V29,train$V30,train$V31,train$V32,train$V33,train$V34,train$V35,train$V36,train$V37,train$V38,train$V39,train$V40,train$V41,train$V42,train$V43,train$V44,train$V45,train$V46,train$V47,train$V48,train$V49,train$V50,train$V51,train$V52,train$V53,train$V54,train$V55,train$V56,train$V57,train$V58,train$V59,train$V60,train$V61,train$V62,train$V63,train$V64,train$V65,train$V66,train$V67,train$V68,train$V69,train$V70,train$V71,train$V72,train$V73,train$V74,train$V75,train$V76,train$V77,train$V78,train$V79,train$V80,train$V81,train$V82,train$V83,train$V84,train$V85,train$V86,train$V87,train$V88,train$V89,train$V90,train$V91,train$V92,train$V93,train$V94,train$V95,train$V96,train$V97,train$V98,train$V99,train$V100,train$V101,train$V102,train$V103,train$V104,train$V105,train$V106,train$V107,train$V108,train$V109,train$V110,train$V111,train$V112,train$V113,train$V114,train$V115,train$V116,train$V117,train$V118,train$V119,train$V120,train$V121,train$V122,train$V123,train$V124,train$V125,train$V126,train$V127,train$V128,train$V129,train$V130,train$V131,train$V132,train$V133,train$V134,train$V135,train$V136,train$V137,train$V138,train$V139,train$V140,train$V141,train$V142,train$V143,train$V144,train$V145,train$V146,train$V147,train$V148,train$V149,train$V150,train$V151,train$V152,train$V153,train$V154,train$V155,train$V156,train$V157,train$V158,train$V159,train$V160,train$V161,train$V162,train$V163,train$V164,train$V165,train$V166,train$V167,train$V168,train$V169,train$V170,train$V171,train$V172,train$V173,train$V174,train$V175,train$V176,train$V177,train$V178,train$V179,train$V180,train$V181,train$V182,train$V183,train$V184,train$V185,train$V186,train$V187,train$V188,train$V189,train$V190,train$V191,train$V192,train$V193,train$V194,train$V195,train$V196,train$V197,train$V198,train$V199,train$V200,train$V201,train$V202,train$V203,train$V204,train$V205,train$V206,train$V207,train$V208,train$V209,train$V210,train$V211,train$V212,train$V213,train$V214,train$V215,train$V216,train$V217,train$V218,train$V219,train$V220,train$V221,train$V222,train$V223,train$V224,train$V225,train$V226,train$V227,train$V228,train$V229,train$V230,train$V231,train$V232,train$V233,train$V234,train$V235,train$V236,train$V237,train$V238,train$V239,train$V240,train$V241,train$V242,train$V243,train$V244,train$V245,train$V246,train$V247,train$V248,train$V249,train$V250,train$V251,train$V252,train$V253,train$V254,train$V255,train$V256,train$V257,train$V258,train$V259,train$V260,train$V261,train$V262,train$V263,train$V264,train$V265,train$V266,train$V267,train$V268,train$V269,train$V270,train$V271,train$V272,train$V273,train$V274,train$V275,train$V276,train$V277,train$V278,train$V279,train$V280,train$V281,train$V282,train$V283,train$V284,train$V285,train$V286,train$V287,train$V288,train$V289,train$V290,train$V291,train$V292,train$V293,train$V294,train$V295,train$V296,train$V297,train$V298,train$V299,train$V300,train$V301,train$V302,train$V303,train$V304,train$V305,train$V306,train$V307,train$V308,train$V309,train$V310,train$V311,train$V312,train$V313,train$V314,train$V315,train$V316,train$V317,train$V318,train$V319,train$V320,train$V321,train$V322,train$V323,train$V324,train$V325,train$V326,train$V327,train$V328,train$V329,train$V330,train$V331,train$V332,train$V333,train$V334,train$V335,train$V336,train$V337,train$V338,train$V339,train$V340,train$V341,train$V342,train$V343,train$V344,train$V345,train$V346,train$V347,train$V348,train$V349,train$V350,train$V351,train$V352,train$V353,train$V354,train$V355,train$V356,train$V357,train$V358,train$V359,train$V360,train$V361,train$V362,train$V363,train$V364,train$V365,train$V366,train$V367,train$V368,train$V369,train$V370,train$V371,train$V372,train$V373,train$V374,train$V375,train$V376,train$V377,train$V378,train$V379,train$V380,train$V381,train$V382,train$V383,train$V384,train$V385,train$V386,train$V387,train$V388,train$V389,train$V390,train$V391,train$V392,train$V393,train$V394,train$V395,train$V396,train$V397,train$V398,train$V399,train$V400,train$V401,train$V402,train$V403,train$V404,train$V405,train$V406,train$V407,train$V408,train$V409,train$V410,train$V411,train$V412,train$V413,train$V414,train$V415,train$V416,train$V417,train$V418,train$V419,train$V420,train$V421,train$V422,train$V423,train$V424,train$V425,train$V426,train$V427,train$V428,train$V429,train$V430,train$V431,train$V432,train$V433,train$V434,train$V435,train$V436,train$V437,train$V438,train$V439,train$V440,train$V441,train$V442,train$V443,train$V444,train$V445,train$V446,train$V447,train$V448,train$V449,train$V450,train$V451,train$V452,train$V453,train$V454,train$V455,train$V456,train$V457,train$V458,train$V459,train$V460,train$V461,train$V462,train$V463,train$V464,train$V465,train$V466,train$V467,train$V468,train$V469,train$V470,train$V471,train$V472,train$V473,train$V474,train$V475,train$V476,train$V477,train$V478,train$V479,train$V480,train$V481,train$V482,train$V483,train$V484,train$V485,train$V486,train$V487,train$V488,train$V489,train$V490,train$V491,train$V492,train$V493,train$V494,train$V495,train$V496,train$V497,train$V498,train$V499,train$V500
                  ,sep = "") ##把原来是column的query连起来
train=data.frame(train$V1,train$V2,train$V3,train$V4,train$query)
colnames(train)=c("uid","age","gender","education","query")
train$query=sub(" +$", "", train$query) ##过滤query末尾空格
train$uid=as.character(train$uid)
train=subset(train,nchar(uid)==32)
test=read.table("user_tag_query.2W.TEST",header = FALSE,quote="",sep = "\t",fill=T,col.names = paste("V", 1:500, sep = ""))
test$query=paste(test$V2,test$V3,test$V4,test$V5,test$V6,test$V7,test$V8,test$V9,test$V10,test$V11,test$V12,test$V13,test$V14,test$V15,test$V16,test$V17,test$V18,test$V19,test$V20,test$V21,test$V22,test$V23,test$V24,test$V25,test$V26,test$V27,test$V28,test$V29,test$V30,test$V31,test$V32,test$V33,test$V34,test$V35,test$V36,test$V37,test$V38,test$V39,test$V40,test$V41,test$V42,test$V43,test$V44,test$V45,test$V46,test$V47,test$V48,test$V49,test$V50,test$V51,test$V52,test$V53,test$V54,test$V55,test$V56,test$V57,test$V58,test$V59,test$V60,test$V61,test$V62,test$V63,test$V64,test$V65,test$V66,test$V67,test$V68,test$V69,test$V70,test$V71,test$V72,test$V73,test$V74,test$V75,test$V76,test$V77,test$V78,test$V79,test$V80,test$V81,test$V82,test$V83,test$V84,test$V85,test$V86,test$V87,test$V88,test$V89,test$V90,test$V91,test$V92,test$V93,test$V94,test$V95,test$V96,test$V97,test$V98,test$V99,test$V100,test$V101,test$V102,test$V103,test$V104,test$V105,test$V106,test$V107,test$V108,test$V109,test$V110,test$V111,test$V112,test$V113,test$V114,test$V115,test$V116,test$V117,test$V118,test$V119,test$V120,test$V121,test$V122,test$V123,test$V124,test$V125,test$V126,test$V127,test$V128,test$V129,test$V130,test$V131,test$V132,test$V133,test$V134,test$V135,test$V136,test$V137,test$V138,test$V139,test$V140,test$V141,test$V142,test$V143,test$V144,test$V145,test$V146,test$V147,test$V148,test$V149,test$V150,test$V151,test$V152,test$V153,test$V154,test$V155,test$V156,test$V157,test$V158,test$V159,test$V160,test$V161,test$V162,test$V163,test$V164,test$V165,test$V166,test$V167,test$V168,test$V169,test$V170,test$V171,test$V172,test$V173,test$V174,test$V175,test$V176,test$V177,test$V178,test$V179,test$V180,test$V181,test$V182,test$V183,test$V184,test$V185,test$V186,test$V187,test$V188,test$V189,test$V190,test$V191,test$V192,test$V193,test$V194,test$V195,test$V196,test$V197,test$V198,test$V199,test$V200,test$V201,test$V202,test$V203,test$V204,test$V205,test$V206,test$V207,test$V208,test$V209,test$V210,test$V211,test$V212,test$V213,test$V214,test$V215,test$V216,test$V217,test$V218,test$V219,test$V220,test$V221,test$V222,test$V223,test$V224,test$V225,test$V226,test$V227,test$V228,test$V229,test$V230,test$V231,test$V232,test$V233,test$V234,test$V235,test$V236,test$V237,test$V238,test$V239,test$V240,test$V241,test$V242,test$V243,test$V244,test$V245,test$V246,test$V247,test$V248,test$V249,test$V250,test$V251,test$V252,test$V253,test$V254,test$V255,test$V256,test$V257,test$V258,test$V259,test$V260,test$V261,test$V262,test$V263,test$V264,test$V265,test$V266,test$V267,test$V268,test$V269,test$V270,test$V271,test$V272,test$V273,test$V274,test$V275,test$V276,test$V277,test$V278,test$V279,test$V280,test$V281,test$V282,test$V283,test$V284,test$V285,test$V286,test$V287,test$V288,test$V289,test$V290,test$V291,test$V292,test$V293,test$V294,test$V295,
                 test$V296,test$V297,test$V298,test$V299,test$V300,test$V301,test$V302,test$V303,test$V304,test$V305,test$V306,test$V307,test$V308,test$V309,test$V310,test$V311,test$V312,test$V313,test$V314,test$V315,test$V316,test$V317,test$V318,test$V319,test$V320,test$V321,test$V322,test$V323,test$V324,test$V325,test$V326,test$V327,test$V328,test$V329,test$V330,test$V331,test$V332,test$V333,test$V334,test$V335,test$V336,test$V337,test$V338,test$V339,test$V340,test$V341,test$V342,test$V343,test$V344,test$V345,test$V346,test$V347,test$V348,test$V349,test$V350,test$V351,test$V352,test$V353,test$V354,test$V355,test$V356,test$V357,test$V358,test$V359,test$V360,test$V361,test$V362,test$V363,test$V364,test$V365,test$V366,test$V367,test$V368,test$V369,test$V370,test$V371,test$V372,test$V373,test$V374,test$V375,test$V376,test$V377,test$V378,test$V379,test$V380,test$V381,test$V382,test$V383,test$V384,test$V385,test$V386,test$V387,test$V388,test$V389,test$V390,test$V391,test$V392,test$V393,test$V394,test$V395,test$V396,test$V397,test$V398,test$V399,test$V400,test$V401,test$V402,test$V403,test$V404,test$V405,test$V406,test$V407,test$V408,test$V409,test$V410,test$V411,test$V412,test$V413,test$V414,test$V415,test$V416,test$V417,test$V418,test$V419,test$V420,test$V421,test$V422,test$V423,test$V424,test$V425,test$V426,test$V427,test$V428,test$V429,test$V430,test$V431,test$V432,test$V433,test$V434,test$V435,test$V436,test$V437,test$V438,test$V439,test$V440,test$V441,test$V442,test$V443,test$V444,test$V445,test$V446,test$V447,test$V448,test$V449,test$V450,test$V451,test$V452,test$V453,test$V454,test$V455,test$V456,test$V457,test$V458,test$V459,test$V460,test$V461,test$V462,test$V463,test$V464,test$V465,test$V466,test$V467,test$V468,test$V469,test$V470,test$V471,test$V472,test$V473,test$V474,test$V475,test$V476,test$V477,test$V478,test$V479,test$V480,test$V481,test$V482,test$V483,test$V484,test$V485,test$V486,test$V487,test$V488,test$V489,test$V490,test$V491,test$V492,test$V493,test$V494,test$V495,test$V496,test$V497,test$V498,test$V499,test$V500,
                 sep="")

test=data.frame(test$V1,test$query)
colnames(test)=c("uid","query")
test$query=sub(" +$", "", test$query) ##替换末尾空格
test$uid=as.character(test$uid)
test=subset(test,nchar(uid)==32)
test1=data.frame(test$uid,age="10",gender="10",education="10",query=test$query)
colnames(test1)=c("uid","age","gender","education","query")
traintest=rbind(train,test1)
traintest=traintest[order(traintest$uid),]

##age分布
# 0    1    2    3    4    5    6 
# 355  7900 5330 3603 2141 589  82

##gender分布
# 0     1      2 
# 424   11365  8211

##edu分布
# 0    1    2    3    4    5    6 
# 1878 65   119  3722 5579 7487 1150

##---------------------------------------------------------------load data

cutter = worker("tag") #标注词性
cutter$bylines = TRUE
traintest_seg=segment(traintest$query,cutter) ##产出格式为每行一个分词向量的list，词性标注是向量的names

i=1
while(i<=20000)
{train_seg[[i]]=enc2utf8(train_seg[[i]][which(names(train_seg[[i]])=="n")]);
i=i+1
} ###只提取词性为n的词

traintest_it = itoken(traintest_seg,tokenizer = word_tokenizer,ids=traintest$uid)
traintest_v = create_vocabulary(traintest_it)
##subset(v$vocab,doc_counts>10) ## 观察词doc数量超过10个的词
traintest_clean_v=prune_vocabulary(traintest_v, doc_proportion_max = 0.4,doc_proportion_min=(2/40000))##去掉出现的文章数占比太低和太高的词语
###clean_v$vocab
traintest_vectorizer = vocab_vectorizer(traintest_clean_v)
traintest_dtm = create_dtm(traintest_it, traintest_vectorizer)
dtm_tfidf = transform_tfidf(traintest_dtm)
nv=120
pc <- dtm_tfidf %*% irlba(dtm_tfidf, nv=nv, nu=0, center=colMeans(dtm_tfidf), right_only=TRUE)$v
pc=as.matrix(pc)
traintest_dt=data.frame(uid=traintest$uid,pc,age=traintest$age,gender=traintest$gender,education=traintest$education)
fix(traintest_dt)

###########################################用训练集拆分训练测试-看效果###################################
###gender
train_dt_g=filter(traintest_dt,gender!= 0 & gender!=10) ##gender data
train_dt_g[which(train_dt_g$gender==2),]$gender=0
ntrain=10000
max.depth_g = 6
gamma_g=0.1 
eta_g =0.01
nround_g = 400
objective_g = "binary:logistic"
booster_g = "gbtree"
bst <- xgboost(data = as.matrix(train_dt_g[1:ntrain,2:(nv+1)]), 
               label = train_dt_g$gender[1:ntrain], 
               max.depth = max.depth_g, 
               gamma=gamma_g, 
               eta =eta_g, 
               nround = nround_g, 
               objective = objective_g,
               booster = booster_g,
               verbose=0)
test1=train_dt_g[(ntrain+1):nrow(train_dt_g),2:(nv+1)]
pred <- predict(bst, as.matrix(test1))
test_list=prediction(predictions=pred,labels=train_dt_g[(ntrain+1):nrow(train_dt_g),]$gender)
performance(test_list, "auc", fpr.stop=1)

###age
train_dt_a=filter(traintest_dt,age!=0 & age!=10) ##age data
train_dt_a[which(train_dt_a$age==1),]$age=0
train_dt_a[which(train_dt_a$age==2),]$age=1
train_dt_a[which(train_dt_a$age==3),]$age=2
train_dt_a[which(train_dt_a$age==4),]$age=3
train_dt_a[which(train_dt_a$age==5),]$age=4
train_dt_a[which(train_dt_a$age==6),]$age=5
ntrain=12000
max.depth_a = 6
gamma_a=0.1 
eta_a =0.1
nround_a = 400
objective_a = "multi:softmax"
booster_a = "gbtree"
bst <- xgboost(data = as.matrix(train_dt_a[1:ntrain,2:(nv+1)]), 
               label = train_dt_a$age[1:ntrain], 
               max.depth = max.depth_a, 
               gamma=gamma_a, 
               eta =eta_a, 
               nround = nround_a, 
               objective = objective_a,
               booster = booster_a,
               num_class = 6,
               verbose = 0)

test1=train_dt_a[(ntrain+1):nrow(train_dt_a),2:(nv+1)]
pred <- predict(bst, as.matrix(test1))
test1$pred=pred
test1$age=train_dt_a[(ntrain+1):nrow(train_dt_a),]$age
nrow(filter(test1,pred==age))/nrow(test1)
table(train_dt_a[(ntrain+1):nrow(train_dt_a),]$age)
table(test1$pred)

###education
train_dt_e=filter(traintest_dt,education!=0 & education!=10) ##age data
train_dt_e[which(train_dt_e$education==1),]$education=0
train_dt_e[which(train_dt_e$education==2),]$education=1
train_dt_e[which(train_dt_e$education==3),]$education=2
train_dt_e[which(train_dt_e$education==4),]$education=3
train_dt_e[which(train_dt_e$education==5),]$education=4
train_dt_e[which(train_dt_e$education==6),]$education=5
ntrain=10000
max.depth_e = 6 
gamma_e=0.001
eta_e =0.2
nround_e =300
objective_e = "multi:softmax"
booster_e="gbtree"
bst <- xgboost(data = as.matrix(train_dt_e[1:ntrain,2:(nv+1)]), 
               label = train_dt_e$education[1:ntrain], 
               max.depth = max.depth_e, 
               gamma=gamma_e, 
               eta =eta_e, 
               nround = nround_e, 
               objective = objective_e,
               booster = booster_e,
               num_class = 6,
               verbose = 0)
test1=train_dt_e[(ntrain+1):nrow(train_dt_e),2:(nv+1)]
pred <- predict(bst, as.matrix(test1))
test1$pred=pred
test1$education=train_dt_e[(ntrain+1):nrow(train_dt_e),]$education
nrow(filter(test1,pred==education))/nrow(test1)
table(test1$pred)

######################prediction#############################

###test gender prediction 进行这步之前要在前面的测试中把参数调整到最优
bst <- xgboost(data = as.matrix(train_dt_g[,2:(nv+1)]), 
               label = train_dt_g$gender, 
               max.depth = max.depth_g, 
               gamma=gamma_g, 
               eta =eta_g, 
               nround = nround_g, 
               objective = objective_g,
               booster = booster_g)
test2=as.matrix(traintest_dt[which(traintest_dt$gender==10),2:(nv+1)])
pred <- predict(bst, test2)
gender=as.data.frame(cbind(test2,pred)[,c(1,(nv+1))])
gender$X1=round(gender$pred,0)
colnames(gender)=c("gender","score")
gender=cbind(rownames(gender),gender$gender)
gender=as.data.frame(gender)
fix(gender)


###test age prediction
bst<- xgboost(data = as.matrix(train_dt_a[,2:(nv+1)]), 
              label = train_dt_a$age, 
              max.depth = max.depth_a, 
              gamma=gamma_a, 
              eta =eta_a, 
              nround = nround_a, 
              objective = objective_a,
              booster = booster_a,
              num_class = 6)
test2=as.matrix(traintest_dt[which(traintest_dt$gender==10),2:(nv+1)])
pred<- predict(bst, test2)
age=as.data.frame(cbind(test2,pred)[,c(1,(nv+1))])
age$X1=age$pred+1
colnames(age)=c("age","agescore")
age=as.data.frame(cbind(rownames(age),age$age))

###test edu prediction 
bst <- xgboost(data = as.matrix(train_dt_e[,2:(nv+1)]), 
               label = train_dt_e$education, 
               max.depth = max.depth_e, 
               gamma=gamma_e, 
               eta =eta_e, 
               nround = nround_e, 
               objective = objective_e,
               booster = booster_e,
               num_class = 6)
test2=as.matrix(traintest_dt[which(traintest_dt$gender==10),2:(nv+1)])
pred<- predict(bst, test2)
edu=as.data.frame(cbind(test2,pred)[,c(1,(nv+1))])
edu$X1=edu$pred+1
colnames(edu)=c("edu","eduscore")
edu=as.data.frame(cbind(rownames(edu),edu$edu))



colnames(age)=c("uid","age")
colnames(gender)=c("uid","gender")
colnames(edu)=c("uid","edu")
result=merge(gender,edu,by="uid")
result1=merge(result,age,by="uid")
fix(result1)
result1[which(result1$gender==0),]$gender=2
result1=data.frame(result1$uid,result1$age,result1$gender,result1$edu)

write.table(result1,"falcon.txt",sep = " ",col.names = FALSE,row.names=FALSE)







