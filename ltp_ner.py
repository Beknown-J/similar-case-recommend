#coding=utf-8

import sys
from pyltp import NamedEntityRecognizer
from pyltp import Segmentor
from pyltp import Postagger

text = open(sys.argv[1], 'rb').read() 
segmentor = Segmentor()  # 初始化实例
segmentor.load('/data/iflyrd/develop/xinlv/ltp/ltp-3.3.1/ltp_data/cws.model') # 加载模型
words = segmentor.segment(text)
#print '\t'.join(words)
segmentor.release()

postagger = Postagger() # 初始化实例
postagger.load('/data/iflyrd/develop/xinlv/ltp/ltp-3.3.1/ltp_data/pos.model')  # 加载模型
postags = postagger.postag(words)  # 词性标注
#print '\t'.join(postags)
postagger.release()  # 释放模型

recognizer = NamedEntityRecognizer() # 初始化实例
recognizer.load('/data/iflyrd/develop/xinlv/ltp/ltp-3.3.1/ltp_data/ner.model') # 加载模型
netags = recognizer.recognize(words, postags)  # 命名实体识别
#print '\t'.join(netags)
recognizer.release()

word_list = []
netag_list = []
for word in words:
	word_list.append(word)
for netag in netags:
	netag_list.append(netag)	

for i in range(0,len(netag_list)):
	if netag_list[i] != 'O':
		print word_list[i] + '\t' + netag_list[i]

