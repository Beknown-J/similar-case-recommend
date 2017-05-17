library(devtools)
library(cidian)
library(stringr)
library(magrittr)
library(data.table)
library(tidyr)

# 建立数据目录，本项目所有数据都保存在这个文件夹下
# 其中getwd()用来获取当前工作环境的目录
data.dir <- sprintf("%s/data", getwd())

# 搜狗词典所在目录
dict.dir <- sprintf("%s/dict", data.dir)

# 输出目录
if(!file.exists("./data/result")){
  dir.create(path="data/result", recursive = TRUE)
}
result.dir <- sprintf("%s/result", data.dir)

# 获取所有搜狗词库的完整路径
scel.paths <- list.files(dict.dir, pattern = ".scel$", full.names = T)

# 将所有词库逐个导入，并输出成.txt文件
lapply(seq_along(scel.paths), function(i){
  decode_scel(scel = scel.paths[i],
              output = str_c(scel.paths[i], ".txt"),
              cpp = TRUE,
              progress = TRUE)}) %>%
  invisible()

# 整合姓氏词典
name.path <- sprintf("%s/中华姓氏大全.scel.txt", dict.dir)
names <- read.csv(name.path, fill = TRUE, header = FALSE, sep = " ")
fname <- gather(names, variable, fname)
fname <- data.frame(fname = fname$fname[fname$fname != ""], mark = "nr")
write.table(fname, file = name.path, quote = FALSE, row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8")

# 将所有的txt字典导入并整合成单一的用户词典，这里使用到了data.table包中的rbindlist函数
dict.paths <- list.files(dict.dir, pattern = ".txt$", full.names = T)
dict.list <- lapply(seq_along(dict.paths),
                    function(i) fread(dict.paths[i], encoding = "UTF-8", header = FALSE))
dict <- rbindlist(dict.list)

# 去重
dict <- unique(dict, by = names(dict))

# 将最后生成的词库导出成单一的txt文件，用于后续分词 ----
write.table(dict, file = sprintf("%s/usr_word.txt", result.dir),
            quote = F,
            row.names = F,
            col.names = F,
            fileEncoding = "UTF-8")
