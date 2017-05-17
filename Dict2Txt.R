setwd("~/Documents/iFly Project/law dict")
##single dict to txt
library(devtools) 
library(cidian)
library(stringr)
library(data.table)
dir.path <- "仲裁法"

decode_scel(scel = paste0(dir.path, ".scel"),
            output = paste0(dir.path, ".txt"),
            cpp = TRUE,
            progress = TRUE)
# 词典合并
# 建立相关目录 ----
# 建立数据目录，本项目所有数据都保存在这个文件夹下（包括搜狗词库文件）。
# 其中getwd()用来获取当前工作环境的目录
data.dir <- sprintf( "%s/data", getwd())

# 用来输出结果的目录
results.dir <- sprintf("%s/results", getwd())

# 搜狗词典所在目录
cidian.dir <- sprintf("%s/cidian", data.dir)

# 获取所有搜狗词库的完整路径
scel.paths <- list.files(cidian.dir, pattern = ".scel$", full.names = T)

# 将所有词库逐个导入，并输出成.txt文件 ----
lapply(seq_along(scel.paths), function(i) {
  decode_scel(scel = scel.paths[i],
              output = str_c(scel.paths[i], ".txt"),
              cpp = TRUE)})

# 将所有的txt字典导入并整合成单一的用户词典，这里使用到了data.table包中的rbindlist函数 ----
dict.paths <- list.files(getwd(), pattern = ".txt$", full.names = T)
dict.list <- lapply(seq_along(dict.paths),
                    function(i) fread(dict.paths[i], encoding = "UTF-8"))
dict <- rbindlist(dict.list)

# 去重
dict <- unique(dict, by = names(dict))

# 将最后生成的词库导出成单一的txt文件，用于后续分词 ----
write.table(dict, file = sprintf("%s/guba.use.dict.utf8", data.dir),
            quote = F,
            row.names = F,
            col.names = F,
            fileEncoding = "UTF-8")