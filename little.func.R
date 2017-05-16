#########
## Little functions
## Start: 2017.5.15
## 
#########
re_gsub <- function(dat){
  ans <- gsub("([N ])", "", dat)
  ans
}

#########
n.sel <- function(dat){
  ans <- enc2utf8(dat[which(names(dat) %in% c("n", "vn", "eng", "nr", "ns", "nt", "nz"))])
  ans
}

#########
sort50 <- function(dat, nt=50) {
  dat_sort <- sort(dat, decreasing = TRUE)
  ans <- dat_sort[1: nt]
  names(ans)
}