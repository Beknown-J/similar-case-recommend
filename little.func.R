#########
## Little functions
## Start: 2017.5.15
## 
#########
re_gsub <- function(dat){
  ans <- gsub("([0-9A-Za-z ])", "", dat)  ## del numbers and blank
  ans
}

#########
n.sel <- function(dat, label){
  ans <- enc2utf8(dat[which(names(dat) %in% label)])
  ans
}

#########
reglar.del <- function(dat){
  dat_chr <- dat[nchar(dat) != 1]  ## delete one character
  # browser()
  del_vect <- grep("X", dat_chr)   ## delete X
  if(length(del_vect)==0){
    ans <- dat_chr
  }else{
    ans <- dat_chr[-del_vect]
  }
  ans
}

#########
sort50 <- function(dat, nt=50) {
  dat_sort <- sort(dat, decreasing = TRUE)
  ans <- dat_sort[1: nt]
  names(ans)
}


