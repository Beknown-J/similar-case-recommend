#########
## Little functions
## Start: 2017.5.15
## 
#########
re_gsub <- function(dat){
  ans1 <- gsub("([ ])", "", dat)  ## del numbers and blank ## 0-9A-Za-z
  ans1
}

regular.gsub <- function(dat, reg){
  res <- dat
  for(i in seq_along(reg)){
    res <- gsub(reg[i], '', res)
  }
  ans <- gsub("([0-9A-Za-z])", "", res)
  return(ans)
}


#########
n.sel <- function(dat, label){
  ans <- enc2utf8(dat[which((names(dat) %in% label))])
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
sort.dat <- function(dat, nt=50) {
  dat_sort <- sort(dat, decreasing = TRUE)
  
  res <- dat_sort[1: nt]
  res <- res[res != 0]
  ans <- names(res)
  return(ans)
}


