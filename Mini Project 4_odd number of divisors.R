
#Project 4: Generating which number has odd number of divisor.

odd.divs <- function(value){
  seq.num <- 1:value
  num.div <- function(value) {
    for (i in 1:value) {
      numbers <- 1:value
      ifelse(i%%numbers == 0,TRUE,FALSE)
      div <- c(ifelse(i%%numbers == 0,TRUE,FALSE))
    } 
    sum(div)
  } 
  list.div <- lapply(seq.num, num.div)
  result <- c(ifelse(as.numeric(list.div)%%2 != 0, T, F))
  which(result)
}

odd.divs(17)
odd.divs(100)
odd.divs(1000)
