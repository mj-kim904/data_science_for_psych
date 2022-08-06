#Project 2: Counting the number of divisors

Count.div <- function(value) {
  n <- c(1:value)
  ifelse(value%%n == 0,TRUE,FALSE)
  div <- c(ifelse(value%%n == 0,TRUE,FALSE))
  sum(div)
}


Count.div(4)
Count.div(12)
Count.div(13)


#Using for loop

num.div <- function(value) {
  for (i in 1:value) {
    numbers <- 1:value
    ifelse(i%%numbers == 0,TRUE,FALSE)
    div <- c(ifelse(i%%numbers == 0,TRUE,FALSE))
  } 
  sum(div)
}

num.div(4)
num.div(7)


#Counting the number of divisors for all numbers less than and equal to a given number.

num.div.all <- function(value){
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
  unlist(list.div)
}

num.div.all(5)


#Factorial for loop

n <- 1:500
for(i in 1:1){
  print(n[(n%%11)==0])
}

n <- 500
multiple.11 <- c()
for(i in 1:n){
  if(i%%11==0){
    multiple.11 <- c(multiple.11,i)
  }
} 

n <- 1:500
for(i in 1:1){
  print(n[(n%%11)==0])
}

multiple.11
