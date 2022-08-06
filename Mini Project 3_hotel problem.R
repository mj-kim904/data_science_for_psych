#Project 3: Hotel Problem

#If the number of divisors of each door number is even, then the door would be closed in the end.
#If the number of divisors of each door number is odd, then the door would be opened in the end.


Count.door <- function(value) {
  #Define the number of hotels/employees as seq.num
  seq.num <- 1:value
  #function to count divisor
  num.div <- function(value) {
    for (i in 1:value) {
      numbers <- 1:value
      ifelse(i%%numbers == 0,TRUE,FALSE)
      div <- c(ifelse(i%%numbers == 0,TRUE,FALSE))
    } 
    sum(div)
  }
  #apply "num.div" function to all the number in "seq.num"
  list.div <- lapply(seq.num, num.div)
  #Find if the number of divisors is even or odd
  result <- c(ifelse(as.numeric(list.div)%%2 == 0, FALSE, TRUE))
  #Count number of doors open
  count.result <- sum(result)
  Status <- c(count.result , value-count.result)
  Status
}



#Count.door(Value) - result will be "[1] the number of open, the number of close"

Count.door(5)
Count.door(8)
Count.door(10)
Count.door(1000)
Count.door(500)

vector.500.2 <- rep(0, 500) # repeating 1 for 500 places

for (p in 1:500) { # p = person
  for (i in 1:500) { # i = door, want to check if p uses i
    if (i %% p == 0) { # if p goes into i with no remainders, or i/p has 0 remainders
      vector.500.2[i] = !vector.500.2[i] # at i, say 10th place, it will become the other value # what is !0: not zero, which 1
    }
  }
}
print(vector.500.2)
