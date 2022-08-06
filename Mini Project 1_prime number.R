#Project 1: Prime number Generator
#: Creating a code generating all prime numbers in a given range.

GeneratingPrimes <- function(integers){
  numbers <- rep(TRUE, integers)
  numbers[1] <- FALSE
    beginning.num <-2
  for(i in beginning.num:sqrt(integers)){
    numbers[seq(from = 2 * beginning.num, to = integers, by = beginning.num)] <-FALSE
    beginning.num <- beginning.num + min(which(numbers[3 : integers]))
  }
  return(which(numbers))
}

#test
GeneratingPrimes(30)
GeneratingPrimes(100)
GeneratingPrimes(1000)

