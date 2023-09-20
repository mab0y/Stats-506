#' Function to calculate total winnings with a loop
#'
#' @param times a numeric scalar
#' @return the total winnings after times of rolls
play_dice1 <- function(times){
  total_winnings <- 0
  for (i in 1:times){
    dice <- sample(1:6, 1, replace = TRUE)
    if (dice %in% c(2, 4, 6)){
      total_winnings <- total_winnings + dice - 2
    }
    else{
      total_winnings <- total_winnings - 2
    }
  }
  return(total_winnings)
}


#' Function to calculate total winnings with vectorized functions
#'
#' @param times a numeric scalar
#' @return the total winnings after times of rolls
play_dice2 <- function(times){
  dices <- sample(1:6, times, replace = TRUE)
  total_winnings <- sum(ifelse(dices %in% c(2,4,6), dices -2, -2))
  return(total_winnings)
}

# Here I refer to https://stackoverflow.com/questions/39176032/extracting-elements-from-a-table-in-r.
# I found that table names are str. I can turn them to integer to test the divisiblity or use them directly to index the value

#' Function to calculate total winnings with table of die rolls
#'
#' @param times a numeric scalar
#' @return the total winnings after times of rolls
play_dice3 <- function(times){
  total_winnings <- 0
  dices_table <- table(sample(1:6, times, replace = TRUE))
  for (i in names(dices_table)){
    name_integer = as.integer(i)
    if (name_integer %% 2 == 0){
      total_winnings <- total_winnings + dices_table[[i]] * (name_integer -2)
    }
    else{
      total_winnings <- total_winnings - dices_table[[i]] * 2
    }
  }
  return(total_winnings)
}

#' Function to calculate total winnings with lapply function
#'
#' @param times a numeric scalar
#' @return the total winnings after times of rolls
play_dice4 <- function(times){
  dices <- sample(1:6, times, replace = TRUE)
  winnings <- sapply(dices, function(dice){
    if (dice %in% c(2,4,6)){
      return(dice-2)
    }
    else{
      return(-2)
    }
  })
  return(sum(winnings))
}

play_dice1(3)
play_dice1(3000)
play_dice2(3)
play_dice2(3000)
play_dice3(3)
play_dice3(3000)
play_dice4(3)
play_dice4(3000)

set.seed(42)
play_dice1(3)
play_dice1(3000)
set.seed(42)
play_dice2(3)
play_dice2(3000)
set.seed(42)
play_dice3(3)
play_dice3(3000)
set.seed(42)
play_dice4(3)
play_dice4(3000)
  
mb1<-microbenchmark(play_dice1(100))
mb2<-microbenchmark(play_dice2(100))
mb3<-microbenchmark(play_dice3(100))
mb4<-microbenchmark(play_dice4(100))
rbind(mb1,mb2,mb3,mb4)
  
mb1<-microbenchmark(play_dice1(10000))
mb2<-microbenchmark(play_dice2(10000))
mb3<-microbenchmark(play_dice3(10000))
mb4<-microbenchmark(play_dice4(10000))
rbind(mb1,mb2,mb3,mb4)
  

for (i in 1:20){
  cat("Number of dice to roll is:", i, "The average total winning is:", mean(replicate(100000,play_dice1(i))), "\n")
}


  