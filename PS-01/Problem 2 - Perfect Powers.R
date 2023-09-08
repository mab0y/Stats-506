# Function to find the root of a given number and power if exists
isPerfectPower <- function(num, power){
  # Begin with root equals 1
  root <- 1
  # Loop until the result reach power
  while (root^power <= num){
    
    if (root^power == num){
      # If root exists, return TRUE and the root
      return(list(isPerfect = TRUE, root = root))
    }
    root <- root + 1
  }
  # If root does not exist, return FALSE and NA
  return(list(isPerfect = FALSE, root = NA))
}

# Function to identify the lowest power
findRootPower <- function(num){
  # Start frow the lowerst power 2
  for (power in 2:100){
    if (isPerfectPower(num, power)$isPerfect == TRUE){
      # If root exists for this power, return the expression
      return (sprintf("%.0f = %.0f ^ %.0f", num, isPerfectPower(num, power)$root, power))
    }
  }
  # If under power 100, there is still no root, return not a perfect power
  return (sprintf("%.0f is not a perfect power", num))
}

# Use the numbers to test the function
test_num <- c(27, 13060694016, 7776, 170859375, 58247422, 94143178827)
lapply(test_num, findRootPower)
