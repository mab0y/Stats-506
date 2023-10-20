nnmaps <- read.csv("~/chicago-nmmaps.csv")
nnmaps$date <- as.Date(nnmaps$date)

#' get_temp()
#'
#' @param month Month, either a numeric 1-12 or a string
#' @param year A numeric year
#' @param data The data set to obtain data from
#' @param celsius Logically indicating whther the results should be in celsius. Default FALSE
#' @param average_fn  A function with which to compute the mean. Default is mean
#'
#' @return The average temperature for a given month
get_temp <- function(month, year, data, celsius=FALSE, average_fn=mean){
  month_names <- c(
    "January", "February", "March", "April", "May", "June", 
    "July", "August", "September", "October", "November", "December"
  )
  
  if (year>=1997 && year<=2000){year_num <- year}
    else{
      stop("Invalid Year")
    }

  if (is.character(month)) {
    month_num <- which(grepl(pattern = month, x = month_names, ignore.case = TRUE))
  }
    else if (is.numeric(month) && month >= 1 && month <= 12) {
    month_num <- month
    } 
      else {
        stop("Invalid Month")
      }

  result <- data %>% 
    filter(year(date) == year_num & month(date) == month_num) %>%
      summarize(avg_temp = average_fn(temp))
  
  if(celsius) {
    result <- result %>%
      mutate(avg_temp = (avg_temp - 32) * 5/9)
  }
  
  return(result$avg_temp)
}


get_temp("Apr", 1999, data = nnmaps)
get_temp("Apr", 1999, data = nnmaps, celsius = TRUE)
get_temp(10, 1998, data = nnmaps, average_fn = median)
get_temp(13, 1998, data = nnmaps)
get_temp(2, 2005, data = nnmaps)
get_temp("November", 1999, data =nnmaps, celsius = TRUE,
         average_fn = function(x) {
           x %>% sort -> x
           x[2:(length(x) - 1)] %>% mean %>% return
         })




