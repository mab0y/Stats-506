library(nycflights13)
library(tidyverse)
library(future)
library(parallel)

n_bootstrap = 1000

# https://stackoverflow.com/questions/31534531/generate-stratified-bootstrap-resamples-in-r
calculate_means_parallel <- function(i) {
  flights %>% 
    group_by(dest) %>%
    sample_n(size = n(), replace = TRUE) %>%
    group_by(origin) %>%
    summarize(mean_air_time = mean(air_time, na.rm = TRUE))
}

calculate_se_and_ci <- function(combined_results) {
  se_and_ci <- combined_results %>%
    group_by(origin) %>%
    summarize(
      mean = mean(mean_air_time, na.rm = TRUE),
      se = sd(mean_air_time, na.rm = TRUE) / sqrt(length(mean_air_time)),
      ci_lower = mean - qt(0.975, df = length(mean_air_time) - 1) * se,
      ci_upper = mean + qt(0.975, df = length(mean_air_time) - 1) * se
    )
  return(se_and_ci)
}

# without any parallel processing
non_parallel <- system.time({
  mean_results <- list()
  for (i in 1:n_bootstrap) {
    mean_results[[i]] <-
      flights %>% 
        group_by(dest) %>%
        sample_n(size = n(), replace = TRUE) %>%
        group_by(origin) %>%
        summarize(mean_air_time = mean(air_time, na.rm = TRUE))
  }
})

as.data.frame(calculate_se_and_ci(bind_rows(mean_results)))


# future
futures <- system.time({
  plan(multisession, workers = 4)
  mean_results <- list()
  for (i in 1:n_bootstrap) {
    mean_results[[i]] <- future(calculate_means_parallel())
  }
  mean_results <- lapply(mean_results, value)
})

as.data.frame(calculate_se_and_ci(bind_rows(mean_results)))

# socket
socket <- system.time({
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, c(library(tidyverse),library(nycflights13)))
  clusterExport(cl, c("calculate_means_parallel", "n_bootstrap"))
  mean_results <- list()
  mean_results <- parLapply(cl, seq_len(n_bootstrap), calculate_means_parallel)
  stopCluster(cl)
})

as.data.frame(calculate_se_and_ci(bind_rows(mean_results)))

rbind(non_parallel, futures, socket)