cars <- read.csv("~/cars.csv",header=TRUE)
colnames(cars) <- c("height", "length", "width", "driveline", "engine_type", 
                    "hybrid", "forward_gears", "transmission", "city_mpg", 
                    "fuel_type", "highway_mpg", "classification", "id", 
                    "make", "model_year", "year", "horsepower", "torque")

cars <- cars[which(cars$fuel_type == "Gasoline"),]

cars$year <- as.factor(cars$year)
cars_lm <- lm(highway_mpg ~ horsepower + torque + height + length + width + year, cars)

cars_lm_inter <- lm(highway_mpg ~ horsepower * torque + height + length + width + year, cars)

table(cars$year)


library(interactions)
interact_plot(cars_lm_inter, pred = horsepower, modx = torque, at = list("year"=factor(2011)))

hist(cars$torque)
quantile(cars$torque)
interact_plot(cars_lm_inter, pred = horsepower, modx = torque, modx.values = c(150,250,350), at = list("year"=factor(2011)))

horsepower_torque<-cars$horsepower*cars$torque
year2010 <- as.numeric(cars$year == 2010)
year2011 <- as.numeric(cars$year == 2011)              
year2012 <- as.numeric(cars$year == 2012)

design_matrix <- cbind(rep(1,nrow(cars)),cars[,c("horsepower","torque")],year2010,year2011,year2012,horsepower_torque)
design_matrix <- as.matrix(design_matrix)
beta_hat <- solve(t(design_matrix) %*% design_matrix) %*% t(design_matrix) %*% cars$highway_mpg
