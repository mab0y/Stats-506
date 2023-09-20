cars <- read.csv("~/cars.csv",header=TRUE)
colnames(cars) <- c("height", "length", "width", "driveline", "engine_type", 
                    "hybrid", "forward_gears", "transmission", "city_mpg", 
                    "fuel_type", "highway_mpg", "classification", "id", 
                    "make", "model_year", "year", "horsepower", "torque")

cars <- cars[which(cars$fuel_type == "Gasoline"),]

cars$year <- as.factor(cars$year)
cars_lm <- lm(highway_mpg ~ horsepower + torque + height + length + width + year, cars)

cars_lm_inter <- lm(highway_mpg ~ horsepower * torque +  year, cars)

interact_plot(cars_lm_inter, pred = horsepower, modx = torque, at = list("year"=factor(2011)))

              