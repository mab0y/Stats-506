# Import wine.data to date.frame
wine_df <- read.csv("~/桌面/Stats-506/PS-01/wine.data",header=FALSE)

# Set column name to wind_df
colnames(wine_df) <- c('class', 'Alcohol', 'Malic acid', 'Ash', 
                       'Alcalinity of ash', 'Magnesium', 'Total phenols', 
                       'Flavanoids', 'Nonflavanoid phenols', 'Proanthocyanins', 
                       'Color intensity', 'Hue', 'OD280/OD315 of diluted wines', 
                       'Proline')

# Check the number of wines within each class
table(wine_df$class)

# Check the class of the highest alcohol content
wine_df[which.max(wine_df$Alcohol),"class"]

# Check the class of the lowest alcohol content
wine_df[which.min(wine_df$Alcohol),"class"]

# Count the number of wines have higher levels of magnesium than 114 mg/l
sum(wine_df$Magnesium > 114)


# Count Within each class the number of wines have 
# higher levels of magnesium than average German beer
table(wine_df[which(wine_df$Magnesium > 114), "class"])

# Examine the level of Ash difference across three classes
#Calculate sample mean and variance
wine_df_mean <- tapply(wine_df$Ash, wine_df$class, mean)
wine_df_var <- tapply(wine_df$Ash,wine_df$class, var)

# First calculate mean of overall dataframe and of each class
avg_overall<-lapply(wine_df, mean)
avg_1 <- lapply(wine_df[which(wine_df$class==1),], mean)
avg_2 <- lapply(wine_df[which(wine_df$class==2),], mean)
avg_3 <- lapply(wine_df[which(wine_df$class==3),], mean)

# Next use map to combine the lists
# https://stackoverflow.com/questions/60605598/r-combine-two-lists-with-same-column-names: helped me figure out to use Map to combine the lists
avg_combine <- Map(c, avg_overall, avg_1,avg_2, avg_3)
avg_combine <- data.frame(avg_combine)
rownames(avg_combine) <- c("Overall", "Class 1", "Class 2", "Class 3")

# Calculate number of sample from each class
num_class <- table(wine_df$class)

# Calculate mean and variance for each class
wine_df_mean <- tapply(wine_df$Ash,wine_df$class, mean)
wine_df_var <- tapply(wine_df$Ash,wine_df$class, var)

# Calculate t-stats, degree of freedom and p-value for class 1 and class 2
ts_12 <- abs((wine_df_mean[1]-wine_df_mean[2])/sqrt(wine_df_var[1]/num_class[1]
                                                  +wine_df_var[2]/num_class[2]))
df_12 <- (wine_df_var[1]/num_class[1]+wine_df_var[2]/num_class[2])^2/
  ((wine_df_var[1]/num_class[1])^2/(num_class[1]-1)+
     (wine_df_var[2]/num_class[2])^2/(num_class[2]-1))
pt_12 <- 2*pt(ts_12, df_12,lower.tail=FALSE)

# Using built-in t-test method to verify the result
t.test(wine_df[which(wine_df$class==1), "Ash"], wine_df[which(wine_df$class==2), "Ash"])

# Repeat the steps for class 1 and class 3
ts_13 <- abs((wine_df_mean[1]-wine_df_mean[3])/sqrt(wine_df_var[1]/num_class[1]
                                                    +wine_df_var[3]/num_class[3]))
df_13 <- (wine_df_var[1]/num_class[1]+wine_df_var[3]/num_class[3])^2/
  ((wine_df_var[1]/num_class[1])^2/(num_class[1]-1)+
     (wine_df_var[3]/num_class[3])^2/(num_class[3]-1))
pt_13 <- 2*pt(ts_13, df_13,lower.tail=FALSE)

t.test(wine_df[which(wine_df$class==1), "Ash"], wine_df[which(wine_df$class==3), "Ash"])

# Repeat the steps for class 2 and class 3
ts_23 <- abs((wine_df_mean[2]-wine_df_mean[3])/sqrt(wine_df_var[2]/num_class[2]+
                                                      wine_df_var[3]/num_class[3]))
df_23 <- (wine_df_var[2]/num_class[2]+wine_df_var[3]/num_class[3])^2/
  ((wine_df_var[2]/num_class[2])^2/(num_class[2]-1)+
     (wine_df_var[3]/num_class[3])^2/(num_class[3]-1))
pt_23 <- 2*pt(ts_23, df_23,lower.tail=FALSE)

t.test(wine_df[which(wine_df$class==2), "Ash"], wine_df[which(wine_df$class==3), "Ash"])

