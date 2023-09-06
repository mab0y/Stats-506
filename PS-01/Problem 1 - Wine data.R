#import wine.data to date.frame
wine_df <- read.csv("~/桌面/Stats-506/PS-01/wine.data",header=FALSE)

#set column name to wind_df
colnames(wine_df) <- c('class','Alcohol','Malic acid','Ash','Alcalinity of ash',
'Magnesium','Total phenols','Flavanoids','Nonflavanoid phenols','Proanthocyanins'
,'Color intensity','Hue','OD280/OD315 of diluted wines','Proline')

#check the number of wines within each class
table(wine_df$class)

#check the class of the highest alcohol content
wine_df[which.max(wine_df$Alcohol),]$class

#check the class of the lowest alcohol content
wine_df[which.min(wine_df$Alcohol),]$class

#Count the number of wines have higher levels of magnesium than 114 mg/l
sum(wine_df$Magnesium>114)


#Count Within each class the number of wines have 
#higher levels of magnesium than average German beer
table(wine_df[which(wine_df$Magnesium>114),]$class)
