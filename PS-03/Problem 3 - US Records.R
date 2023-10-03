us_500 <- read.csv("~/us-500.csv",header=TRUE)

length(grep("\\.net$", us_500$email))/length(us_500$email)

length(grep("[^a-zA-Z0-9]+[a-zA-Z0-9]*@", us_500$email))/length(us_500$email)

area_code<-table(sapply(us_500$phone1,substr,start=1,stop=3))
area_code_most_common<-area_code[which.max(area_code)]

# https://stackoverflow.com/questions/9704213/remove-part-of-a-string
address_apt<-us_500$address[grep(".+#", us_500$address)]
apt_num<-gsub(pattern=".+#", replacement = "", address_apt)
barplot(table(apt_num_leading))

apt_num_leading<-as.numeric(substr(apt_num,1,1))

apt_num_leading_dist<-table(apt_num_leading)/length(apt_num_leading)
benford<-log(1+1/c(1:9),base=10)
names(benford)<-1:9
cbind(apt_num_leading_dist,benford)

st_split<-strsplit(us_500$address," ")
st_num <- as.numeric(sapply(st_split, function(x) x[1]))
st_num_last<-st_num %%10
st_num_last<-st_num_last[st_num_last>0]

barplot(table(st_num_last))
st_num_last_dist<-table(st_num_last)/length(st_num_last)
cbind(st_num_last_dist,benford)
