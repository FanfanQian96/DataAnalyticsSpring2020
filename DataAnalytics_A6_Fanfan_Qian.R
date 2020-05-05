rm(list=ls())
house<-read.csv("~/Semster2/DA/Project/houseprice.csv",sep=",")
house1<-as.data.frame(house)
View(house)
library(ggplot2)
qqnorm(house$square) 
ppnorm(house$square)
install.packages("ggmap")
install.packages("mapview")
library(ggmap)
library(sp)
library(mapview)
coordinates (house) <- ~ Lng + Lat
proj4string(house) <- CRS("+init=epsg:4326")
mapview(house)
register_google(key = "AIzaSyCVdoBt0YYUhpiYoL2diGTrNZAXC9v4Dok")

newmap5 <- get_googlemap(location = c(-71.2612362452596,42.3308503846824
                                      ,-71.0475647202879,42.4560226746649), source = 'google',
                         maptype = "satellite")
rm(list=ls())

#load packages
library(randomForest)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
#load data
rm(list=ls())
data <- read.csv("~/Semster2/DA/Project/houseprice.csv")
#basic info
summary(data)
dim(data)
head(data)
sapply(data,class)
sum(is.na(data))
missing<-colSums(is.na(data))[colSums(is.na(data))>0]%>% sort((decreasing=T))
missing
install.packages("naniar")
library(naniar)
gg_miss_upset(data)
#the proportion of missing data is quite low thus i delete the missing data directly.
data <- data[complete.cases(data),]
dim(data)
data$DOM<-NULL
data$buildingType<-NULL
data$Lng<-NULL
data$Lat<-NULL
data$renovationCondition<-NULL
data$constructionTime<-NULL

#data transformation

data$constructionTime <- as.numeric(data$constructionTime)
data$district <- as.factor(data$district)
data$elevator <- as.factor(data$elevator)
data$subway <- as.factor(data$subway)
data$district <- as.factor(data$district)
data$fiveYearsProperty <- as.factor(data$fiveYearsProperty)
#data visualization
#histogram
table(data$totalPrice)
ggplot(data,aes(x=square,y=totalPrice))+geom_point()
ggplot(data)+geom_density(aes(x=totalPrice,fill=district))
ggplot(data)+geom_histogram(aes(x=totalPrice, fill=district))
ggplot(data)+geom_histogram(aes(x=unitprice, fill=district))


data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
#density plot
data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()




nums <- unlist(lapply(data, is.numeric))  
model_data_num <- data[,nums]
library(corrplot)
corrplot::corrplot.mixed(cor(model_data_num), add=TRUE, type="lower", method="number",order="AOE",tl.pos="n", cl.pos="n")
corrplot(corr=cor(model_data_num),order = "AOE",type="upper",tl.pos = "d")
corrplot(corr = cor(model_data_num),add=TRUE, type="lower", method="number",order="AOE",diag=FALSE,tl.pos="n", cl.pos="n")

nums <- unlist(lapply(data, is.numeric))  
model_data_num <- data[,nums]
corrplot::corrplot(cor(model_data_num))
###model
#linear regression
d <- density(data$unitprice) # returns the density data
plot(d) # plots the results

set.seed(1234)
train <- data[sample(nrow(data),2*nrow(data)/3),]
dim(train)
test <- data[-sample(nrow(data),2*nrow(data)/3),]
dim(test)
lm.model <- lm(unitprice~.,data=train[,!names(train)%in%c('tradeTime')])
summary(lm.model)
pred <- predict(lm.model,newdata=test)
mean(abs(pred-test$unitprice)/test$unitprice)
sqrt(sum((pred-test$totalPrice)^2)/length(pred))
plot(lm.model)

#randomforest
rf.model <- randomForest(unitprice~.,
                         data=train[sample(nrow(train),10000),!names(train)%in%c('tradeTime')])
rf.model
varImpPlot(rf.model)
pred <- predict(rf.model,newdata=test)
mean(abs(pred-test$unitprice)/test$unitprice)

