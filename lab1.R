# Creating a dataframe
# Example: RPI Weather dataframe

days <- c('Mon','Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun') # days
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4) # Temperature in F' during the winter
snowed <- c('T','T','F','F','T','T','F') # Snowed on that day:T = TRUE, F= FALSE
help("data.frame")
RPI_Weather_Week <- data.frame(days, temp, snowed) # creating the dataframe using the data.frame

RPI_Weather_Week
head(RPI_Weather_Week) # head of the data frame, NOTE: it will snow only 6 rows, usually head() function shows the 
#first 6 rows of the dataframe, hewe we have onlhy 7 rows in our dataframe.

str(RPI_Weather_Week) # we can take a look at the structure of the dataframe using the str() function

summary(RPI_Weather_Week) # summary of the dataframe using the summary function

RPI_Weather_Week[1,] # showing the 1st row and all columns
RPI_Weather_Week[,1] # showing the 1st column and all rows

RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week, SUBSET = snowed == TRUE)

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

# RPI_Weather_Week[descending_snowed,]
dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow
#Creating dataframes
#creating an empty dataframe
empty.dataframe <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df




EPI_data<-read.csv("~/Semster2/DA/HW1/2010EPI_data.csv",skip=1)
View(EPI_data)
attach(EPI_data)
fix(EPI_data) #launches a simple data editor
EPI
tf<-is.na(EPI)
E<-EPI[!tf]
summary(EPI) 
fivenum(EPI,na.rm=TRUE)
help(stem)
stem(EPI)
help(hist)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
help(lines)
lines(density(EPI,na.rm=TRUE,bw=1.)) 
help(rug)
rug(EPI) 
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(EPI)
qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


DALY
tf<-is.na(DALY)
E<-DALY[!tf]
summary(DALY) 
fivenum(DALY,na.rm=TRUE)
stem(DALY)
help(hist)
hist(DALY)
hist(DALY, seq(0., 100., 1.0), prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.)) 
rug(DALY) 
plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(DALY)
qqline(DALY)
 x<-seq(0,100,0.5)
qqplot(qt(ppoints(100), df = 5), x, xlab = "Q-Q plot for DALY")
qqline(x)

WATER_H
tf<-is.na(WATER_H)
E<-WATER_H[!tf]
summary(WATER_H) 
fivenum(WATER_H,na.rm=TRUE)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0., 100., 1.0), prob=TRUE)
lines(density(WATER_H,na.rm=TRUE,bw=1.)) 
rug(WATER_H) 
plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(WATER_H)
qqline(WATER_H)
x<-seq(0,100,0.5)
qqplot(qt(ppoints(100), df = 5), x, xlab = "Q-Q plot for WATER_H")
qqline(x)

boxplot(EPI,DALY)
qqplot(EPI,DALY)

boxplot(EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H)
help(distributions)
lmAIR_E<-lm(AIR_E~DALY+AIR_H+WATER_H)
DALY<-c(seq(5,95,5))
AIR_H<-c(seq(5,95,5))
WATER_H<-c(seq(5,95,5))
NEW<-data.frame(DALY,AIR_H,WATER_H)
pENV<- predict(lmAIR_E,NEW,interval="prediction")
cENV<- predict(lmAIR_E,NEW,interval="confidence")



EPI_data<-read.csv("~/Semster2/DA/HW1/2010EPI_data.csv",skip=1)
View(EPI_data)
attach(EPI_data)
fix(EPI_data) #launches a simple data editor
EPI

Eland
tf<-is.na(Eland)
E<-Eland[!tf]
summary(Eland) 
fivenum(Eland,na.rm=TRUE)
stem(Eland)
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
