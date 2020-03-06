set.seed(12345)
help(par)
par(mar=rep(0.2,4))
data_Martix <-matrix(rnorm(400),nrow=40)
image(1:10,1:40,t(data_Martix)[,nrow(data_Martix):1])
help("heatmap") 

par(mar=rep(0.2,4))
heatmap(data_Martix)

help("rbinom") 
set.seed(678910)
for(i in 1:40){
  #flipping a coin and getting the data
  coin_Flip <- rbinom(1,size =1,prob =0.5)
  #if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Martix[i,]<-data_Martix[i,]+rep(c(0,3),each=5)
  }
}

par(mar=rep(0.2,4))
image(1:10,1:40,t(data_Martix)[,nrow(data_Martix):1])

hh<-hclust(dist(data_Martix))
data_Martix_Ordered<-data_Martix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Martix_Ordered)[,nrow(data_Martix_Ordered):1])
plot(rowMeans(data_Martix_Ordered),40:1, , xlab="The Row Mean", ylab="Row",pch=19)
plot(colMeans(data_Martix_Ordered),xlab="Column",ylab="Column Mean",pch=19)
