#DOWNLOADING AIRBNB DATA FROM KAGGLE AND READING IT TO A DATAFRAME
dfAirBnB=read.csv("AB_NYC_2019.csv")
head(dfAirBnB)
#cleaning data before analysis
#removing omitting na values
dfAirBnB=na.omit(dfAirBnB)
#view dataframe
View(dfAirBnB)
summary(dfAirBnB$host_id)
result=aggregate(host_name~host_id,data=dfAirBnB,FUN=length)
head(result)
tail(result)
dim(result)
#plotting graph between neighbourhood group and host id
table(dfAirBnB$neighbourhood)
result=table(dfAirBnB$neighbourhood_group)
dim(result)
neighbourhoodgroups=c("Bronx","Brroklyn","Manhattan","Queens","Staten Island")
barplot(result, main = "Listings in Neighbourhood Group", names.arg = neighbourhoodgroups , xlab = "Neighbourhood Groups", ylab = "Listings in Neighbourhood", col =result )
#host ids with multiple listings on Airbnb
#resultids=dfAirBnB[,dfAirBnB$calculated_host_listings_count>1]
resultids=subset(dfAirBnB,dfAirBnB$calculated_host_listings_count>1)
head(resultids)
tail(resultids)
#no of values with calculated host listings>1
dim(resultids)
View(resultids)
result=aggregate(host_name~host_id,data=resultids,FUN=length)
result=unique(dfAirBnB$host_id)
length(result)
dim(result)
#decision tree for high price or low price
#dropping id,name,host_id,host_name

dfAirBnB1=dfAirBnB[,-(1:4)]
head(dfAirBnB1)
#dropping lattitude and longitude
dfAirBnB1=dfAirBnB1[,-(3:4)]
head(dfAirBnB1)
View(dfAirBnB1)
dfAirBnB1=dfAirBnB1[,-7]
#getting data information about price column using summary 
summary(dfAirBnB1$price)
#getting mean price of room listing
mean(dfAirBnB1$price)
#creating a column with values above mean and below mean as 
#above or below
AboveMean=ifelse(dfAirBnB1$price>mean(dfAirBnB1$price),"Yes","No")
#combine above mean and airbnb1
dfAirBnB1=data.frame(dfAirBnB1,AboveMean)
View(dfAirBnB1)
set.seed(4)
train=sample(1:nrow(dfAirBnB),100)
AirBnB.test=dfAirBnB1[-train,]
AboveMean.test=dfAirBnB1[-train]
library(tree)
dfAirBnB1=dfAirBnB
dfAirBnB1=dfAirBnB[4,-3]
tree.AirBnB=tree(AboveMean~.,data=dfAirBnB1)
View(dfAirBnB1)

#####
#####
#subset selection
data=read.csv("AB_NYC_2019.csv")
data=na.omit(data)
View(data)
library(leaps)
#cleaning data by removing columns which have qualitative values
#removing the first four columns
data=data[,-(1:4)]
View(data)
data=data[,-(2:4)]
View(data)
data=data[,-(6:8)]
View(data)
#running regsubsets with price as dependent and all other columns as predictors
#to find best subset of predictors
regfit.full=regsubsets(price~.,data=data)
#understanding different values in regfit
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
reg.summary$adjr2
reg.summary$cp
#trying to find the number of predictors which will lead to
#maximum adjusted r2
which.max(reg.summary$adjr2)
#determining the coefficients for 8 predictors
coef(regfit.full,8)
#plotting a graph between adjusted r2 and no of predictors
plot(reg.summary$adjr2,xlab="Number of predictors",ylab="Adjusted R^2")
#k clustering
#read the file 
airbnb=read.csv("AB_NYC_2019.csv",na.string="?")
airbnb=na.omit(airbnb)
head(airbnb)
#clean the data and keep the useful variables 
airbnb1=airbnb[,c(2,5,6,9,10,12,14)]
head(airbnb1)
dim(airbnb1)
colnames(airbnb1)
Price=airbnb1[,2]
maxprice=max(Price)
summary(airbnb1$price)
#Max price among all the NY areas
Info_MaxPrice=subset(airbnb1,airbnb1$price==maxprice,select=name:review_per_month)
Info_MaxPrice
#read the varibles sizes 
table(airbnb1$neighbourhood)
table(airbnb1$neighbourhood_group)
#set the seed 
set.seed(3)
#get the average price for each neighbourhood
n_price=aggregate(airbnb1$price~airbnb1$neighbourhood, data=airbnb1,FUN=mean)
head(n_price)
#get the average reviews per month for each neighbourhood
n_review=aggregate(airbnb1$reviews_per_month~airbnb1$neighbourhood, data=airbnb1,FUN=mean)
#merge those variables and create the data frame for our clustering model 
forcluster=merge(n_price,n_review)
head(forcluster)
#set the rownames to neighbourhood names 
rownames(forcluster)=forcluster[,1]
head(forcluster)
forcluster=forcluster[,-1]
head(forcluster)
#run the Hierarchial Clustering model
hc.complete=hclust(dist(forcluster),method="complete")
#plot the clutser dendrogram
plot(hc.complete)
#cut the data set into 15 different clusters
hc.out=cutree(hc.complete,15) 
#the assigned cluster for each neighbourhood
hc.out
#plot the clustered neighbourhoods 
forcluster$neighbourhood=rownames(forcluster)
plot(forcluster$`airbnb1$price`,forcluster$`airbnb1$reviews_per_month`,col=hc.out,pch=hc.out,xlab="average price",ylab="average review per month")
text(x=forcluster$`airbnb1$price`,y=forcluster$`airbnb1$reviews_per_month`-0.15,labels=forcluster$neighbourhood,col=hc.out)

#model selection 
#tring to find the maximum price in air
library(ISLR)
aggregate(airbnb1$price~airbnb1$neighbourhood_group,data=airbnb1,FUN=max)
aggregate(airbnb1$price~airbnb1$neighbourhood_group,data=airbnb1,FUN=mean)
airbnb1$roomprivate=0
airbnb1$roomprivate[airbnb1$room_type=="Shared room"]=1
airbnb1=airbnb1[,c(2,5,6,7,8)]

head(airbnb1)
set.seed(1)
train=sample(1:nrow(airbnb1),nrow(airbnb1)/2)
head(train)
airbnb1.train=airbnb1[train,]
head(airbnb1.train)
airbnb1.test=airbnb1[-train,]
head(airbnb1.test)


x=model.matrix(airbnb1.train$price~.,data=airbnb1.train)[,-1]
head(x)
y=airbnb1.train$price
head(y)
install.packages("Matrix")
library(glmnet)
grid=10^seq(2,-2,length=100)
grid
lasso1=glmnet(x,y,alpha=1,lambda=grid,thresh=1e-12)
dim(coef(lasso1))
lasso1$lambda[50]
coef(lasso1)[,50]
x.test=model.matrix(airbnb1.test$price~.,data=airbnb1.test)[,-1]
y.test=airbnb1.test$price
lasso.pred=predict(lasso1,s=4,newx=x.test)
#the MSE
mean((lasso.pred-y.test)^2)
#10 fold cross-validation 
set.seed(1)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.final=glmnet(x,y,alpha=1,lambda=bestlam)
coef(lasso.final)
