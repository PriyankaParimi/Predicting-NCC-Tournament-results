library("dplyr")
library("boot")
library("pls")
library("glmnet")
library("FNN")

#Loading Regular Session Details

myData = RegularSeasonDetailedResults

#Performing Data Wrangling

losses = myData[c(1,5:6,22:34)]
wins = myData[c(1,3:4,9:21)]
names(losses)<- c("Season","team","score","fgm","fga","fgm3","fga3","ftm","fta","or","dr","ast","to","stl","blk","pf")
names(wins)<- c("Season","team","score","fgm","fga","fgm3","fga3","ftm","fta","or","dr","ast","to","stl","blk","pf")
finalData = rbind(losses,wins)
wrangledData=data.frame(finalData %>% group_by(Season,team) %>% summarise_all(funs(mean)))
knormal = kmeans(wrangledData[,3:16] , 8)  # between_SS / total_SS =  59.5 %

#Scaling 

scaledData=scale(wrangledData[,3:16])
kscaled= kmeans(scaledData,8)
kscaled   # between_ss / total_SS = 43.3%
plot(scaledData, col=kscaled$cluster,main="Clustering with Scaled Data")


#PCA

pca_data = prcomp(scaledData, scale. = FALSE, center = TRUE    )
head(pca_data)
pca_data.var =pca_data$sdev ^2
pve=pca_data.var/sum(pca_data.var )
pve
plot(pve , xlab=" Principal Component ", ylab=" Proportion of
     Variance Explained ", ylim=c(0,1) ,type='b',main="Principal Component")
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
     Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b',main="Cumulative Variance")
pca_data.scaled = scale(pca_data$x[,1:4])
pca_kmeans= kmeans(pca_data.scaled,8)
pca_kmeans         # between_ss / total_SS = 54.5%
TourneyData= TourneyCompactResults
View(TourneyData)
TourneyData <- TourneyData[ which( TourneyData$Season > 2002),]

wTeam = TourneyData[c(1,3:4)]
lTeam=TourneyData[c(1,5:6)]
names(lTeam)<-c("Season","team","Score")
names(wTeam)<-c("Season","team","Score")
finalTourney<-rbind(lTeam,wTeam)
finalTourney
wrangledData<-wrangledData[-c(3)]
head(wrangledData)
final=merge(wrangledData,finalTourney,by=c("Season","team"))
head(final)

#Using linear regression

glm.fit = glm(Score~., data=final)
cv.out = cv.glm(final, glm.fit, K=10)
rmse_lr = sqrt(cv.out$delta[1])
rmse_lr
summary(final$Score)

#Using Ridge
x=model.matrix(Score~.,final)[,-1]
y=final$Score

grid=10^seq(10,-2,length=100)

ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
plot(ridge.mod,main="Ridge")
dim(coef(ridge.mod))
names(ridge.mod)
predict(ridge.mod,s=50,type="coefficients")[1:16,]
set.seed(1)
cv.out=cv.glmnet(x,y,alpha=0)

plot(cv.out,main="Ridge CV")
bestlam=cv.out$lambda.min
bestlam
min(cv.out$cvm)
rmse_ridge = sqrt(min(cv.out$cvm))
rmse_ridge

#Using lasso

lasso.mod =glmnet (x,y,alpha =1, lambda =grid)
plot(lasso.mod,main="Lasso")


set.seed (1)
cv.out =cv.glmnet (x,y,alpha =1)
plot(cv.out,main="Lasso CV")
bestlam =cv.out$lambda.min
bestlam
min(cv.out$cvm)
rmse_lasso = sqrt(min(cv.out$cvm))
rmse_lasso


lasso.coef=predict(lasso.mod,type ="coefficients",s=bestlam )[1:16,]
lasso.coef
lasso.coef[lasso.coef!=0]

#Using PCR

train = sample(1:1828,1600)
training_Set =final[train,]
testing_Set = final[-train,]
train.x = scale(x[train,])
test.x = scale(x[-train,])
train.y = y[train]
test.y = y[-train]
pcr.fit = pcr(Score~., data=training_Set, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP",main="PCR")
summary(pcr.fit)
predictions <- predict(pcr.fit, testing_Set, ncomp=1:15)
mse_pcr <- mean((predictions - test.y)^2)
rmse_pcr = sqrt(mse_pcr)
print(rmse_pcr)

#Using KNN

train = sample(1:1828,1600)
train.x = scale(x[train,])
test.x = scale(x[-train,])
train.y = y[train]
test.y = y[-train]

bins = sample(1:10,1828, replace = TRUE)
binErrs = rep(0,10)
errs = rep(0,15)
for(i in 1:15){
  for(k in 1:10){
    train.x = scale(x[bins != k,])
    test.x = scale(x[bins == k,])
    train.y = y[bins != k]
    test.y = y[bins == k]
    knn.fit = knn.reg(train.x, test.x, train.y, k=i)
    binErrs[k] = mean((test.y - knn.fit$pred)^2)
  }
  errs[i] = mean(binErrs)
}
errs

mse_knn = sum(errs)/length(errs)
rmse_knn = sqrt(mse_knn)
print(rmse_knn)



