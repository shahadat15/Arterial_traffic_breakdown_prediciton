library(rpart)
library(randomForest)


#read file
a<-read.csv(file.choose())


# grow tree
b<-rpart(bk~Time+occ4+vol4+spd4+sdoc4+sdvol4+sdspd4+occ7+vol7+spd7+sdocc7+sdvol7+sdspd7, data=a, method="class")

# plot tree 
plot(b, uniform=TRUE, main="Classification Tree for Kyphosis")
text(b, use.n=TRUE, all=TRUE, cex=.8)
printcp(b) # display the results
plotcp(b) # visualize cross-validation results 
rsq.rpart(b)
summary(b) # detailed summary of splits

#randomForest 
r<-randomForest(bk~avg_occ_4+tot_vol_4+wt_med_spd_4+sd_occ_4+sd_vol_4+sd_med_spd_4+avg_occ_1+tot_vol_1+wt_med_spd_1+sd_occ_1+sd_vol_1+sd_med_spd_1, data=a, ntree=50)

# to find the optimum no of trees
plot(randomForest(bk ~ ., a, keep.forest=FALSE, ntree=100), log="y")
print(r) # view results 
importance(r) # importance of each predictor

## Look at the third trees in the forest.
w<-getTree(r, 3, labelVar=TRUE)
iris.rf <- randomForest(bk ~ ., a, keep.forest=FALSE)
plot(margin(iris.rf))

#Resubstitution error rate
pred<- predict(b, newdata=a, type = "class")
mc<-table(a$bk,pred)
print(mc)
err.resub<-1.0-(mc[1,1]+mc[2,2])/sum(mc)
print(err.resub)


#Cross validation
n<-nrow(a)
k<-2
taille<-n%/%k
set.seed(5)
alea<-runif(n)
rang<-rank(alea)
bloc<-(rang-1)%/%taille+1
bloc<-as.factor(bloc)
print(summary(bloc))

all.err<-numeric(0)
for(k in 1:k){
arbre<-rpart(bk~.,data=a[bloc!=k,],method="class")
pred<-predict(arbre,newdata=a[bloc==k,],type="class")
mc<-table(a$bk[bloc==k],pred)
err<-1.0-(mc[1,1]+mc[2,2])/sum(mc)
all.err<-rbind(all.err,err)
}

print(all.err)
err.cv<-mean(all.err)
print(err.cv)
pred<- predict(arbre, newdata=a, type = "class")
mc<-table(a$bk,pred)
print(mc)
