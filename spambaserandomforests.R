library(randomForest)
set.seed(5)
#bagging
#bagging using random forest (mtry=p)
?randomForest
bag.spambase = randomForest(Spam ~ ., data = train_set, mtry = 58)
plot(bag.spambase)
bag.preds = predict(bag.spambase, newdata = test_set, type = "class")
table(bag.preds, test_set$Spam)
#this gives us something much better than the one tree!!

#what about using a random forest without all predictors at each stage??
rf.spambase = randomForest(Spam~., data = train_set, mtry = 22)
rf.preds = predict(rf.spambase, newdata = test_set, type = "class")
table(rf.preds, test_set$Spam)
#considering 22 random predictors at each split seems to give lower false positives at 29?

fp.rate = c()

#originally 5 to 15
for (i in 7:25){
  loop.rf.spambase = randomForest(Spam~., data = train_set, mtry = i)
  loop.rf.preds = predict(loop.rf.spambase, newdata = test_set, type = "class")
  fp.no = table(loop.rf.preds, test_set$Spam)[1,2]
  tn.no = table(loop.rf.preds, test_set$Spam)[2,2]
  fp.rate[i] = (fp.no) / (fp.no + tn.no)
}
fp.rate #seems lowest for 14 or 15 vars
#then check the accuracy for 14 and 15

which.min(fp.rate)
fp.rate[7:25]
plot(x = 7:25, y = fp.rate[7:25]*100, xlab = "Value of m", 
     ylab = "False positive rate (%)", col = 'black', pch = 16)
axis(1, 7:25)

rf.8.spambase = randomForest(Spam~., data = train_set, mtry = 8)
rf.8.preds = predict(rf.8.spambase, newdata = test_set, type = "class")
table(rf.8.preds, test_set$Spam)

rf.13.spambase = randomForest(Spam~., data = train_set, mtry = 13)
rf.13.preds = predict(rf.13.spambase, newdata = test_set, type = "class")
table(rf.13.preds, test_set$Spam)

rf.15.spambase = randomForest(Spam~., data = train_set, mtry = 15)
rf.15.preds = predict(rf.15.spambase, newdata = test_set, type = "class")
table(rf.15.preds, test_set$Spam)

importance(rf.13.spambase)
tab = table(importance(rf.13.spambase))
tab
