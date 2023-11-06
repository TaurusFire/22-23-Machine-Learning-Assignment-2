#boosting
library(gbm)
set.seed(5)
boost.spambase = gbm(as.character(Spam)~., data = train_set, distribution = "bernoulli", n.trees = 1000)
boost.preds = predict(boost.spambase, newdata = test_set, type = "response")
boost.preds_classes = sapply(boost.preds, FUN = function(i){
  if (i < 0.5) {
     0
  } else {
     1
  }
})
table(boost.preds_classes, test_set$Spam)


fp_calculator = function(table){
  fp.no = table[1,2]
  tn.no = table[2,2]
  fp.rate = ((fp.no)/(fp.no + tn.no)) * 100
  return (fp.rate)
}

acc_calculator = function(table){
  acc = ((table[1,1]+table[2,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2])) * 100
  return (acc)
}

table_for = function(number){
  set.seed(5)
  boost.spambase = gbm(as.character(Spam)~., data = train_set, 
                       distribution = "bernoulli", n.trees = number)
  boost.preds = predict(boost.spambase, newdata = test_set, type = "response")
  boost.preds_classes = sapply(boost.preds, FUN = function(s){
    if (s < 0.5) {
      0
    } else {
      1
    }
  })
  n_table = table(boost.preds_classes, test_set$Spam)
  return (n_table)
}

tree_stats = matrix(data= NA, nrow = 7, ncol = 3, dimnames = NULL)
for (i in 1:7){
  set.seed(5)
  num = (500 + i*500)
  iter_table = table_for(num)
  print(iter_table)
  fp = fp_calculator(iter_table)
  acc = acc_calculator(iter_table)
  tree_stats[i,1] = num
  tree_stats[i,2] = fp
  tree_stats[i,3] = acc
  print(tree_stats)
}


