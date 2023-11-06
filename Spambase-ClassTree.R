colnames(spambase) = c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d", 
"word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet", 
"word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will", 
"word_freq_people", "word_freq_report", "word_freq_addresses", "word_freq_free", 
"word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit", 
"word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money", "word_freq_hp", 
"word_freq_hpl", "word_freq_george", "word_freq_650", "word_freq_lab", "word_freq_labs", 
"word_freq_telnet", "word_freq_857", "word_freq_data", "word_freq_415", "word_freq_85", 
"word_freq_technology", "word_freq_1999", "word_freq_parts", "word_freq_pm", "word_freq_direct", 
"word_freq_cs", "word_freq_meeting", "word_freq_original", "word_freq_project", "word_freq_re", 
"word_freq_edu", "word_freq_table", "word_freq_conference", "char_freq_semicol", 
"char_freq_leftpar", "char_freq_leftbrac", "char_freq_exclam", "char_freq_dollar", "char_freq_hash", 
"capital_run_length_average", "capital_run_length_longest", "capital_run_length_total", "Spam")

library(tree)
spambase = data.frame(spambase)
spambase$Spam = as.factor(spambase$Spam)
?tree
set.seed(5)
rows = nrow(spambase)
train_ind = sample(1:rows, 0.8*rows)
train_set = spambase[train_ind,]
test_set = spambase[-train_ind,]

tree.spambase = tree(Spam ~ . , data= train_set)
summary(tree.spambase)
test_preds = predict(tree.spambase, test_set, type = "class")
table(test_preds, test_set$Spam)

#61 false positives

#now we try pruning the tree?
cv.spambase = cv.tree(tree.spambase, FUN = prune.misclass)
cv.spambase
plot(cv.spambase)
prune.spambase = prune.misclass(tree.spambase, best = 10)
plot(prune.spambase); text(prune.spambase, pretty=0)

prune_test_preds = predict(prune.spambase, test_set, type = 'class')
table(prune_test_preds, test_set$Spam)


