# Install packages as required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")

# Load required libraries
library(tidyverse)
library(caret)
library(e1071)

# Set global option for significant digits
options(digits = 4)

# Load data from UCI site
url_uci <- 
        "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"
spambase <- read.csv(url(url_uci), header=FALSE)

# Insert column names
colnames(spambase) <- c("f_make", "f_address", "f_all", "f_3d", "f_our", "f_over", "f_remove", "f_internet", 
                        "f_order", "f_mail", "f_receive", "f_will", "f_people", "f_report", "f_addresses",
                        "f_free", "f_business", "f_email", "f_you", "f_credit", "f_your", "f_font", 
                        "f_000", "f_money", "f_hp", "f_hpl", "f_george", "f_650", "f_lab", "f_labs", "f_telnet",
                        "f_857", "f_data", "f_415", "f_85", "f_technology", "f_1999", "f_parts", "f_pm",
                        "f_direct", "f_cs", "f_meeting", "f_original", "f_project", "f_re", "f_edu", "f_table", 
                        "f_conference", "f_;", "f_(", "f_[", "f_!", "f_$", "f_3", 
                        "c_average", "c_longest", "c_total", "spam")

# Classify spam outcome as factor of 2 levels "0" (false) and "1" (true)
spambase$spam <- factor(spambase$spam, levels = c(0,1))

# Set aside 10% of dataset as validation set
set.seed(2021, sample.kind="Rounding") 
val_index <- createDataPartition(spambase$spam, times = 1, p = 0.1, list = FALSE)
temp <- spambase[-val_index,]
val_set <- spambase[val_index,]

# Split remaining data into train_set (80%) and test_set (20%)
set.seed(2021, sample.kind = "Rounding")
test_index <- createDataPartition(temp$spam, times = 1, p = 0.2, list = FALSE)
train_set <- temp[-test_index,]
test_set <- temp[test_index,]

# Global setting for number of significant digits
options(digits = 4)

# Tabulate datasets and number of rows in each
data.frame(Number_of_rows = c(nrow(train_set), nrow(test_set), nrow(val_set)), 
           row.names = c("train_set", "test_set", "Validation set"))

# First 3 rows of spambase dataset
head(spambase, 3)

# Proportion of spam emails
sum(spambase$spam==1)/nrow(spambase)

# Show summary statistics of of first 6 columns of spambase dataset
summary(spambase[1:8])

# Plot 4 histograms in 2 rows of 2
par(mfrow = c(2,2))
hist(spambase$f_make, main = NULL, xlab = "word = 'make'")
hist(spambase$f_our, main = NULL, xlab = "word = 'our'")
hist(spambase$f_george, main = NULL, xlab = "word = 'george'")
hist(spambase$f_project, main = NULL, xlab = "word = 'project'")

# Plot variables with highest frequencies of occurrence in spam and non-spam emails
# Number of spam emails
num_spam_1 <- sum(spambase$spam==1)
# Number of non-spam emails
num_spam_0 <- sum(spambase$spam==0)
# Frequency of words per spam email
freq_w_1 <- colSums(train_set[which(train_set$spam==1), 1:48])/num_spam_1
# Frequency of words per non=spam email
freq_w_0 <- colSums(train_set[which(train_set$spam==0), 1:48])/num_spam_0
# Put bar charts of average frequencies of words for spam and non-spam emails side-by-side
# Average frequency of words for spam email in descending order
order_w_1 <- freq_w_1[order(-freq_w_1)]
# Average frequency of words for non-spam email in descending order
order_w_0 <- freq_w_0[order(-freq_w_0)]
# 1 row of 2 plots
par(mfrow = c(1,2))
# Set same y-axis scales with ylim for comparability
barplot(order_w_1, ylim = c(0, 2), xlab = "Words in spam emails", 
        ylab = "Average frequency", xaxt = 'n')
barplot(order_w_0, ylim = c(0, 2), xlab = "Words in non-spam emails", 
        ylab = "Average frequency", xaxt = 'n')

# Plot top 5 frequency words of spam and non-spam emails side-by-side
# 1 row of 2 plots
par(mfrow = c(1,2))
barplot(order_w_1[1:5], main = "Words in spam emails", ylim = c(0, 2), las=2, 
        ylab = "Average frequency", col = "yellow")
barplot(order_w_0[1:5], main = "Words in non-spam emails", ylim = c(0, 2), las=2, 
        ylab = "Average frequency", col = "cyan")

# Frequency of characters per spam email
freq_c_1 <- colSums(train_set[which(train_set$spam==1), 49:54])/num_spam_1
# Frequency of characters per non=spam email
freq_c_0 <- colSums(train_set[which(train_set$spam==0), 49:54])/num_spam_0
# Put bar charts of average frequencies of characters for spam and non-spam emails side-by-side
# 1 row of 2 plots
par(mfrow = c(1,2))
# Set same y-axis scales with ylim for comparability
barplot(freq_c_1[order(-freq_c_1)], main = "Characters in spam emails", 
        ylim = c(0, 0.4), ylab = "Average frequency", col = "yellow", 
        cex.main = 1.1, cex.names = 0.8)
barplot(freq_c_0[order(-freq_c_0)], main = "Characters in non-spam emails",
        ylim = c(0, 0.4), ylab = "Average frequency", col = "cyan", 
        cex.main = 1.1, cex.names = 0.8)

# Capital letter occurrences per spam email
cap_1 <- colSums(train_set[which(train_set$spam==1), 55:57])/num_spam_1
# Capital letter occurrences per non=spam email
cap_0 <- colSums(train_set[which(train_set$spam==0), 55:57])/num_spam_0
# Insert a row of 3 bar charts with 3 capital letter attributes
par(mfrow = c(1,3))
# Plot of average length of uninterrupted sequences of capital letters
barplot(c(cap_1[1], cap_0[1]), main = "Average cap length", 
        names.arg = c("Spam", "Non-Spam"), col = c("yellow", "cyan"))
# Plot of avearge longest sequence of capital letters
barplot(c(cap_1[2], cap_0[2]), main = "Average longest cap length", 
        names.arg = c("Spam", "Non-Spam"), col = c("yellow", "cyan"))
# Plot of average number of capital letters
barplot(c(cap_1[3], cap_0[3]), main = "Average cap letters", 
        names.arg = c("Spam", "Non-Spam"), col = c("yellow", "cyan"))

# Accuracy function
accuracy <- function(predicted_spam, true_spam){
        confusionMatrix(predicted_spam, true_spam)$overall["Accuracy"]
}

# Just guessing
set.seed(2021, sample.kind = "Rounding")
# Generate guesses for all observations in test_set
pred_guess <- as.factor(sample(c(0,1), nrow(test_set), replace = TRUE))
# Check accuracy
accuracy_guess <- accuracy(pred_guess, test_set$spam)
accuracy_guess

# Select "spam" column (no. 58) and place into y and place rest of columns into x
y <- train_set[, 58]
x <- train_set[, -58]

# Naive Bayes
# Obtain fit by training using Naive Bayes method
train_nb <- train(x, y, method = "naive_bayes")
# Calculate predictions using Naive Bayes fit
pred_nb <- predict(train_nb, test_set)
# Check accuracy
accuracy_nb <- accuracy(pred_nb, test_set$spam)
accuracy_nb

# Logistic regression
# Obtain fit by training using GLM method
train_glm <- train(x, y, method = "glm")
# Calculate predictions using GLMs fit
pred_glm <- predict(train_glm, test_set, type = "raw")
# Check accuracy
accuracy_glm <- accuracy(pred_glm, test_set$spam)
accuracy_glm

# Support Vector Machine (SVM)
# Obtain fit by training using SVM method
train_svm <- svm(spam ~ ., kernel = "linear", data = train_set)
# Calculate predictions using SVM fit
pred_svm <- predict(train_svm, test_set, type = "raw")
# Check accuracy
accuracy_svm <- accuracy(pred_svm, test_set$spam)
accuracy_svm

# K-Nearest Neighbours or KNN
set.seed(2021, sample.kind = "Rounding")
# Obtain fit by training using KNN method
train_knn <- train(x, y, method = "knn")
# Calculate predictions using KNN fit
pred_knn <- predict(train_knn, test_set, type = "raw")
# Check accuracy
accuracy_knn <- accuracy(pred_knn, test_set$spam)
accuracy_knn

# Random forest
set.seed(2021, sample.kind = "Rounding")
# Obtain fit by training using Random Forest (RF) method
train_rf <- train(spam ~ ., method = "rf", data = train_set)
# Calculate predictions using RF fit
pred_rf <- predict(train_rf, test_set, type = "raw")
# Check accuracy
accuracy_rf <- accuracy(pred_rf, test_set$spam)
accuracy_rf

#Tabulate results
results <- data.frame(Method=c("Just guess", "Naive Bayes", "GLM", "SVM", "KNN"
                               , "RandomForest"), 
                      Accuracy=c(accuracy_guess, accuracy_nb, accuracy_glm,
                                 accuracy_svm, accuracy_knn, accuracy_rf))
results

# Calculate false positive rates of the various methods
preds <- list(pred_guess, pred_nb, pred_glm, pred_svm, pred_knn, pred_rf)
false_pos <- sapply(preds, function(pred){
        sum(pred==1 & test_set$spam==0)/nrow(test_set)
})

#Tabulate results with false positive rates
results_fp <- data.frame(Method=c("Just guess", "Naive Bayes", "GLM", "SVM", 
                                  "KNN", "RandomForest"), 
                         Accuracy=c(accuracy_guess, accuracy_nb, accuracy_glm,
                                    accuracy_svm, accuracy_knn, accuracy_rf),
                         False_positive=c(false_pos[1:6]))
results_fp

# Use Random Forest on validation set
pred_val_rf <- predict(train_rf, val_set, type = "raw")
# Check accuracy
accuracy_val_rf <- accuracy(pred_val_rf, val_set$spam)
accuracy_val_rf

# Check false positive rate
falsepos_val_rf <- sum(pred_val_rf==1 & val_set$spam==0)/nrow(val_set)
falsepos_val_rf


        