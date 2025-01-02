## PH125.8x Data Science: Machine Learning
# In the Introduction to Machine Learning section, you will be introduced to machine learning.
# 
# After completing this section, you will be able to:
#   
# Explain the difference between the outcome and the features.
# Explain when to use classification and when to use prediction.
# Explain the importance of prevalence.
# Explain the difference between sensitivity and specificity.

#https://rafalab.dfci.harvard.edu/dsbook/
#https://github.com/rafalab/dsbook

# Section 1: Introduction to Machine Learning ----------------------
# Comprehension check
# Q1. True or False: A key feature of machine learning is that the algorithms are built with data.
# ANswer: T

# Q2. True or False: For prediction and classification tasks in machine learning, we build algorithms that take feature values (X) and train a model using known outcomes (Y) that is then used to predict outcomes when presented with features without known outcomes.
# ANswer: T

# Ask your questions or make your comments about Introduction to Machine Learning here! Remember, one of the best ways to reinforce your own learning is by explaining something to someone else, so we encourage you to answer each other's questions (without giving away the answers, of course). 
# 
# Some reminders:
# 
# Search the discussion board before posting to see if someone else has asked the same thing before asking a new question
# Please be specific in the title and body of your post regarding which question you're asking about to facilitate answering your question.
# Posting snippets of code is okay, but posting full code solutions is not.
# If you do post snippets of code, please format it as code for readability. If you're not sure how to do this, there are instructions in a pinned post in the "general" discussion forum


#Section 2: Machine Learning Basics --------------------------
# In the Machine Learning Basics section, you will learn the basics of machine learning.
# 
# After completing this section, you will be able to:
#   
# Start to use the caret package.
# Construct and interpret a confusion matrix.
# Use conditional probabilities in the context of machine learning.
# This section has two parts: basics of evaluating machine learning algorithms and conditional probabilities. There are comprehension checks at the end of each part.



#Evaluation metrics
# Note: the set.seed() function is used to obtain reproducible results. This course requires a R version of 3.6 or newer to obtain the same results when setting the seed.
# To mimic the ultimate evaluation process, we randomly split our data into two — a training set and a test set — and act as if we don’t know the outcome of the test set. We develop algorithms using only the training set; the test set is used only for evaluation.
# The createDataPartition() function from the caret package can be used to generate indexes for randomly splitting data.
# Note: contrary to what the documentation says, this course will use the argument p as the percentage of data that goes to testing. The indexes made from createDataPartition() should be used to create the test set. Indexes should be created on the outcome and not a predictor.
# The simplest evaluation metric for categorical outcomes is overall accuracy: the proportion of cases that were correctly predicted in the test set.

library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)

# compare heights in males and females in our data set
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# now try predicting "male" if the height is within 2 SD of the average male
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#Comprehension check 
#Q1 For each of the following, indicate whether the outcome is continuous or categorical.

# Digit reader -categorical
# Height - continuous
# Spam filter - categorical
# Stock prices - continuous
# Sex -  categorical

#Q2 How many features are available to us for prediction in the mnist digits dataset? You can download the mnist dataset using the read_mnist() function from the dslabs package.
library(dslabs)
mnist <- read_mnist()
ncol(mnist$train$images)

#Ask your questions or make your comments about Basics of Evaluating Machine Learning Algorithms here! Remember, one of the best ways to reinforce your own learning is by explaining something to someone else, so we encourage you to answer each other's questions (without giving away the answers, of course). 
# Some reminders:
# Search the discussion board before posting to see if someone else has asked the same thing before asking a new question
# Please be specific in the title and body of your post regarding which question you're asking about to facilitate answering your question.
# Posting snippets of code is okay, but posting full code solutions is not.
# If you do post snippets of code, please format it as code for readability. If you're not sure how to do this, there are instructions in a pinned post in the "general" discussion forum.

#Confusion matrix
# Overall accuracy can sometimes be a deceptive measure because of unbalanced classes.
# A general improvement to using overall accuracy is to study sensitivity and specificity separately. These will be defined in the next video.
# A confusion matrix tabulates each combination of prediction and actual value. You can create a confusion matrix in R using the table() function or the confusionMatrix() function from the caret package. The confusionMatrix() function will be covered in more detail in a later video.
# If your training data is biased in some way, you are likely to develop algorithms that are biased as well. The problem of biased training sets is so commom that there are groups dedicated to study it.

# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)


## Sensitivity, specificity, and prevalence
# Sensitivity, also known as the true positive rate or recall, is the proportion of actual positive outcomes correctly identified as such:  when . High sensitivity means that .
# Specificity, also known as the true negative rate, is the proportion of actual negative outcomes that are correctly identified as such:  when . High specificity means that .
# Specificity can also be thought of as the proportion of positive calls that are actually positive: that is, high specificity means that .
# Sensitivity is typically quantified by , the proportion of actual positives () that are called positives (). This quantity is also called the true positive rate (TPR) or recall.
# Specificity is typically quantified by , the proportion of actual negatives () that are called negatives (). This quantity is also called the true negative rate (TNR).
# Specificity can also be quantified by , the proportion of outcomes called positives () that are actually positives (). This quantity is called the positive predictive value (PPV) or precision.
# Prevalence is defined as the proportion of positives.
# Confusion matrix entries
# Actually positive	Actually negative
# Predicted positive	True positives (TP)	False positives (FP)
# Predicted negative	False negatives (FN)	True negatives (TN)
# Sensitivity and specificity measures table
# Measure of	Name 1	Name 2	Definition	Probability representation
# Sensitivity	TPR	Recall		
# Specificity	TNR	1 - FPR		
# Specificity	PPV	Precision		

# get the metrics
cm <- confusionMatrix(data = y_hat, reference = test_set$sex)

# access specific metrics
cm$overall["Accuracy"]

cm$byClass[c("Sensitivity","Specificity", "Prevalence")]


##Balanced accuracy and F1-score 
# For optimization purposes, sometimes it is more useful to have a one number summary than studying both specificity and sensitivity. One preferred metric is balanced accuracy. Because specificity and sensitivity are rates, it is more appropriate to compute the harmonic average. In fact, the F1-score, a widely used one-number summary, is the harmonic average of precision and recall. 
# Depending on the context, some type of errors are more costly than others. The F1-score can be adapted to weigh specificity and sensitivity differently. 
# You can compute the F1-score using the F_meas() function in the caret package.

# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff_2 <- cutoff[which.max(F_1)]
best_cutoff_2

y_hat <- ifelse(test_set$height > best_cutoff_2, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

##Prevalence matters in practice
# A machine learning algorithm with very high sensitivity and specificity may not be useful in practice when prevalence is close to either 0 or 1. For example, if you develop an algorithm for disease diagnosis with very high sensitivity, but the prevalence of the disease is pretty low, then the precision of your algorithm is probably very low based on Bayes' theorem.

##ROC and precision-recall curves
# A very common approach to evaluating accuracy and F1-score is to compare them graphically by plotting both. A widely used plot that does this is the receiver operating characteristic (ROC) curve. The ROC curve plots sensitivity (TPR) versus 1 - specificity, also known as the false positive rate (FPR).
# However, ROC curves have one weakness and it is that neither of the measures plotted depend on prevalence. In cases in which prevalence matters, we may instead make a precision-recall plot, which has a similar idea with ROC curve.

# Note: seed is not set so your results may slightly vary from those shown in the video.
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


##Loss function
# The most commonly used loss function is the squared loss function. Because we often have a test set with many observations, say N, we use the mean squared error (MSE). In practice, we often report the root mean squared error (RMSE), which is the square root of MSE, because it is in the same units as the outcomes.
# If the outcomes are binary, both RMSE and MSE are equivalent to one minus accuracy
# Note that there are loss functions other than the squared loss. For example, the Mean Absolute Error uses absolute values instead of squaring the errors. However, we focus on minimizing square loss since it is the most widely used.

##Comprehension Check: Practice with Machine Learning, Part 1
# The following questions all ask you to work with the dataset described below.
# 
# The reported_heights and heights datasets were collected from three classes taught in the Departments of Computer Science and Biostatistics, as well as remotely through the Extension School. The Biostatistics class was taught in 2016 along with an online version offered by the Extension School. On 2016-01-25 at 8:15 AM, during one of the lectures, the instructors asked student to fill in the sex and height questionnaire that populated the reported_heights dataset. The online students filled out the survey during the next few days, after the lecture was posted online. We can use this insight to define a variable which we will call type, to denote the type of student, inclass or online.
# 
# The code below sets up the dataset for you to analyze in the following exercises:

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#Q1
# The type column of dat indicates whether students took classes in person ("inclass") or online ("online"). What proportion of the inclass group is female? What proportion of the online group is female?
# Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.
# In class

q1_check <- dat %>% 
  group_by(type, sex) %>% 
  count() %>% 
  ungroup() %>% 
  janitor::adorn_totals() %>% 
  janitor::adorn_percentages("col") %>%
  janitor::adorn_pct_formatting() %>% 
  janitor::adorn_ns(position = "front")

#Online 

#Q2
# In the course videos, height cutoffs were used to predict sex. Instead of height, use the type variable to predict sex. Assume that for each class type the students are either all male or all female, based on the most prevalent sex in each class type you calculated in Q1. Report the accuracy of your prediction of sex based on type. You do not need to split the data into training and test sets.
# Enter your accuracy as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.


#Q3

# Write a line of code using the table() function to show the confusion matrix between y_hat and y. Use the exact format table(a, b) for your answer and do not name the columns and rows. Your answer should have exactly one space. Enter the line of code below.