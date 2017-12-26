#rm(list = ls())
#packages
library(dplyr)
##############################
list.files()
setwd("/Users/Advait/Desktop/New_School/Fall17/Bayes_ML/HW_1/EECS6720-hw1-data")
#54 should be the number of words and X is called a 54 dimensional vector.
#load the train feature matrix
X_train <- read.csv("X_train.csv",
                    header = F)
dim(X_train)

#load the test feature matrix
X_test <- read.csv("X_test.csv",
                   header = F)
dim(X_test)


#load the test labels
y_train <- read.csv("label_train.csv",
                   header = F)
dim(y_train)

#load the test labels
y_test <- read.csv("label_test.csv",
                   header = F)
dim(y_test)
#vector of 460 rows
#########################
#    Question 4 A    #
#########################

#########################
#Naive Bayes Classifier#
#########################
adv8_naiveBayes <- function(new_data,
                            data){
  spam_counter <- vector()
  ham_counter <- vector()
  spam_probs <- vector()
  ham_probs <- vector()
  a <- 1
  b <- 1
  e <- 1
  f <- 1
  N <- 4140
  spam_features <- data[data[,1] == 1,][-1]
  spam_labels <- data[data[,1] == 1,][1]
  ham_features <- data[data[,1] == 0,][-1]
  ham_labels <- data[data[,1] == 0,][1]
  
  #the prior part
  #(spam,ham)
  p_ystar <- rep(0, 2) 
  p_ystar[1] <- (e + dim(spam_features)[1])/(N + e + f)
  p_ystar[2] <- (f + dim(ham_features)[1])/(N + e + f)
  
  #starting a for loop for x_star
  for(i in 1:dim(new_data)[1]){
  x_star <- new_data[i,]
  
  #for spam emails
  summation_x_1 <- colSums(spam_features[,1:ncol(spam_features)])
  spam_numerator <- x_star + summation_x_1 + a
  spam_denominator_1 <- summation_x_1 + a
  spam_denominator_2 <- x_star + 1
  p_x_star_1 <- (lgamma(spam_numerator) 
                 + (summation_x_1)*log((N + b)/(N+b+1)) 
                 + x_star*log(1/(N+b+1))
                 - lgamma(spam_denominator_1)
                 - lgamma(spam_denominator_2))
  p_y_star_1 <- exp(sum(p_x_star_1))*p_ystar[1]
  
  #for ham emails
  summation_x_0 <- colSums(ham_features[,1:ncol(ham_features)])
  ham_numerator <- x_star + summation_x_0 + a
  ham_denominator_1 <- summation_x_0 + a
  ham_denominator_2 <- x_star + 1
  p_x_star_0 <- (lgamma(ham_numerator) 
                 + (summation_x_1)*log((N + b)/(N+b+1)) 
                 + x_star*log(1/(N+b+1))
                 - lgamma(ham_denominator_1)
                 - lgamma(ham_denominator_2))
  p_y_star_0 <- exp(sum(p_x_star_0))*p_ystar[2]
  
  #return information
  p_spam <- p_y_star_1/(p_y_star_1 + p_y_star_0)
  p_ham <- p_y_star_0/(p_y_star_1 + p_y_star_0)
  
  # pred_prob <- c(p_spam,p_ham)
  if(is.nan(p_spam) == TRUE | is.nan(p_ham) == TRUE){
    cat("email number",i,"is messed up","\n")
    next
  }
  spam_probs <- c(spam_probs,p_spam)
  ham_probs <- c(ham_probs,p_ham)
  if(p_spam >= p_ham){
    spam_counter <- c(spam_counter,1)
    ham_counter <- c(ham_counter,1000)
  }
  if(p_spam < p_ham){
    ham_counter <- c(ham_counter,1)
    spam_counter <- c(spam_counter,1000)
  }
  }
  return(cbind(spam_counter,ham_counter,spam_probs,ham_probs))
}
#
#setting up data
spam <- cbind(y_train,X_train)
colnames(spam)[1] <- "labels"

#predictions
predictions <- adv8_naiveBayes(new_data = X_test,
                               data = spam)

#deleting the messed up emails as informed by my function
y_test <- y_test[-c(164,228,264,326,350,383,403),]
length(y_test)
predictions <- cbind(y_test,predictions)
colnames(predictions) <- c("labels","spam","ham","spam_probs","ham_probs")
predictions <- data.frame(predictions)
str(predictions)
#####
#number of spam emails in the truth
dim(predictions[predictions$labels == 1,])[1]
#181 spam emails

#number of ham emails in the truth
dim(predictions[predictions$labels == 0,])[1]
#273 ham emails

#number of spam emails in the predictions
dim(predictions[predictions$spam == 1,])[1]
#205 predicted spam

#number of ham emails in the predictions
dim(predictions[predictions$ham == 1,])[1]
#249 predicted ham

#########################
#    Question 4 B   #
#########################
###################
#confusion matrix 
###################
#a11 = actual spam & predicted spam
dim(predictions[predictions$labels == 1 & predictions$spam == 1,])[1]
#a12 = actual spam & predicted ham
dim(predictions[predictions$labels == 1 & predictions$ham == 1,])[1]
#a21 = actual ham & predicted spam
dim(predictions[predictions$labels == 0 & predictions$spam == 1,])[1]
#a22 = actual ham & predicted ham
dim(predictions[predictions$labels == 0 & predictions$ham == 1,])[1]

#########################
#    Question 4 C    #
#########################

#calculating posterior values of lambda_0 and lambda_1 from training data
a <- 1
b <- 1
N <- 4140

#spam emails
X_train_1 <- spam[spam[,1] == 1,][-1]
dim(X_train_1)
sum_X_train_1 <- colSums(X_train_1[,1:ncol(X_train_1)])
lambda_1 <- (sum_X_train_1 + a)/(N + b)

#ham emails
X_train_0 <- spam[spam[,1] == 0,][-1]
dim(X_train_0)
sum_X_train_0 <- colSums(X_train_0[,1:ncol(X_train_0)])
lambda_0 <- (sum_X_train_0 + a)/(N + b)


#now picking three misclassified emails
predictions[predictions$labels == 1 & predictions$ham == 1,]
#email - spam classified as ham

predictions[predictions$labels == 0 & predictions$spam == 1,]
#email - ham classified as spam

#pulling features out
x_1 <- X_test[168,]
x_1 <- unlist(x_1)
x_2 <- X_test[155,]
x_2 <- unlist(x_2)
x_3 <- X_test[372,]
x_3 <- unlist(x_3)



#plotting ----
x_axis_labels <- read.csv("README.csv",
                          skip = 1)
colnames(x_axis_labels) <- "xlab"

par(col = "gray",
    mfcol = c(3,1),
    mar = c(4,4,4,2))
#email 1
plot(lambda_1,
     pch = 16,
     cex = 0.7,
     col = "red",
     xaxt = 'n',
     yaxt = 'n',
     xlab = NA,
     ylab = NA,
     main = "misclassified email 1 - spam classified as ham",
     cex.main = 0.9,
     ylim = c(0,36),
     bty = 'n')
axis(1, at=1:54, 
     labels=F,
     cex.axis = 0.7,
     las =2,
     col = "gray",
     tck = -0.008)
axis(2,at = seq(0,30, length.out = 4),
     col = "gray")
text(x = seq(1:54),
     y = -2.4,
     labels = x_axis_labels$xlab,
     srt=45,
     adj = 1,
     xpd = T,
     cex = 0.8,
     col = "black")
lines(lambda_1, 
      col = "pink")
points(lambda_1,
       col = "red",
       pch = 16,
       cex = 0.7)
lines(lambda_0,
      col = "lightblue")
points(lambda_0,
       col = "blue",
       pch = 16,
       cex = 0.7)
lines(x_1,
      col = "gray")
points(x_1,
       pch = 16,
       cex = 0.7,
       col = "black")
legend("topright",
       legend = c("data","lambda_1","lambda_0"),
       pch = 16,
       col = c("black","red","blue"),
       bty = 'n',
       cex = 0.7,
       text.col = "black",
       y.intersp = 0.3)

#email 2
plot(lambda_1,
     pch = 16,
     cex = 0.7,
     col = "red",
     xaxt = 'n',
     yaxt = 'n',
     xlab = NA,
     ylab = NA,
     main = "misclassified email 2 - spam classified as ham",
     cex.main = 0.9,
     ylim = c(0,20),
     bty = 'n')
axis(1, at=1:54, 
     labels=F,
     cex.axis = 0.7,
     las =2,
     col = "gray",
     tck = -0.008)
axis(2,at = seq(0,20, length.out = 5),
     col = "gray")
text(x = seq(1:54),
     y = -2.4,
     labels = x_axis_labels$xlab,
     srt=45,
     adj = 1,
     xpd = T,
     cex = 0.8,
     col = "black")
lines(lambda_1, 
      col = "pink")
points(lambda_1,
       col = "red",
       pch = 16,
       cex = 0.7)
lines(lambda_0,
      col = "lightblue")
points(lambda_0,
       col = "blue",
       pch = 16,
       cex = 0.7)
lines(x_2,
      col = "gray")
points(x_2,
       pch = 16,
       cex = 0.7,
       col = "black")
legend("topright",
       legend = c("data","lambda_1","lambda_0"),
       pch = 16,
       col = c("black","red","blue"),
       bty = 'n',
       cex = 0.7,
       text.col = "black",
       y.intersp = 0.3)
#email 3
plot(lambda_1,
     pch = 16,
     cex = 0.7,
     col = "red",
     xaxt = 'n',
     yaxt = 'n',
     xlab = NA,
     ylab = NA,
     main = "misclassified email 3 - ham classified as spam",
     cex.main = 0.9,
     ylim = c(0,17),
     bty = 'n')
axis(1, at=1:54,
     labels=F,
     cex.axis = 0.7,
     las =2,
     col = "gray",
     tck = -0.008)
axis(2,at = seq(0,15, length.out = 4),
     col = "gray")
text(x = seq(1:54),
     y = -2.4,
     labels = x_axis_labels$xlab,
     srt=45,
     adj = 1,
     xpd = T,
     cex = 0.8,
     col = "black",
     bty = 'n')
lines(lambda_1, 
      col = "pink")
points(lambda_1,
       col = "red",
       pch = 16,
       cex = 0.7)
lines(lambda_0,
      col = "lightblue")
points(lambda_0,
       col = "blue",
       pch = 16,
       cex = 0.7)
lines(x_3,
      col = "gray")
points(x_3,
       pch = 16,
       cex = 0.7,
       col = "black")
legend("topright",
       legend = c("data","lambda_1","lambda_0"),
       pch = 16,
       col = c("black","red","blue"),
       bty = 'n',
       cex = 0.7,
       text.col = "black",
       y.intersp = 0.3)

#########################
#    Question 4 D    #
#########################

##############
##ambiguous##
##############
predictions

#a11 = actual spam & predicted spam
predictions[predictions$labels == 1 & predictions$spam == 1,]
#a22 = actual ham & predicted ham
predictions[predictions$labels == 0 & predictions$ham == 1,]

#######
predictions$abs_1 <- abs(predictions$ham_probs - 0.5)

#writing a while loop to pick the ambiguous emails
loc_min <- rep(NA,3)
min_diff <- rep(NA,3)
pred_ambig <- matrix(NA, nrow = 3, ncol = 2)
x_ambig <- matrix(NA, nrow = 3, ncol = 54)

index <- 1
while(index <= 3){
  min_diff[index] <- min(predictions$abs_1)
  loc_min[index] <- which.min(predictions$abs_1)
  predictions$abs_1[loc_min[index]] <- max(predictions$abs_1)
   if((predictions$labels == 1 & predictions$spam == 1) || (predictions$labels == 0 & predictions$ham == 1)){
    x_ambig[index,] <- unlist(X_test[loc_min[index],])
    pred_ambig[index,1] <- predictions$spam_probs[loc_min[index]]
    pred_ambig[index,2] <- predictions$ham_probs[loc_min[index]]
    index <- index + 1
   }
}

###ambiguous email plotting ----

x_1 <- x_ambig[1,]
x_2 <- x_ambig[2,]
x_3 <- x_ambig[3,]

par(col = "gray",
    mfcol = c(3,1),
    mar = c(4,4,4,2))
#email 1
plot(lambda_1,
     pch = 16,
     cex = 0.7,
     col = "red",
     xaxt = 'n',
     yaxt = 'n',
     xlab = NA,
     ylab = NA,
     main = "ambiguous email 1",
     cex.main = 0.9,
     ylim = c(0,15),
     bty = 'n')
axis(1, at=1:54, 
     labels=F,
     cex.axis = 0.7,
     las =2,
     col = "gray",
     tck = -0.008)
axis(2,at = seq(0,15, length.out = 4),
     col = "gray")
text(x = seq(1:54),
     y = -2.4,
     labels = x_axis_labels$xlab,
     srt=45,
     adj = 1,
     xpd = T,
     cex = 0.8,
     col = "black")
lines(lambda_1, 
      col = "pink")
points(lambda_1,
       col = "red",
       pch = 16,
       cex = 0.7)
lines(lambda_0,
      col = "lightblue")
points(lambda_0,
       col = "blue",
       pch = 16,
       cex = 0.7)
lines(x_1,
      col = "gray")
points(x_1,
       pch = 16,
       cex = 0.7,
       col = "black")
legend("topright",
       legend = c("data","lambda_1","lambda_0"),
       pch = 16,
       col = c("black","red","blue"),
       bty = 'n',
       cex = 0.7,
       text.col = "black",
       y.intersp = 0.3)

#email 2
plot(lambda_1,
     pch = 16,
     cex = 0.7,
     col = "red",
     xaxt = 'n',
     yaxt = 'n',
     xlab = NA,
     ylab = NA,
     main = "ambiguous email 2",
     cex.main = 0.9,
     ylim = c(0,48),
     bty = 'n')
axis(1, at=1:54, 
     labels=F,
     cex.axis = 0.7,
     las =2,
     col = "gray",
     tck = -0.008)
axis(2,at = seq(0,40, length.out = 5),
     col = "gray")
text(x = seq(1:54),
     y = -2.4,
     labels = x_axis_labels$xlab,
     srt=45,
     adj = 1,
     xpd = T,
     cex = 0.8,
     col = "black")
lines(lambda_1, 
      col = "pink")
points(lambda_1,
       col = "red",
       pch = 16,
       cex = 0.7)
lines(lambda_0,
      col = "lightblue")
points(lambda_0,
       col = "blue",
       pch = 16,
       cex = 0.7)
lines(x_2,
      col = "gray")
points(x_2,
       pch = 16,
       cex = 0.7,
       col = "black")
legend("topright",
       legend = c("data","lambda_1","lambda_0"),
       pch = 16,
       col = c("black","red","blue"),
       bty = 'n',
       cex = 0.7,
       text.col = "black",
       y.intersp = 0.3)
#email 3
plot(lambda_1,
     pch = 16,
     cex = 0.7,
     col = "red",
     xaxt = 'n',
     yaxt = 'n',
     xlab = NA,
     ylab = NA,
     main = "ambiguous email 3",
     cex.main = 0.9,
     ylim = c(0,35),
     bty = 'n')
axis(1, at=1:54,
     labels=F,
     cex.axis = 0.7,
     las =2,
     col = "gray",
     tck = -0.008)
axis(2,at = seq(0,35, length.out = 3),
     col = "gray")
text(x = seq(1:54),
     y = -2.4,
     labels = x_axis_labels$xlab,
     srt=45,
     adj = 1,
     xpd = T,
     cex = 0.8,
     col = "black",
     bty = 'n')
lines(lambda_1, 
      col = "pink")
points(lambda_1,
       col = "red",
       pch = 16,
       cex = 0.7)
lines(lambda_0,
      col = "lightblue")
points(lambda_0,
       col = "blue",
       pch = 16,
       cex = 0.7)
lines(x_3,
      col = "gray")
points(x_3,
       pch = 16,
       cex = 0.7,
       col = "black")
legend("topright",
       legend = c("data","lambda_1","lambda_0"),
       pch = 16,
       col = c("black","red","blue"),
       bty = 'n',
       cex = 0.7,
       text.col = "black",
       y.intersp = 0.3)
