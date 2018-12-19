# Neural Networks vs. Linear Models - Setup
# 
# Set up dependencies, workspace, and relevant functions.

#1. INSTALL DEPENDENCIES
#2. ORGANIZING WORKSPACE
#3. SPECIFY PARAMETERS

#1. ----[INSTALL DEPENDENCIES]--------------------------------------------------

install.packages(neuralnet)
library(neuralnet)

#2. -----[ORGANIZING WORKSPACE]-------------------------------------------------

# NOTE: MAKE SURE THAT YOU'VE SAVED THIS REPOSITORY TO YOUR WORKING DIRECTORY.
# NOTE: MAKE SURE THAT YOUR WORKING DIRECTORY IS IN FINAL PROJECT.

# Creating new folder names in working directory to store data.
folder.data <- c("Data") # names of folders to be created
# Creating folders if they do not already exist.
if(file.exists(folder.data) == FALSE) 
  dir.create(folder.data)

# Storing data path to data file
p.data <- paste(getwd(), "/Data/", sep = "")

#3. ----[CUSTOM FUNCTIONS]------------------------------------------------------

# Takes a vector as input and makes every element that is less than 0
# equal to 0.
noZero <- function(vec) {
  ifelse(vec < 0, 0, vec)
}

# Takes a vector as input and multiplies every element that is less than 0
# by -1.
noNegative <- function(vec) {
  ifelse(vec < 0, -vec, vec)
}

# Converts age to a value to be used when calculating housing value. We expect
# the most expensive neighborhoods will have an average age between 40-60 and
# so ageValue is at a maximum between ages 40-60.
ageValue <- function(age) {
  age.value <- -(0.1 * age - 5)^2 + 25
  return(age.value)
}

# Calculates the housing values for the simulated housing dataset in
# Neural_Network_Sim. Note that the function is currently a linear combination
# of 6 variables: age, criminal, prestige, amenities, smell, and block.
calcHousingValues <-function(age, criminal, prestige, amenities, smell, block) {
  value <- w.criminal * -criminal + w.prestige * prestige + 
    w.amenities * amenities + w.age * ageValue(age) + w.smell * smell +
    w.block * block 
  return(value)
}

# Calculates the root mean square value between a vector of predicted values
# and a vector of the actual values.
rootMeanSqError <- function(pred, vals) {
  rmse <- sqrt(sum((pred - vals)^2)/length(pred))
  return(rmse)
}

# Calculates the proportional root mean square value between a vector of predicted values
# and a vector of the actual values by calling on the rootMeanSqError function
# and then dividing the result by the mean of all the values that are to be 
# predicted. This allows for a comparison of performance across datasets which
# contain differing magnitudes of predicted values.
propRMSE <- function(pred, vals, mean.value) {
  return(rootMeanSqError(pred, vals)/abs(mean.value))
}

# The trigonometric function that is used in Neural_Network_Trig to transform
# the input values to the trigonometric output values.
trigFunc <- function(x) {
  trig.val <- sin(x^2+2)*cos(x-1)
  return(trig.val)
}

# Calculate the k-fold cross validation values for the squared and trig
# scripts. The code here is not heavily commented because this same code can be
# found in both the squared and trig scripts.
kFoldValidationNum <- function(sq.data, k) {
  RMSE.nn <- c()
  RMSE.lm <- c()
  for(i in 1:k) {
    (index <- sample(1:nrow(sq.data),round(0.75*nrow(sq.data))))
    train <- sq.data[index,]
    test <- sq.data[-index,]
    maxs <- apply(sq.data, 2, max) 
    mins <- apply(sq.data, 2, min)
    sq.scaled <- as.data.frame(scale(sq.data, center = mins,
                                     scale = maxs - mins))
    sq.train <- sq.scaled[index,]
    sq.test <- sq.scaled[-index,]
    nn.sq <- neuralnet(Output~Input, sq.train, hidden=c(8,3), 
                       threshold=threshold.num, stepmax= 1000000)
    pr.nn.sq <- compute(nn.sq, sq.test[,1]) 
    pr.nn.sq_ <- pr.nn.sq$net.result * (max(sq.data$Output) - 
                                  min(sq.data$Output)) + min(sq.data$Output)
    lm.fit <- glm(Output ~ Input, data=train)
    pr.lm <- predict(lm.fit, test)
    pr.lm_ <- as.vector(pr.lm)
    RMSE.nn <- c(RMSE.nn, rootMeanSqError(pr.lm_, as.vector(test$Output)))
    RMSE.lm <- c(RMSE.lm, rootMeanSqError(as.vector(pr.nn.sq_), 
                                          as.vector(test$Output)))
  }
  kMSE <- c(mean(RMSE.nn), mean(RMSE.lm))
  return(kMSE)
}

# Calculate the k-fold cross validation values for the sim
# script. The code here is not heavily commented because this same code can be
# found in the sim script.
kFoldValidationSim <- function(data, outliers, k) {
  RMSE.nn <- c()
  RMSE.lm <- c()
  for(i in 1:k) {
    (index <- sample(1:nrow(data),round(0.75*nrow(data))))
    train <- rbind(data[index,], outliers)
    test <- data[-index,]
    maxs <- apply(data, 2, max) 
    mins <- apply(data, 2, min)
    scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
    train_ <- scaled[index,]
    test_ <- scaled[-index,]
    n <- names(train_)
    f <- as.formula(paste("value ~", paste(n[!n %in% "value"], 
                                           collapse = " + ")))
    nn <- neuralnet(f, data=train_, hidden=hidden.layers.sim, 
                    threshold=threshold.sim, linear.output=T)
    pr.nn <- compute(nn, test_[,1:(ncol(data)-1)])
    pr.nn_ <- as.vector(pr.nn$net.result * (max(housing.data$value) -
                             min(housing.data$value)) + min(housing.data$value))
    lm.fit <- glm(value~., data=train)
    pr.lm <- predict(lm.fit, test)
    pr.lm_ <- as.vector(pr.lm)
    t <- as.vector(test$value)
    RMSE.nn <- c(RMSE.nn,  meanSqError(pr.nn_, t))
    RMSE.lm <- c(RMSE.lm, meanSqError(pr.lm_, t))
  }
  kMSE <- c(mean(RMSE.nn), mean(RMSE.lm))
  return(kMSE)
}

# Calculate the k-fold cross validation values for the cali
# script. The code here is not heavily commented because this same code can be
# found in the cali script.
kFoldValidationCali <- function(data, outliers, k) {
  RMSE.nn <- c()
  RMSE.lm <- c()
  for(i in 1:k) {
    (index <- sample(1:nrow(data),round(0.75*nrow(data))))
    train <- rbind(data[index,], outliers)
    test <- data[-index,]
    maxs <- apply(data, 2, max) 
    mins <- apply(data, 2, min)
    scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
    train_ <- scaled[index,]
    test_ <- scaled[-index,]
    n <- names(train_)
    f <- as.formula(paste("value ~", paste(n[!n %in% "value"], 
                                           collapse = " + ")))
    nn <- neuralnet(Output~Input, sq.train, hidden=hidden.layers.cali, 
                    threshold=threshold.cali, stepmax= 1000000)
    pr.nn <- compute(nn, test_[,1:(ncol(data)-1)])
    pr.nn_ <- pr.nn$net.result * (max(housing.data$value) -
                            min(housing.data$value)) + min(housing.data$value)
    lm.fit <- glm(value~., data=train)
    pr.lm <- predict(lm.fit, test)
    pr.lm_ <- as.vector(pr.lm)
    RMSE.nn <- c(RMSE.nn,  meanSqError(as.vector(pr.nn_), 
                                       as.vector(test$value)))
    RMSE.lm <- c(RMSE.lm, meanSqError(pr.lm_, as.vector(test$value)))
  }
  kMSE <- c(mean(RMSE.nn),mean(RMSE.lm))
  return(kMSE)
}

# Deletes all rows in data containing NA values in desiredCols.
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#4. ----[SPECIFY PARAMETERS]----------------------------------------------------

# Specifying the weight of the following variables (the higher the weight, the
# more the housing value will be biased towards that variable).

#----[Neural_Network_Sim]-------------------------------------------------------
# Weight for age.
w.age <- 1

# Weight for criminality.
w.criminal <- 5

# Weight for prestige.
w.prestige <- 20

# Weight for amentities.
w.amenities <- 10

# Weight for smell.
w.smell <- 0

# Weight for block. 
w.block <- 0

# Number of data points for each variable to generate.
length.data <- 1000

# Specify how many hidden layers in the neural network and how many nodes per
# layer of the simulated housing data neural network.
hidden.layers.sim <- c(10, 10, 5, 5, 3)

# A numeric value specifying the threshold for the partial derivatives of the 
# error function as stopping criteria. The higher this value, the faster the
# learning experience will be.
threshold.sim <- 0.2

# The threshold under which we consider housing values to be outliers.
outlier.threshold <- 200

# Adds 2 * (outlier.loop) outlier data to housing.data. This is done so that the
# neural networks has more outlier data to train on.
outlier.loop <- 1

# Specifies the number of times to run the learning cycle for the neural network
k.sim <- 10

#----[Neural_Network_Sq & Neural_Network_Trig]----------------------------------

# Specify the number of hidden layers and the number of nodes in each layer of
# the numbers (squared and trig) neural network.
hidden.layers.num <- c(8,3)

# A numeric value specifying the threshold for the partial derivatives of the 
# error function as stopping criteria. The higher this value, the faster the
# learning experience will be.
threshold.num <- 0.2

# Specifies the number of times to run the learning cycle for the neural network
k.num <- 10

#----[Neural_Network_Cali]------------------------------------------------------

# Specify the number of hidden layers and the number of nodes in each layer of
# the california neural network.
hidden.layers.cali <- c(8,3)

# A numeric value specifying the threshold for the partial derivatives of the 
# error function as stopping criteria. The higher this value, the faster the
# learning experience will be.
threshold.cali <- 0.2

# Specifies the number of times to run the learning cycle for the neural network
k.cali <- 10
