# Neural Networks vs. Linear Models - California Housing Dataset

#1. SETTING UP THE NEURAL NETWORK
#2. VISUALIZE RESULTS
#3. CALCULATE CROSS VALIDATION VALUE

#1. ----[SETTING UP THE NEURAL NETWORK]-----------------------------------------

# Load in California housing market dataset
housing.data.cali <- read.csv(paste(p.data, "cali-housing.csv", sep=""))

# Ignore the last column because the elements are strings. Take a subset of the 
# dataset because training a neural network on 20k rows takes an extremely long
# time.
housing.data.cali <- housing.data.cali[1:10000,1:9]

# Check if there are any NA values in the columns.
apply(housing.data.cali,2,function(x) sum(is.na(x)))

# There are NA values in the 5th column, so we delete all the rows containing
# NA values in the 5th column.
housing.data.cali <- completeFun(housing.data.cali, c(5))

# Setting up training and testing datasets. The training data make up 75% of
# the dataset and are drawn randomly from the original dataset. The test data 
# is the remaining 25% of the data.
index <- sample(1:nrow(housing.data.cali), round(0.75*nrow(housing.data.cali)))
train <- housing.data.cali[index,]
test <- housing.data.cali[-index,]

# Finding the max values for each column in the dataset.
maxs <- apply(housing.data.cali, 2, max)
mins <- apply(housing.data.cali, 2, min)

# Scaled takes the original dataset and normalizes it based on the max and 
# min values. Normalized training and testing data is then created.
scaled <- as.data.frame(scale(housing.data.cali, center = mins, 
                              scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

# Set n equal to a vector containing the names for each of the columns in train_
n <- names(train_)

# # Creating the formula that stores how we want the neural network to fit the 
# data.
f <- as.formula(paste("median_house_value ~", 
                      paste(n[!n %in% "median_house_value"], collapse = " + ")))

# Train the NN based on a set of parameters defined in Setup.R.
nn <- neuralnet(f, data=train_, hidden=hidden.layers.cali, linear.output=T, 
                threshold=threshold.cali)

# Compute the NN's predicted values based on the testing data.
pr.nn <- compute(nn, test_[,1:(ncol(train_)-1)])

# Convert normalized predictions to actual value
pr.nn_ <- pr.nn$net.result * (max(housing.data.cali$median_house_value) -
                                min(housing.data.cali$median_house_value))
                                + min(housing.data.cali$median_house_value)

# Train the linear model on the training data and then test it on the testing
# data. Note that we don't use the scaled datasets here since it is not
# necessary to train the linear model on normalized values.
lm.fit <- glm(median_house_value~., data=train)
pr.lm <- predict(lm.fit, test)
pr.lm_ <- as.vector(pr.lm)

# Record the median house value in the test set in an easy to read variable.
t <- as.vector(test$median_house_value)

# Calculate the root mean square for the predictions.
RMSE.nn <- rootMeanSqError(as.vector(pr.nn_), t)
RMSE.lm <- rootMeanSqError(pr.lm_, t)

# Compute the proportional root mean square error which is normalized value
# with respect to the mean output value of the original dataset. These
# values allow us to compare to the performance of both the neural network and
# linear model ACROSS datasets.
prop.nn.cali <- propRMSE(as.vector(pr.nn_), t, 
                         mean(housing.data.cali$median_house_value))
prop.lm.cali <- propRMSE(pr.lm_, t, mean(housing.data.cali$median_house_value))

# Computes how many times RMSE.nn goes into RMSE.lm and is a score that shows
# how well the neural network did compared to the linear mode.
score.cali <- (RMSE.lm/RMSE.nn)

#2. ----[VISUALIZE RESULTS]-----------------------------------------------------

# Plot both figures in the same frame.
par(mfrow=c(2,1))

# We plot using median income as the explanatory variable because both
# values are correlated (and more so than other variables in the dataset).
plot(test$median_income, test$median_house_value, pch=19, xlab="Median Income",
     ylab="Median House Value", main="Linear Model")
points(test$median_income, pr.lm_, col="red", pch=19)

plot(test$median_income, test$median_house_value, pch=19, xlab="Median Income",
     ylab="Median House Value", main="Neural Network")
points(test$median_income, as.vector(pr.nn_), col="blue", pch=19)

#3. ----[CALCULATE THE CROSS VALIDATED VALUES]----------------------------------

# Run the cross validation algorithm to get the average RMSE values for the 
# neural network and the linear model. Note that the training process for the 
# neural network may converge on suboptimal weights, resulting in suboptimal 
# predictions. We are only interested when the neural network converges on 
# optimal weights, thus cross validation is not a great way to benchmark the 
# neural network's performance. 

# Returns the average RMSE values for the linear model and for 
# the neural network by running k learning experiences. Note this takes a few 
# minutes to run.
CV <- kFoldValidationCali(housing.data, k.cali)

# Print the average RMSE across k learning experiences.
print(paste("The average RMSE across", k.cali, "loops for the neural network is",
            CV[1], sep=" "))

print(paste("The average RMSE across", k.cali, "loops for the linear model is",
            CV[2], sep=" "))
