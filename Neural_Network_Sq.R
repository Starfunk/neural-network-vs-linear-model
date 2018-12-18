# Neural Networks vs. Linear Models - Squared Number Dataset

#1. SETTING UP THE NEURAL NETWORK
#2. VISUALIZE RESULTS
#3. CALCULATE CROSS VALIDATION VALUE

#1. ----[SETTING UP THE NEURAL NETWORK]-----------------------------------------

# Create input values
Input <- sample(1:100, 1000, replace=T)
sq.data <- as.data.frame(Input)

# Create output values by squaring input values.
Output <- (Input)^2

# Column bind the input and output values into 1 dataframe and rename columns
# Input and Output.
sq.data <- cbind(sq.data, Output)
colnames(sq.data) <- c("Input","Output")

# Setting up training and testing datasets. The training data make up 75% of
# the dataset and are drawn randomly from the original dataset. The test data 
# is the remaining 25% of the data.
(index <- sample(1:nrow(sq.data),round(0.75*nrow(sq.data))))
train <- sq.data[index,]
test <- sq.data[-index,]

# Finding the max values for each column in the dataset.
maxs <- apply(sq.data, 2, max) 
mins <- apply(sq.data, 2, min)

# Scaled takes the original dataset and normalizes it based on the max and 
# min values. Normalized training and testing data is then created.
sq.scaled <- as.data.frame(scale(sq.data, center = mins, scale = maxs - mins))
sq.train <- sq.scaled[index,]
sq.test <- sq.scaled[-index,]


# Training the neural network (NN). See Setup.R for a description of the 
# arguments used.
net.sq <- neuralnet(Output~Input, sq.train, hidden=c(8,3), threshold=threshold.num, 
                  stepmax= 1000000)

# Compute the predicted values for the neural network.
pr.nn.sq <- compute(net.sq, sq.test[,1]) #Run them through the neural network

pr.nn.sq_ <- pr.nn.sq$net.result * (max(sq.data$Output) - 
                          min(sq.data$Output)) + min(sq.data$Output)

# Train the linear model on the training data and then test it on the testing
# data. Note that we don't use the scaled datasets here since it is not
# necessary to train the linear model on normalized values.
lm.fit <- glm(Output ~ Input, data=train)
pr.lm <- predict(lm.fit, test)
pr.lm_ <- as.vector(pr.lm)


RMSE.lm <- rootMeanSqError(pr.lm_, as.vector(test$Output))
RMSE.nn <- rootMeanSqError(as.vector(pr.nn.sq_), as.vector(test$Output))

prop.lm.sq <- propRMSE(pr.lm_, as.vector(test$Output), mean(sq.data$Output))
prop.nn.sq <- propRMSE(as.vector(pr.nn.sq_), as.vector(test$Output),
                       mean(sq.data$Output))

score.sq <- (RMSE.lm/RMSE.nn)

#2. ----[VISUALIZE RESULTS]-----------------------------------------------------

# Plot the neural network (red) against the linear model (blue) on top of the 
# test data (black).
plot(sq.data$Input,sq.data$Output, pch=19, xlab="Value", ylab="Value Squared")
points(test$Input, pr.lm_, col="red", pch=19)
points(test$Input, pr.nn.sq_, col="blue", pch=19)

#3. ----[CALCULATE CROSS VALIDATION VALUE]------------------------------------------

# Returns the average RMSE values for the linear model and for 
# the neural network by running k learning experiences. Note this takes a few 
# minutes to run.
CV <- kFoldValidationNum(sq.data, k.num)

# Print the average RMSE across k learning experiences.
print(paste("The average RMSE across", k, "loops for the neural network is", 
            CV[1], sep=" "))
print(paste("The average RMSE across", k, "loops for the linear model is", 
            CV[2], sep=" "))

