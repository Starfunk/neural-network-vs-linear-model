# Neural Networks vs. Linear Models - Trigonometric Dataset

#1. SETTING UP THE NEURAL NETWORK
#2. VISUALIZE RESULTS
#3. CALCULATE CROSS VALIDATION VALUE


#1. ----[SETTING UP THE NEURAL NETWORK]-----------------------------------------

# Create input data. Save it in a dataframe.
Input <- runif(6000, 1, 10)
(trig.data <- as.data.frame(Input))

# Create output data.
Output <- trigFunc(Input)

# Combine input and output data into one dataframe.
trig.data <- cbind(trig.data, Output)
colnames(trig.data) <- c("Input","Output")


# Setting up training and testing datasets. The training data make up 75% of
# the dataset and are drawn randomly from the original dataset. The test data 
# is the remaining 25% of the data.
(index <- sample(1:nrow(trig.data),round(0.75*nrow(trig.data))))
train <- trig.data[index,]
test <- trig.data[-index,]

# Finding the max values for each column in the dataset.
maxs <- apply(trig.data, 2, max) 
mins <- apply(trig.data, 2, min)

# Scaled takes the original dataset and normalizes it based on the max and 
# min values. Normalized training and testing data is then created.
sq.scaled <- as.data.frame(scale(trig.data, center = mins, scale = maxs - mins))
trig.train <- sq.scaled[index,]
trig.test <- sq.scaled[-index,]

# Train the neural network.
# Note that during the learning process, the network may converge on a set of 
# weights that are not optimal (if the training process takes a few seconds,
# then this is the case). In such a case, rerun the script and continue doing so
# until the network converges on an optimal model (as seen in our paper). We
# use a custom hidden layer architecture for this 
net.sq <- neuralnet(Output~Input, trig.train, hidden=c(10,10,10,10,10,10,10), 
                    threshold=threshold.num, stepmax= 1000000)

# Compute the predicted values for the neural network.
pr.nn.sq <- compute(net.sq, trig.test[,1]) 

# Transform the normalized prediction data back to absolute values. 
pr.nn.sq_ <- pr.nn.sq$net.result * (max(trig.data$Output) - 
                                      min(trig.data$Output)) + min(trig.data$Output)

# Train the linear model on the training data and then test it on the testing
# data. Note that we don't use the scaled datasets here since it is not
# necessary to train the linear model on normalized values.
lm.fit <- glm(Output ~ Input, data=train)
pr.lm <- predict(lm.fit, test)
pr.lm_ <- as.vector(pr.lm)

# Calculate the root mean square for the predictions.
RMSE.lm <- rootMeanSqError(pr.lm_, as.vector(test$Output))
RMSE.nn <- rootMeanSqError(as.vector(pr.nn.sq_), as.vector(test$Output))

# Compute the proportional root mean square error which is normalized value
# with respect to the mean output value of the original dataset. These
# values allow us to compare to the performance of both the neural network and
# linear model ACROSS datasets.
prop.lm.trig <- propRMSE(pr.lm_, as.vector(test$Output), mean(trig.data$Output))
prop.nn.trig <- propRMSE(as.vector(pr.nn.sq_), as.vector(test$Output), mean(trig.data$Output))

# Computes how many times RMSE.nn goes into RMSE.lm and is a score that shows
# how well the neural network did compared to the linear mode.
score.trig <- (RMSE.lm/RMSE.nn)

#2. ----[VISUALIZE RESULTS]-----------------------------------------------------

# Plot the neural network (red) against the linear model (blue) on top of the 
# test data (black).
plot(trig.data$Input,trig.data$Output, pch=19, xlab="Value", 
     ylab="Trigonometric Value")
points(test$Input, pr.nn.sq_, col="blue", pch=19)
points(test$Input, pr.lm_, col="red", pch=19)

#3. ----[CALCULATE THE CROSS VALIDATED VALUES]----------------------------------

# Returns the average RMSE values for the linear model and for 
# the neural network by running k learning experiences. Note this takes a few 
# minutes to run.
CV <- kFoldValidationNum(trig.data, k.num)

# Print the average RMSE across k learning experiences.
print(paste("The average RMSE across", k.num, "loops for the neural network is", 
            CV[1], sep=" "))
print(paste("The average RMSE across", k.num, "loops for the linear model is", 
            CV[2], sep=" "))
