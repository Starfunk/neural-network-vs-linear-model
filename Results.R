# Neural Networks vs. Linear Models - Results

# This script displays the performance of the neural network and the linear 
# model across datasets.

# Create a dataframe that stores the LM vs. NN scores. I.e. How many times
# does the neural network error go into the linear model error?
(scores <- data.frame(
  squared = score.sq,
  trig = score.trig,
  sim = score.sim,
  cali = score.cali
))

View(scores)

# Create a dataframe that stores the proportional root mean square error for 
# both the neural network values and linear model values.
(proportions <- data.frame(
  squared = c(prop.nn.sq, prop.lm.sq),
  trig = c(prop.nn.trig, prop.lm.trig),
  sim = c(prop.nn.sim, prop.lm.sim),
  cali = c(prop.nn.cali, prop.lm.cali)
))
rownames(proportions) <- c("Neural Network", "Linear Model")

View(proportions)
