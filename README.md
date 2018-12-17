# When should you use a neural network over a linear model?

## In this repository you will find four R scripts:

**Functions.R** <br />
This script contains contains custom functions that were written specifically for this project.

**Data_Generation.R** <br />
This script generates housing market data based on a variety of parameters that can be modified. 

**Neural_Network_4.R** <br />
This script runs a neural network on 4 variables, each variable has a high impact on the housing market.

**Neural_Network_6.R** <br />
This script is essentially the same as Neural_Network_4.R except that two new variables have been introduced: "smell" (how each neighborhood smells) and "block" (the average length of the blocks in the neighborhood). These two new variables have little to no impact on the housing market data. The idea is to see if the neural network can recognize that fact.


## How to use this repository

1. Run Functions.R. This script will download and load the neuralnet library and set up the relevant paths.

2. If you would like to generate your own housing data, run Data_Generation.R

3. If you would like to run the 4 variable model,  
