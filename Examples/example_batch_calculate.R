#setwd("/Users/user/repos/api-r")

source("RAgena.R")

# Importing an existing model from a .cmpx file

model <- from_cmpx("Models/CarCosts.cmpx")
network <- model$networks[[1]]

# Creating an empty csv file template with all the networks and nodes in the model
create_csv_template(model)

# The dataset csv is manually prepared and filled in outside the R environment
# In this example, it now includes five scenarios with different observations, and only the columns about the observed variables are kept

inputData <- "Car Costs_0 Model_DataSet_modified.csv"

# Creating batch cases, this function creates a new .json model file in the working directory with dataSets representing all the rows in the input data
create_batch_cases(model, inputData)

# It is possible to import the new model file back to R to work with it
model_with_cases <- from_cmpx("Car Costs_0 Model_Batch_Cases.json")

# Now model_with_cases is an R model object containing both the dataSets already existing in the model and a new dataSet for each row in the input data
# and it is ready to be used for calculation purposes, locally or on the cloud

