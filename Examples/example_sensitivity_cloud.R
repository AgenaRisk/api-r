#setwd("/Users/user/repos/api-r")

source("RAgena.R")

# Importing an existing model from a .cmpx file

model <- from_cmpx("Models/CarCosts.cmpx")
network <- model$networks[[1]]

# Creating a new dataSet in the model for sensitivity analysis

model$create_dataSet(id = "sa")

# Creating a sensitivity analysis config object 
# which uses all nodes for sensitivity analysis on the node total_cost, to calculate mean and variance results

sa_config <- create_sensitivity_config(
  target="total_cost",
  sensitivity_nodes="*",
  dataset="sa",
  report_settings = list(summaryStats = c("mean", "variance"))
)

# Login to the agena.ai cloud servers and running the sensitivity analysis

credentials <- login("test@example.com", "1234567890")

sensitivity_analysis(model, credentials, sa_config)