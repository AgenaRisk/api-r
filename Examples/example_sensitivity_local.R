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

# Running the sensitivity analysis using local agena.ai developer API

local_api_sensitivity(model, sa_config, 'local_sensitivity.json')
