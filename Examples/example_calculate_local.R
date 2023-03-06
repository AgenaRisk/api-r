#setwd("/Users/user/repos/api-r")

source("RAgena.R")

# Importing an existing model from a .cmpx file

model <- from_cmpx("Models/CarCosts.cmpx")

# Defining the dataSet of interest ("mercedes"), and finding the relevant dataSet object in the model

dsId <- "mercedes"
dsIndex <- which(sapply(model$dataSets, function(sds) sds$id %in% dsId))
dataSet <- model$dataSets[[dsIndex]]

# Assigning certain nodes in the network so that it is easier to manipulate them (such as entering new observation)

network <- model$networks[[1]]

nodeCarType <- network$nodes[[which(sapply(network$nodes, function(item) item$id %in% "car_type"))]]
nodeMilesPerYear <- network$nodes[[which(sapply(network$nodes, function(item) item$id %in% "Miles_per_year"))]]
nodeReliability <- network$nodes[[which(sapply(network$nodes, function(item) item$id %in% "Reliability"))]]
nodeTotalAnnualCost <- network$nodes[[which(sapply(network$nodes, function(item) item$id %in% "total_cost"))]]

# Entering new observations to the selected nodes

model$enter_observation(dsId, node=nodeCarType$id, network=network$id, value="Small")
model$enter_observation(dsId, node=nodeMilesPerYear$id, network=network$id, value=1000)
model$enter_observation(dsId, node=nodeReliability$id, network=network$id, value="Low")

# Calculating the model using local agena.ai developer API

local_api_calculate(model, dsId, "mercedes.json")

# Importing the results to the model object, accessing and displaying information

model$import_results('mercedes.json')

dsIndex <- which(sapply(model$dataSets, function(sds) sds$id %in% dsId))
dataSet <- model$dataSets[[dsIndex]]

resIndex <- which(sapply(model$dataSets[[dsIndex]]$results, function(res) res$node %in% "total_cost"))
model$dataSets[[dsIndex]]$results[[resIndex]]$node
model$dataSets[[dsIndex]]$results[[resIndex]]$summaryStatistics$mean
