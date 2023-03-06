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

# Login to the agena.ai cloud servers and calculating the model

credentials <- login("test@example.com", "1234567890")

calculate(model, credentials, dataSet$id)

# Obtaining the results from agena.ai cloud server, accessing and displaying information

resIndex <- which(sapply(model$dataSets[[dsIndex]]$results, function(res) res$node %in% "total_cost"))
model$dataSets[[dsIndex]]$results[[resIndex]]$node
model$dataSets[[dsIndex]]$results[[resIndex]]$summaryStatistics$mean
