source("RAgena.R")

model <- from_cmpx("Models/CarCosts.cmpx")

dsId <- "mercedes"
dsIndex <- which(sapply(model$dataSets, function(sds) sds$id %in% dsId))
dataSet <- model$dataSets[[dsIndex]]

network <- model$networks[[1]]

nodeCarType <- network$nodes[[which(sapply(network$nodes, function(item) item$id %in% "car_type"))]]
nodeMilesPerYear <- network$nodes[[which(sapply(network$nodes, function(item) item$id %in% "Miles_per_year"))]]
nodeReliability <- network$nodes[[which(sapply(network$nodes, function(item) item$id %in% "Reliability"))]]
nodeTotalAnnualCost <- network$nodes[[which(sapply(network$nodes, function(item) item$id %in% "total_cost"))]]

model$enter_observation(dsId, node=nodeCarType$id, network=network$id, value="Small")
model$enter_observation(dsId, node=nodeMilesPerYear$id, network=network$id, value=1000)
model$enter_observation(dsId, node=nodeReliability$id, network=network$id, value="Low")

local_api_calculate(model, dsId, "mercedes.json")

model$import_results('mercedes.json')

dsIndex <- which(sapply(model$dataSets, function(sds) sds$id %in% dsId))
dataSet <- model$dataSets[[dsIndex]]

resIndex <- which(sapply(model$dataSets[[dsIndex]]$results, function(res) res$node %in% "total_cost"))
model$dataSets[[dsIndex]]$results[[resIndex]]$node
model$dataSets[[dsIndex]]$results[[resIndex]]$summaryStatistics$mean
