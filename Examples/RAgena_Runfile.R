source("RAgena.R")

######
###dev testing
# modelPath <- "Models/AdvancedReliabilityModelling.cmpx"
# modelPath <- "Models/CarCosts.cmpx"
# modelPath <- "Models/Asia.cmpx"
# 
inputModel <- car_model
# inputModel <- TIC_Model
# inputModel <- rel_model
#
inputModel <- TIC_Model
inputData <- "TICdataset.csv"
###
######

###### 1. SIMPLE EXAMPLES OF IMPORTING/EXPORTING CMPX FILES IN R

# importing Car Costs model from cmpx file

car_costs_path = "Models/CarCosts.cmpx"
car_model <- from_cmpx(car_costs_path)

inputNetwork <- car_model$networks[[1]]
plot_network(inputNetwork)

# exporting Car Costs model to cmpx and json files

car_model$to_cmpx()
car_model$to_json()

length(car_model$networks[[1]]$nodes[[3]]$variables)


car_model$networks[[1]]$nodes[[3]]$variables[[1]][[1]]
car_model$networks[[1]]$nodes[[4]]$variables

car_model$enter_observation(node = )



# importing Adv Reliability model from cmpx file
  
adv_rel_model_path = "Models/AdvancedReliabilityModelling.cmpx"
rel_model <- from_cmpx(adv_rel_model_path)

rel_model$networks[[1]]$plot()




# importing Asia model from cmpx file

asia_model <- from_cmpx("Models/Asia.cmpx")



###### 2. SIMPLE USE EXAMPLE OF CREATING A MODEL IN R ENVIRONMENT

# 2.1 creating new Nodes
# their types, states, parents, probs, and expressions are set here

TIC <- Node$new(id="TIC")
Shock <- Node$new(id="Shock",type="Labelled",states=c("None","Compensated","Uncompensated"))
TissueInjury <- Node$new(id="TissueInjury",type="Ranked")

TIC$addParent(Shock)
TIC$addParent(TissueInjury)

HeartRate <- Node$new(id="HeartRate",simulated=TRUE)
HeartRate$addParent(Shock)
HeartRate$setDistributionType("Partitioned")

Lactate <- Node$new(id="Lactate",simulated=TRUE)

Shock$setProbabilities(list(0.5,0.2,0.3))

# setProbabilities alternative one
# TIC$setProbabilities(list(c(0.9,0.8,0.7,0.8,0.7,0.6,0.8,0.7,0.5),
#                          c(0.1,0.2,0.3,0.2,0.3,0.4,0.2,0.3,0.5)))

# setProbabilities alternative two
TIC$setProbabilities(list(c(0.9,0.1),c(0.8,0.2),c(0.7,0.3),c(0.8,0.2),
                          c(0.7,0.3),c(0.6,0.4),c(0.8,0.2),c(0.7,0.3),
                          c(0.5,0.5)), by_rows=FALSE)

HeartRate$setExpressions(c("Normal(90,10)","Normal(110,15)","Normal(120,30)"),"Shock")
Lactate$setExpressions("TNormal(4,1,-10,10)")

Energy <- Node$new(id="Energy",type="Ranked")
Energy$setProbabilities(list(0.5,0.3,0.2))

TissueInjury$addParent(Energy)

Mechanism <- Node$new(id="Mechanism",type="Labelled")
Mechanism$states <- c("Penetrating", "Blunt")
TissueInjury$addParent(Mechanism)

######
# TissueInjury$removeParent("Mechanism")
# TissueInjury$parents
# TissueInjury$probabilities

# TempParent <- Node$new(id="TempParent")
# HeartRate$expressions
# HeartRate$addParent(TempParent)
# HeartRate$setExpressions(c("Normal(90,10)","Normal(90,10)","Normal(110,15)","Normal(110,15)","Normal(120,30)","Normal(120,30)"),c("Shock","TempParent"))
# HeartRate$partitions
# HeartRate$expressions
# HeartRate$removeParent("TempParent")
# HeartRate$partitions
# HeartRate$expressions
######

# 2.2 creating a new Network

Network_TIC <- Network$new(id="TIC_Network",nodes=c(TIC,Shock,TissueInjury,HeartRate))

# Network_TIC$nodes
# Network_TIC$getNodes()

Network_TIC$addNode(Energy)
Network_TIC$addNode(Mechanism)
Network_TIC$addNode(TIC)
Network_TIC$removeNode(TissueInjury)
Network_TIC$addNode(TissueInjury)

# 2.3 creating a new Model
# and exporting to cmpx and json

inputNetwork <- Network_TIC

TIC_Model <- Model$new(networks = list(Network_TIC))

# TIC_Model$to_cmpx()
# TIC_Model$to_json("custom-name-tic")
# TIC_Model$to_json()


# 2.4 observation, dataset, and scenario operations

# TIC_Model$dataSets[[1]]$observations

# TIC_Model$dataSets[[1]]$observations
# 
# TIC_Model$networks
# TIC_Model$networks[[1]]$nodes
TIC_Model$enter_observation(node="TIC",network="TIC_Network",value="Yes")
TIC_Model$enter_observation(node="HeartRate",network="TIC_Network",value="100")
TIC_Model$enter_observation(node="Energy",network="TIC_Network",value="High")
TIC_Model$enter_observation(node="Energy",network="TIC_Network",value="Low")
TIC_Model$remove_observation(node="Energy",network="TIC_Network")
TIC_Model$remove_observation(node="TIC",network="TIC_Network")
TIC_Model$clear_all_observations()
# TIC_Model$clear_all_observations()

TIC_Model$create_scenario("Scenario 2")
TIC_Model$enter_observation(node="HeartRate",network="TIC_Network",value="95")
TIC_Model$enter_observation(scenario="Scenario 2",node="HeartRate",network="TIC_Network",value="95")
TIC_Model$enter_observation(scenario="Scenario 2",node="HeartRate",network="TIC_Network",value="100")
# TIC_Model$enter_observation(scenario="Scenario 2",node="Energy",network="TIC_Network",value="Low")
# TIC_Model$enter_observation(scenario="Scenario 1",node="Energy",network="TIC_Network",value="Low")
# TIC_Model$enter_observation(node="Energy",network="TIC_Network",value="High")

TIC_Model$clear_scenario_observations("Scenario 1")

# 2.5 creating batch case json files using a CSV input
# correct CSV format: each column header is titled Node_id.Network_id

# TIC_Model$dataSets
create_batch_cases(TIC_Model,"TICdataset.csv")

Testnode <- Node$new(id="Testnode",type="Ranked")
Testnode$states
Testnode$probabilities
Testnode$states <- c("One", "Two")

Testnode2 <- Node$new(id="Testnodetwo", type="Ranked", states=c("One","Two"))
Testnode2$states
Testnode2$probabilities
Testnode2$distr_type
Testnode2$expressions
Testnode2$setDistributionType("Expression")

Testnode3 <- Node$new(id="Testnodethree",simulated=TRUE)

Testnode$probabilities
Testnode$setProbabilities(list(0.2,0.5,0.3))


node_one <- Node$new(id = "node_one")
node_two <- Node$new(id = "node_two", name = "Second Node")
node_three <- Node$new(id = "node_three", type = "Ranked")
node_four <- Node$new(id = "node_four", type = "Ranked", states = c("Very low", "Low", "Medium", "High", "Very high"))

node_three$parents
node_three$getParents()
node_three$probabilities
node_three$addParent(node_one)
node_three$addParent(node_two)

node_one$states <- c("Negative","Positive")
node_one$states

node_three$setProbabilities(list(0.7, 0.2, 0.1))
node_three$setProbabilities(list(c(0.1, 0.4, 0.5), c(0.2, 0.45, 0.35), c(0.3, 0.6, 0.1), c(0.4, 0.55, 0.05)), by_rows = FALSE)

node_three$setProbabilities(list(c(0.1, 0.2, 0.3, 0.4), c(0.4, 0.45, 0.6, 0.55), c(0.5, 0.35, 0.1, 0.05)), by_rows = TRUE)
