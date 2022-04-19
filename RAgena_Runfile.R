source("RAgena.R")

###dev testing
modelPath <- "Models/AdvancedReliabilityModelling.cmpx"
modelPath <- "Models/CarCosts.cmpx"
###


car_costs_path = "Models/CarCosts.cmpx"
car_model <- from_cmpx(car_costs_path)

adv_rel_model_path = "Models/AdvancedReliabilityModelling.cmpx"
rel_model <- from_cmpx(adv_rel_model_path)

help(sink)

#test creating new Nodes and Network

TIC <- Node$new(id="TIC")
Shock <- Node$new(id="Shock",type="Labelled",states=c("None","Compensated","Uncompensated"))
Injury <- Node$new(id="Injury",type="Ranked")
TIC$addParent(Shock)
TIC$addParent(Injury)
#TIC$setDistributionType("Manual")
Shock$probabilities
HeartRate <- Node$new(id="HeartRate",simulated=TRUE)
HeartRate$addParent(Shock)
HeartRate$setDistributionType("Partitioned")
Lactate <- Node$new(id="Lactate",simulated=TRUE)
#Lactate$setDistributionType("Partitioned")
#TIC$addParent(Shock)
#TIC$probabilities
#Shock$probabilities
#Injury$probabilities
#HeartRate$distr_type
Shock$setProbabilities(list(0.5,0.2,0.3))
Shock$probabilities
TIC$setProbabilities(list(c(0.9,0.8,0.7,0.8,0.7,0.6,0.8,0.7,0.5),c(0.1,0.2,0.3,0.2,0.3,0.4,0.2,0.3,0.5)))
TIC$setProbabilities(list(c(0.9,0.1),c(0.8,0.2),c(0.7,0.3),c(0.8,0.2),c(0.7,0.3),c(0.6,0.4),c(0.8,0.2),c(0.7,0.3),c(0.5,0.5)), by_rows=FALSE)
TIC$probabilities

rows_list <- list(c(0.9,0.8,0.7,0.8,0.7,0.6,0.8,0.7,0.5),c(0.1,0.2,0.3,0.2,0.3,0.4,0.2,0.3,0.5))
columns_list <- list(c(0.9,0.1),c(0.8,0.2),c(0.7,0.3),c(0.8,0.2),c(0.7,0.3),c(0.6,0.4),c(0.8,0.2),c(0.7,0.3),c(0.5,0.5))
length(rows_list)
length(columns_list)

TIC$probabilities
#HeartRate$distr_type
HeartRate$setExpressions(c("Normal(90,10)","Normal(110,15)","Normal(120,30)"),"Shock")
#HeartRate$expressions
Lactate$setExpressions("TNormal(4,1,-10,10)")
#Lactate$expressions

Energy <- Node$new(id="Energy",type="Ranked")
Energy$setProbabilities(list(0.5,0.3,0.2))

Injury$probabilities
Injury$addParent(Energy)
Injury$probabilities

Mechanism <- Node$new(id="Mechanism",type="Labelled")
Mechanism$states <- c("Penetrating", "Blunt")
Injury$addParent(Mechanism)
Injury$probabilities

Injury$removeParent("Mechanism")
Injury$parents
Injury$probabilities

TempParent <- Node$new(id="TempParent")
HeartRate$expressions
HeartRate$addParent(TempParent)
HeartRate$setExpressions(c("Normal(90,10)","Normal(90,10)","Normal(110,15)","Normal(110,15)","Normal(120,30)","Normal(120,30)"),c("Shock","TempParent"))
HeartRate$partitions
HeartRate$expressions
HeartRate$removeParent("TempParent")
HeartRate$partitions
HeartRate$expressions


Network_TIC <- Network$new(id="TIC_Network",nodes=c(TIC,Shock,Injury,HeartRate))
Network_TIC$id
Network_TIC$nodes
