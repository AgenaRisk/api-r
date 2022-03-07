source("RAgena.R")

###dev testing
modelPath <- "Models/AdvancedReliabilityModelling.cmpx"
modelPath <- "Models/CarCosts.cmpx"
###


car_costs_path = "Models/CarCosts.cmpx"
car_model <- from_cmpx(car_costs_path)

adv_rel_model_path = "Models/AdvancedReliabilityModelling.cmpx"
rel_model <- from_cmpx(adv_rel_model_path)



#test creating new Nodes and Network

TIC <- Node$new(id="TIC")
Shock <- Node$new(id="Shock",type="Labelled",states=c("None","Compensated","Uncompensated"))
Injury <- Node$new(id="Injury",type="Ranked")
TIC$addParent(Shock)
TIC$addParent(Injury)
#TIC$setDistributionType("Manual")
HeartRate <- Node$new(id="HeartRate",simulated=TRUE)
HeartRate$addParent(Shock)
HeartRate$setDistributionType("Partitioned")
Lactate <- Node$new(id="Lactate",simulated=TRUE)
#Lactate$setDistributionType("Partitioned")
#TIC$addParent(Shock)
TIC$probabilities
Shock$probabilities
Injury$probabilities
HeartRate$distr_type
Shock$setProbabilities(list(0.5,0.2,0.3))
Shock$probabilities
TIC$setProbabilities(list(c(0.9,0.8,0.7,0.8,0.7,0.6,0.8,0.7,0.5),c(0.1,0.2,0.3,0.2,0.3,0.4,0.2,0.3,0.5)))
TIC$probabilities
HeartRate$distr_type
HeartRate$setExpressions(c("Normal(90,10)","Normal(110,15)","Normal(120,30)"),"Shock")
HeartRate$expressions
Lactate$setExpressions("TNormal(10,1,0,2)")
Lactate$expressions

Network_TIC <- Network$new(id="TIC_Network",nodes=c(TIC,Shock,Injury,HeartRate))
Network_TIC$id
Network_TIC$nodes


