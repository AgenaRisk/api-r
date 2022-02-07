source("RAgena.R")

sw_project_cont_path = "Models/SWProjectRiskCont.cmpx"
sw_project_disc_path = "Models/SWProjectRiskDiscrete.cmpx"
car_costs_path = "Models/CarCosts.cmpx"
adv_rel_model_path = "Models/AdvancedReliabilityModelling.cmpx"

###dev testing
modelPath <- "Models/AdvancedReliabilityModelling.cmpx"
modelPath <- "Models/CarCosts.cmpx"
###

car_model <- from_cmpx(car_costs_path)
rel_model <- from_cmpx(adv_rel_model_path)
