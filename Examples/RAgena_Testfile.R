source("RAgena.R")

TIC <- Node$new(id="TIC")
Shock <- Node$new(id="Shock",type="Labelled",states=c("None","Compensated","Uncompensated"))
TissueInjury <- Node$new(id="TissueInjury",type="Ranked")
TIC$add_parent(Shock)
TIC$add_parent(TissueInjury)
HeartRate <- Node$new(id="HeartRate",simulated=TRUE)
HeartRate$add_parent(Shock)
HeartRate$set_distribution_type("Partitioned")
Lactate <- Node$new(id="Lactate",simulated=TRUE)
Shock$set_probabilities(list(0.5,0.2,0.3))
TIC$set_probabilities(list(c(0.9,0.1),c(0.8,0.2),c(0.7,0.3),c(0.8,0.2),
                          c(0.7,0.3),c(0.6,0.4),c(0.8,0.2),c(0.7,0.3),
                          c(0.5,0.5)), by_rows=FALSE)
HeartRate$set_expressions(c("Normal(90,10)","Normal(110,15)","Normal(120,30)"),"Shock")
Lactate$set_expressions("TNormal(4,1,-10,10)")
Energy <- Node$new(id="Energy",type="Ranked")
Energy$set_probabilities(list(0.5,0.3,0.2))
TissueInjury$add_parent(Energy)
Mechanism <- Node$new(id="Mechanism",type="Labelled")
Mechanism$states <- c("Penetrating", "Blunt")
TissueInjury$add_parent(Mechanism)
Network_TIC <- Network$new(id="TIC_Network",nodes=c(TIC,Shock,TissueInjury,HeartRate))
Network_TIC$add_node(Energy)
Network_TIC$add_node(Mechanism)
TIC_Model <- Model$new(networks = list(Network_TIC))

################################################################################################

Second_Network <- Network$new(id="Second_Network",nodes=c(TIC,Shock,TissueInjury,HeartRate))
Second_Network$add_node(Energy)
Second_Network$add_node(Mechanism)

TIC_Model$add_network(Second_Network)


TIC_Model$add_network_link(source_network = "TIC_Network",
                         source_node = "Mechanism",
                         target_network = "Second_Network",
                         target_node = "Mechanism",
                         link_type = "Marginals")


TIC_Model$remove_network_link(source_network = "TIC_Network",
                              source_node = "Mechanism",
                              target_network = "Second_Network",
                              target_node = "Mechanism")



TIC_Model$to_cmpx(filename = "two_tic_nets")
TIC_Model$to_json(filename = "two_tic_no_link")

TIC_Model$to_json(filename = "two_tic_r_link")

TIC_Model$add_network_link(source_network = "TIC_Network",
                         source_node = "TIC",
                         target_network = "Second_Network",
                         target_node = "Mechanism",
                         link_type = "Marginals")
TIC_Model$to_cmpx("testtic")

# TIC_Model$enter_observation(node="TIC",network="TIC_Network",value="Yes")
# TIC_Model$enter_observation(node="HeartRate",network="TIC_Network",value="100")
# TIC_Model$enter_observation(node="Energy",network="TIC_Network",value="High")
# TIC_Model$create_scenario("Scenario 2")
# TIC_Model$enter_observation(scenario="Scenario 2",node="HeartRate",network="TIC_Network",value="95")
# TIC_Model$enter_observation(scenario="Scenario 2",node="HeartRate",network="TIC_Network",value="100")
# TIC_Model$clear_scenario_observations("Scenario 1")
# 
# TIC_Model$networks[[1]]$plot()

# TIC_Model$to_json(filename = "TICTEST")
# custom_settings <- list(parameterLearningLogging = FALSE,
#                         discreteTails = FALSE,
#                         sampleSizeRanked = 5,
#                         convergence = 0.005,
#                         simulationLogging = FALSE,
#                         iterations = 200,
#                         tolerance = 1)
# TIC_Model$to_json(filename = "TICwithCUSTOMSETTINGS", settings = custom_settings)

# create_csv_template(TIC_Model)

TIC_Model$to_json()


# for (nd in TIC_Model$networks[[1]]$nodes){
#   if (length(nd$variables)>0) {cat("\nYes")} else {cat("\nNo")}
# }

TIC_Model$networks[[1]]$nodes[[4]]$set_variable(variable_name = "heartrate_const",
                                                variable_value = 90)

TIC_Model$networks[[1]]$nodes[[4]]$variables

TIC_Model$networks[[1]]$nodes[[4]]$remove_variable("heartrate_const")

# TIC_Model$networks[[1]]$nodes[[3]]$variables
# 
# TIC_Model$to_json(filename = "var_test")

TIC_Model$enter_observation(node = "Mechanism", network = "TIC_Network", value = "Blunt")
TIC_Model$enter_observation(node = "Energy", network = "TIC_Network", value = c("Low",6,"Medium",4),
                            soft_evidence = TRUE)
TIC_Model$enter_observation(node = "HeartRate", network = "TIC_Network", 
                            value = "heartrate_const", variable_input = TRUE)

testinput <- c("Low",6,"Medium",4)

TIC_Model$dataSets[[1]]$observations

TIC_Model$to_json(filename = "var_test2")

