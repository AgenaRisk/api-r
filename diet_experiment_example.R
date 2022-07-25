source("RAgena.R")

#First we create the "mean" and "variance" nodes

mean <- Node$new(id = "mean", simulated = TRUE)
mean$setExpressions("Normal(0.0,100000.0)")

variance <- Node$new(id = "variance", simulated = TRUE)
variance$setExpressions("Uniform(0.0,50.0)")

#Now we create the "common variance" and its "tau" parameter nodes

tau <- Node$new(id = "tau", simulated = TRUE)
tau$setExpressions("Gamma(0.001,1000.0)")

common_var <- Node$new(id = "common_var", name = "common variance", simulated = TRUE)
common_var$addParent(tau)
common_var$setExpressions("Arithmetic(1.0/tau)")

#Creating a list of four mean nodes, "mean A", "mean B", "mean C", and "mean D"

mean_names <- c("A", "B", "C", "D")
means_list <- vector(mode = "list", length = length(mean_names))

for (i in seq_along(mean_names)) {
  node_id <- paste0("mean",mean_names[i])
  node_name <- paste("mean",mean_names[[i]])
  means_list[[i]] <- Node$new(id = node_id, name = node_name, simulated = TRUE)
  means_list[[i]]$addParent(mean)
  means_list[[i]]$addParent(variance)
  means_list[[i]]$setExpressions("Normal(mean,variance)")
}

# Defining the list of observations for the experiment nodes
# and creating the experiment nodes y11, y12, ..., y47, y48

observations <- list(c(62, 60, 63, 59),
                     c(63, 67, 71, 64, 65, 66),
                     c(68, 66, 71, 67, 68, 68),
                     c(56, 62, 60, 61, 63, 64, 63, 59))

obs_nodes_list <- vector(mode = "list", length = length(mean_names))
for (i in seq_along(obs_nodes_list)) {
  obs_nodes_list[[i]] <- vector(mode = "list", length = length(observations[[i]]))
  this_mean_id <- means_list[[i]]$id
  
  for (j in seq_along(obs_nodes_list[[i]])) {
    node_id <- paste0("y",i,j)
    obs_nodes_list[[i]][[j]] <- Node$new(id = node_id, simulated = TRUE)
    obs_nodes_list[[i]][[j]]$addParent(common_var)
    obs_nodes_list[[i]][[j]]$addParent(means_list[[i]])
    this_expression <- paste0("Normal(",this_mean_id,",common_var)")
    obs_nodes_list[[i]][[j]]$setExpressions(this_expression)
  }
}

#Creating the network for all the nodes

diet_network <- Network$new(id = "Hierarchical_Normal_Model_1",
                            name = "Hierarchical Normal Model")

# Adding first eight nodes to the network

for (nd in c(mean, variance, tau, common_var, means_list)) {
  diet_network$addNode(nd)
}

# Adding all the experiment nodes to the network

for (nds in obs_nodes_list) {
  for (nd in nds) {
    diet_network$addNode(nd)
  }
}

# Creating a model with the network

diet_model <- Model$new(networks = list(diet_network),
                        id = "Diet_Experiment_Model")

# Entering all the observations

for (i in seq_along(observations)) {
  for (j in seq_along(observations[[i]])) {
    this_node_id <- paste0("y",i,j)
    this_value <- observations[[i]][j]
    diet_model$enter_observation(node = this_node_id,
                                 network = diet_model$networks[[1]]$id,
                                 value = this_value)
  }
}

# Creating json or cmpx file for the model
diet_model$to_json()
diet_model$to_cmpx()
