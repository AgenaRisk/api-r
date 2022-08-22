
### Node object as an R reference class
#id required (unique)
#name is same as id if not given (non unique)
#description (non unique optional string)
#type: ContinuousInterval, IntegerInterval, Boolean, Labelled, Ranked, DiscreteReal
#parents: ***will be either other Node objects or other id's of other Node objects (either a list of Nodes or a list of strings)
#simulated: boolean, in CMPX it's also NULL when FALSE
#distr_type: Manual, Partitioned, Expression - this will decide the state of next four attributes
#states: list of state names if Node$simulated == FALSE
#probabilities: list of numeric values if Node$simulated == FALSE, length of the list is either # of states x # of all parent states (if distr_type manual), or it depends on the expressions/partitions
#expressions: list of strings if Node$distr_type != "Manual", length is 1 if Node$distr_type == "Expression", length is # of partitions if Node$distr_type == "Partitioned"
#partitions: list of parent IDs which partitioned expressions are based on if Node$distr_type == "Partitioned"
#variables: list of variables (name and value)
Node <- setRefClass("Node",
                    fields = list(id = "character",
                                  name = "character",
                                  description = "character",
                                  type = "character",
                                  parents = "list",
                                  simulated = "logical",
                                  distr_type = "character",
                                  states = "character",
                                  probabilities = "list",
                                  expressions = "character",
                                  partitions = "character",
                                  variables = "list"),
                    methods = list(
                      show = function(verbose=FALSE){
                        if (!verbose) {
                          cat(paste0("Bayesian Network Node: \"", .self$name, "\"\nUnique identifier: ", .self$id))
                          if (.self$simulated == TRUE) {
                            cat("\nNode type:", .self$type, "(simulated)")
                          } else {
                            cat("\nNode type:", .self$type)
                          }
                        } else {
                          cat(paste0("Bayesian Network Node: \"", .self$name, "\"\nUnique identifier: ", .self$id))
                          if (.self$simulated == TRUE) {
                            cat("\nNode type:", .self$type,"(simulated)")
                          } else {
                            cat("\nNode type:", .self$type)
                          }
                          if (length(.self$parents) == 0) {
                            cat("\nThe node has no parents.")
                          } else if (length(.self$parents) == 1) {
                            cat("\nParent node:", .self$parents[[1]]$name)
                          } else if (length(.self$parents) == 2) {
                            cat(paste0("\nParent nodes: ", .self$parents[[1]]$name, "; ", .self$parents[[2]]$name))
                          } else if (length(.self$parents) > 2) {
                            cat(paste0("\nParent nodes: ", .self$parents[[1]]$name, "; "))
                            for (pr in 2:(length(.self$parents)-1)) {
                              cat(paste0(.self$parents[[pr]]$name, "; "))
                            }
                            cat(.self$parents[[length(.self$parents)]]$name)
                          }
                          cat("\nNPT type:", .self$distr_type)
                          if (.self$distr_type == "Manual") {
                          }
                        }
                      },
                      initialize = function(id, name = NULL, description = NULL, type = NULL, simulated = FALSE, states = NULL){
                        
                        'Creates a new Node object, a unique id is required, other fields are filled with defaults unless specified.
                        Node id, name, description, type, states, and whether it is a simulation or regular node do not depend on its edges and parents in a network,
                        a Node object can be defined with all this information outside a Network as well.
                        To add/remove/modify parents, distr_type, probabilities, expressions, and partitions; use the corresponding method.'
                        
                        #assigning $id - mandatory input to create a new Node object
                        .self$id <<- id
                        
                        #assigning $name - input name if given, id otherwise
                        if (is.null(name)) {
                          .self$name <<- id
                        } else {
                          .self$name <<- name
                        }
                        
                        #assigning $description - input desc if given, New Node Object otherwise
                        if (is.null(description)) {
                          .self$description <<- "New Node"
                        } else {
                          .self$description <<- description
                        }
                        
                        #setting $simulated - false by default
                        if (simulated) {
                          .self$simulated <<- TRUE
                        } else {
                          .self$simulated <<- FALSE
                        }
                        
                        #setting $type - Boolean by default, input type if given correctly
                        if (.self$simulated) {
                          if (is.null(type)) {
                            .self$type <<- "ContinuousInterval"
                          } else {
                            if(type == "IntegerInterval"){
                              .self$type <<- type
                            } else {
                              .self$type <<- "ContinuousInterval"
                            }
                          }
                        } else {
                          if (is.null(type)) {
                            .self$type <<- "Boolean"
                          } else {
                            if (type == "Labelled" || type == "Ranked" || type == "DiscreteReal" || type == "ContinuousInterval" || type == "IntegerInterval") {
                              .self$type <<- type
                            } else {
                              .self$type <<- "Boolean"
                            }
                          }
                        }

                        #setting $states - null if simulated, depends on type if not simulated
                        if (.self$simulated) {
                          .self$states <<- character(0)
                        } else {
                          if (is.null(states)) { #if no states given, set defaults based on type
                            if (.self$type == "Boolean") {
                              .self$states <<- c("False", "True") #Boolean type default states
                            } else if (.self$type == "Ranked") {
                              .self$states <<- c("Low", "Medium", "High") #Ranked type default states
                            } else if (.self$type == "Labelled") {
                              .self$states <<- c("False", "True") #Labelled type default states
                            } else if (.self$type =="DiscreteReal") {
                              .self$states <<- c("0.0", "1.0") #DiscreteReal type default states
                            }
                          } else { #if input states given, check if it's fine based on type
                            if (.self$type == "Boolean") {
                              if (length(states) == 2) { #if Boolean and input states have two names, use them
                                .self$states <<- states 
                              } else { #if Boolean and input states do not have two names, use default
                                .self$states <<- c("False", "True")
                              }
                            } else if (.self$type == "Ranked" || .self$type == "Labelled" || .self$type == "DiscreteReal") {
                              .self$states <<- states
                            } else {
                              .self$states <<- NULL
                            }
                          } 
                        }
                        
                        #sensible defaults are assigned to probabilitiess/expressions which will be rewritten with the specific methods
                        if (.self$simulated) {
                          .self$distr_type <<- "Expression"
                          .self$expressions <<- "Normal(0,1000000)"
                        } else {
                          if (.self$type == "ContinuousInterval" || .self$type == "IntegerInterval") {
                            .self$distr_type <<- "Expression"
                            .self$expressions <<- "Normal(0,1000000)"
                          } else {
                            .self$distr_type <<- "Manual"
                            .self$probabilities <<- vector(mode = "list", length = length(.self$states))
                            for (i in seq_along(.self$probabilities)) {
                              probabilities[[i]] <<- 1/length(.self$probabilities)
                            }
                          }
                        }
                      },
                      get_parents = function() {
                        parList <- c()
                        if (length(.self$parents)>0) {
                          for (i in seq_along(.self$parents)) {
                            parList[i] <- .self$parents[[i]]$id
                          }
                        }
                        else {
                          parList <- NULL
                        }
                        return(parList)
                      },
                      add_parent = function(newParent){
                        'Adds a Node object as a new parent node to the current Node object and resets/resizes the NPT values and expressions of the Node as needed.
                        Parents list of a Node object is a list of other Node objects.
                        The input parameter of the function is a Node object variable. A good practice is to use Node ids as their variable names.'
                        temp_par_list <- c()
                        for (pr in .self$parents) {
                          temp_par_list <- append(temp_par_list, pr$id)
                        }
                        if (!(newParent$id %in% temp_par_list)) {
                          parents <<- append(parents, newParent)
                        }
                        
                        if (.self$distr_type == "Manual") {
                          #update probabilities when parent added (reset to uniform with correct number of values)
                          updated_probs <- vector(mode = "list", length = length(.self$states))
                          temp_length <- 1
                          if (length(.self$parents)>0) {
                            for (prt in .self$parents) {
                              temp_length <- temp_length * length(prt$states)
                            }
                          }
                          
                          .self$set_probabilities(updated_probs)
                          for (i in seq_along(.self$probabilities)) {
                            probabilities[[i]] <<- rep(1/length(.self$probabilities), temp_length)
                          }
                          
                          cat("Node", newParent$id, "has been added to the parents list of", .self$id, "\nNPT values for", .self$id, "are reset to uniform\n")
                        } else {
                          cat("Node", newParent$id, "has been added to the parents list of", .self$id, "\nNow you can use", newParent$id, "in the expression of", .self$id, "\n")
                        }
                      },
                      addParent_byID = function(newParentID, varList) {
                        'This is a method to add parent Nodes by their ids for cmpx parser capabilities.
                        To add parents to Node objects, please use $add_parent(Node) method.'
                        
                        for (i in seq_along(varList)) {
                          if (newParentID == varList[[i]]$id) {
                            .self$add_parent(varList[[i]])
                          }
                        }
                        #This does not do any smart adjustments to NPTs/expressions - only used in cmpx parser
                      },
                      remove_parent = function(oldParent) {
                        'Removes a Node object from parents of the current Node object and resets/resizes the NPT values and expressions of the Node as needed.
                        The input parameter of the function is a Node object variable. A good practice is to use Node ids as their variable names.'
                        if (oldParent$id %in% .self$get_parents()) {
                          for (i in seq_along(.self$parents)) {
                            if (oldParent$id == .self$parents[[i]]$id) {
                              parents <<- .self$parents[-i]
                              break
                            }
                          }
                        }
                        
                        if (.self$distr_type == "Manual") {
                          updated_probs <- vector(mode = "list", length = length(.self$states))
                          temp_length <- 1
                          if (length(.self$parents)>0) {
                            for (prt in .self$parents) {
                              temp_length <- temp_length * length(prt$states)
                            }
                          }
                          
                          .self$set_probabilities(updated_probs)
                          for (i in seq_along(.self$probabilities)) {
                            probabilities[[i]] <<- rep(1/length(.self$probabilities), temp_length)
                          }
                          cat("Node", oldParent$name, "has been removed from the parents list of", .self$name, "\nNPT values for", .self$name, "are reset to uniform\n")
                        } else if (.self$distr_type == "Partitioned") {
                          if (oldParentID %in% .self$partitions) {
                            partitions <<- partitions[partitions != oldParentID]
                            temp_length <- 1
                            if (length(.self$partitions) > 0) {
                              for (pt in .self$partitions) {
                                for (prt in .self$parents) {
                                  if (pt == prt$id) {
                                    temp_length <- temp_length * length(prt$states)
                                  }
                                }
                              }
                            }
                            expressions <<- rep("Normal(0,1000000)", temp_length)
                            cat("Node", oldParent$name, "has been removed from the parents list of", .self$name, "\nPartitioned expressions for", .self$name, "are reset to Normal distribution\n")
                          } else {
                            cat("Node", oldParent$name, "has been removed from the parents list of", .self$name, "\n")
                          }
                        } else {
                          expressions <<- "Normal(0,1000000)"
                          cat("Node", oldParent$name, "has been removed from the parents list of", .self$name, "\nExpression for", .self$name, "is reset to Normal distribution\n")
                        }
                      },
                      set_distribution_type = function(new_distr_type) {
                        'A method to set the distribution type of a Node for the table configurations.'
                        
                        if (.self$simulated) {
                          if (new_distr_type == "Partitioned" && length(.self$parents) > 0) {
                              distr_type <<- "Partitioned" ######if successfully changed to Partitioned, we need to reset temp default expressions
                            } else {
                              distr_type <<- "Expression"
                              cat("Node", .self$id, "has no parents. Distribution type is set to Expression instead.\n")
                            }
                        } else {
                          if (new_distr_type == "Manual" || new_distr_type == "Expression") {
                            distr_type <<- new_distr_type
                          } else if (new_distr_type == "Partitioned") { ######if successfully changed to Partitioned, we need to reset temp default expressions
                              if (length(.self$parents > 0)) { 
                                distr_type <<- new_distr_type
                              } else { 
                                distr_type <<- "Expression" #if Node has no parents, do not allow Partitioned
                                cat("Node", .self$id, "has no parents. Distribution type is set to Expression instead.\n")
                              }
                            } else {
                              distr_type <<- "Manual" #if incorrect input, set it to default Manual
                              cat("Incorrect input. Distribution type is set to Manual instead.\n")
                            }
                        }
                      },
                      set_probabilities = function(new_probs, by_rows=TRUE) {
                        if (by_rows) {
                          if (!.self$simulated && .self$distr_type == "Manual" && (.self$type != "ContinuousInterval" || .self$type != "IntegerInterval")) {
                            if (length(new_probs) == length(.self$states)) {
                              temp_length <- 1
                              if (length(.self$parents)>0) {
                                for (prt in .self$parents) {
                                  temp_length <- temp_length * length(prt$states)
                                }
                              }
                              
                              subset_length_control <- 1
                              for (subset in new_probs) {
                                if (length(subset) == temp_length) {
                                  subset_length_control <- subset_length_control * 1
                                } else {
                                  subset_length_control <- subset_length_control * 0
                                }
                              }
                              
                              ######need another control to check probability sums are 1
                              if (subset_length_control == 1) {
                                probabilities <<- new_probs
                              }
                            }
                          }
                        } else {
                          if (!.self$simulated && .self$distr_type == "Manual" && (.self$type != "ContinuousInterval" || .self$type != "IntegerInterval")) {
                            temp_length <- 1
                            if (length(.self$parents)>0) {
                              for (prt in .self$parents) {
                                temp_length <- temp_length * length(prt$states)
                              }
                            }
                            
                            if (length(new_probs) == temp_length) {
                              subset_length_control <- 1
                              for (subset in new_probs) {
                                if (length(subset) == length(.self$states)) {
                                  subset_length_control <- subset_length_control * 1
                                } else {
                                  subset_length_control <- subset_length_control * 0
                                }
                              }
                            }
                            
                            if (subset_length_control == 1) {
                              for (i in seq_along(new_probs)) {
                                for (j in seq_along(new_probs[[i]])) {
                                  probabilities[[j]][[i]] <<- new_probs[[i]][[j]] 
                                }
                              }
                            }
                          }
                        }
                      },
                      set_expressions = function(new_expr,partition_parents=NULL) {
                        if(!is.null(partition_parents)) {
                          partitions <<- partition_parents ######need to check these are in Node's parents list
                          expressions <<- new_expr ######need to check length of exprs is equal to parent_states_product
                        } else {
                            expressions <<- new_expr ######need to make sure length of exprs is 1 if not partitioned
                        }
                      },
                      get_variables = function(){
                        varList <- c()
                        if (length(.self$variables)>0) {
                          for (i in seq_along(.self$variables)) {
                            varList[i] <- .self$variables[[i]][[1]]
                          }
                        }
                        else {
                          varList <- NULL
                        }
                        return(varList)
                      },
                      set_variable = function(variable_name, variable_value) {
                        cur_vars <- .self$get_variables()
                        if (is.null(cur_vars)){
                          var_num <- 0
                        } else {
                          var_num <- length(cur_vars)
                        }
                        
                        if(!is.null(cur_vars) && variable_name %in% cur_vars){
                          cat("There is already a variable defined with this name")
                        } else {
                          variables[[var_num+1]] <<- list(variable_name, variable_value)
                        }
                      },
                      remove_variable = function(variable_name){
                        cur_vars <- .self$get_variables()
                        
                        if(is.null(cur_vars)){
                          cat("This node has no variables")
                        } else {
                          for (i in seq_along(.self$variables)) {
                            if (.self$variables[[i]][[1]] == variable_name) {
                              variables <<- variables[-i]
                              cat(variable_name, "has been removed from the node's variables")
                              break
                            } else {
                              cat("This node does not have a variable called", variable_name)
                            }
                          }
                        }
                      })
                    )

#Network object as an R reference class
Network <- setRefClass("Network",
                       fields = list(id = "character",
                                     name = "character",
                                     description = "character",
                                     nodes = "list"),
                       methods = list(
                         show = function() {
                           cat(paste0("Bayesian Network: \"", .self$name, "\"\nID: ", .self$id))
                           cat(paste0("\nNodes in the network: ", .self$nodes[[1]]$name, "; "))
                           for (nd in 2:(length(.self$nodes)-1)) {
                             cat(paste0(.self$nodes[[nd]]$name,"; "))
                           }
                           cat(.self$nodes[[length(.self$nodes)]]$name)
                         },
                         initialize = function(id, name = NULL, description = NULL, nodes = NULL) {
                           #assigning $id - mandatory input to create a new Network object
                           .self$id <<- id
                           
                           #assigning $name - input name if given, id otherwise
                           if(is.null(name)){
                             .self$name <<- id
                           } else{
                             .self$name <<- name
                           }
                           
                           #assigning $description - input desc if given, New Node Object otherwise
                           if (is.null(description)) {
                             .self$description <<- "New Network"
                           } else {
                             .self$description <<- description
                           }
                           
                           if (!is.null(nodes)) {
                             .self$nodes <<- nodes
                           }
                         },
                         plot = function(){
                           plot_network(.self)
                         },
                         get_nodes = function() {
                           nodeList <- c()
                           if (length(.self$nodes)>0) {
                             for (i in seq_along(.self$nodes)) {
                               nodeList[i] <- .self$nodes[[i]]$id
                             }
                           }
                           else {
                             nodeList <- NULL
                           }
                           return(nodeList)
                         },
                         
                         add_node = function(newNode) {
                           'A method to add new Node objects to a Network.
                           Note that adding a new Node to the network does not automatically add its parents to the network.
                           You need to add all the parents separately too.'
                           
                           if (newNode$id %in% .self$get_nodes()) {
                             cat("There is already a node in the network with this ID")
                           } else {
                             nodes <<- append(nodes,newNode)
                             cat(newNode$name, "is successfully added to the network")
                           }
                           
                         },
                         remove_node = function(oldNode) {
                           'remove Node from Network'
                           if (oldNode$id %in% .self$get_nodes()) {
                             for (i in seq_along(.self$nodes)) {
                               if (oldNode$id == .self$nodes[[i]]$id) {
                                 nodes <<- nodes[-i]
                                 break
                               }
                             }

                             cat(oldNode$name, "is successfully removed from the network. If", oldNode$name, "had any child nodes in the network, make sure to adjust their parents accordingly")
                           } else {
                             cat("This node is not in the network")
                           }
                         })
                       )

#Dataset object as an R reference class
#id: id of the "scenario"
#observations: set of observations (values / states) for all the observed Nodes in the Model
#results: set of result values / posteriors for all the Nodes in the Model
Dataset <- setRefClass("Dataset",
                       fields = list(id = "character",
                                observations = "list",
                                results = "list"),
                       methods = list(
                         initialize = function(id, observations){
                             .self$id <<- id
                             if(!is.null(observations)){
                               .self$observations <<- observations
                             }
                             
                         }
                       )) 


#Model object as an R reference class
#One CMPX file corresponds to one R Model instance
Model <- setRefClass("Model",
                     fields = list(id = "character",
                                   networks = "list",
                                   dataSets = "list",
                                   networkLinks = "list"
                                   ),
                     methods = list(
                       show = function(){
                         cat(paste0("BN Model: \"",.self$id,"\""))
                         cat("\nNetworks in this model are:")
                         for (nt in .self$networks){
                           cat("\n-",nt$id)
                         }
                       },
                       initialize = function(id=NULL, networks, from_cmpx=FALSE, dataSets = NULL, networkLinks = NULL){
                         if(is.null(id)){
                           .self$id <<- paste(networks[[1]]$id,"Model")
                         } else{
                           .self$id <<- id
                         }
                         .self$networks <<- networks
                         if(!from_cmpx){
                           .self$dataSets[[1]] <- Dataset$new(id="Scenario 1",observations=NULL) 
                         }
                         if(from_cmpx){
                           if(!is.null(dataSets)){
                             .self$dataSets <<- dataSets 
                           }
                           if(!is.null(networkLinks)){
                             .self$networkLinks <<- networkLinks
                           }
                           
                         }
                       },
                       get_networks = function() {
                         networkList <- c()
                         if (length(.self$networks)>0) {
                           for (i in seq_along(.self$networks)) {
                             networkList[i] <- .self$networks[[i]]$id
                           }
                         }
                         else {
                           networkList <- NULL
                         }
                         return(networkList)
                       },
                       add_network = function(newNetwork){
                         'add Network to the Model'
                         if (newNetwork$id %in% .self$get_networks()) {
                           cat("There is already a network in the model with this ID")
                         } else {
                           networks <<- append(networks,newNetwork)
                           cat(newNetwork$id, "is successfully added to the model")
                         }
                       },
                       remove_network = function(oldNetwork) {
                         'remove Network from the Model'
                         if (oldNetwork$id %in% .self$get_networks()) {
                           for (i in seq_along(.self$networks)) {
                             if (oldNetwork$id == .self$networks[[i]]$id) {
                               networks <<- networks[-i]
                               break
                             }
                           }
                           
                           cat(oldNetwork$id, "is successfully removed from the model. If", oldNetwork$id, "had any links to other networks, make sure to adjust network links accordingly")
                         } else {
                           cat("This network is not in the model")
                         }
                       },
                       add_network_link = function(source_network, source_node, target_network, target_node, link_type, pass_state=NULL){
                         
                         for (i in seq_along(.self$networks)) {
                           if (source_network == .self$networks[[i]]$id) {
                             out_network <- .self$networks[[i]]
                             for (j in seq_along(out_network$nodes)) {
                               if (source_node == out_network$nodes[[j]]$id) {
                                 out_node <- out_network$nodes[[j]]
                               }
                             }
                           }
                         }
                         
                         for (i in seq_along(.self$networks)) {
                           if (target_network == .self$networks[[i]]$id) {
                             in_network <- .self$networks[[i]]
                             for (j in seq_along(in_network$nodes)) {
                               if (target_node == in_network$nodes[[j]]$id) {
                                 in_node <- in_network$nodes[[j]]
                               }
                             }
                           }
                         }
                         
                         targets <- c()
                         if(length(.self$networkLinks)>0) {
                           for (i in seq_along(.self$networkLinks)) {
                             targets <- append(targets, .self$networkLinks[[i]]$targetNode)
                           }
                         }
                         
                         if (length(in_node$parents)>0) {
                           cat("Target node is a child node in its network, it cannot be a target node")
                         } else {
                           if (!is.null(targets) && target_node %in% targets) {
                             cat("The required target node is already an target for another link")
                           } else {
                             num_list <- c("ContinuousInterval", "IntegerInterval", "DiscreteReal")
                             num_intv_list <- c("ContinuousInterval", "IntegerInterval")
                             val_check <- 0
                             
                             if (link_type == "Marginals") {
                               if (!out_node$simulated && !in_node$simulated) {
                                 val_check <- val_check
                               } else {
                                 if (out_node$type %in% num_list && in_node$type %in% num_list) {
                                   val_check <- val_check
                                 } else {
                                   val_check <- val_check + 1
                                 }
                               }
                             }
                             
                             if (link_type %in% c("Mean", "Median", "Variance", "StandardDeviation", 
                                                  "LowerPercentile", "UpperPercentile")) {
                               if (out_node$type %in% num_list && in_node$simulated){
                                 val_check <- val_check
                               } else {
                                 val_check <- val_check + 1
                               }
                             }
                             
                             if (link_type == "State") {
                               if(!(out_node$type %in% num_intv_list) && in_node$simulated) {
                                 val_check <- val_check
                               } else {
                                 val_check <- val_check + 1
                               }
                             }
                             
                             if(val_check == 0){
                               if (link_type == "State"){
                                 if (is.null(pass_state)){
                                   cat("\nPlease enter the source node state to be passed on")
                                 } else {
                                   newLink <- list(sourceNode = source_node,
                                                   sourceNetwork = source_network,
                                                   targetNode = target_node,
                                                   targetNetwork = target_network,
                                                   passState = pass_state,
                                                   type = link_type) 
                                 }
                               } else {
                                 newLink <- list(sourceNode = source_node,
                                                 sourceNetwork = source_network,
                                                 targetNode = target_node,
                                                 targetNetwork = target_network,
                                                 type = link_type)
                               }
                               
                               cur_length <- length(.self$networkLinks)
                               networkLinks[[cur_length+1]] <<- newLink
                             } else {
                               cat("\nThe link between source node and target node is not valid")
                             }
                           }
                           }
                       },
                       remove_network_link = function(source_network,source_node,target_network,target_node){
                         
                         if(length(.self$networkLinks)>0){
                           for (i in seq_along(.self$networkLinks)){
                             if(.self$networkLinks[[i]]$sourceNetwork == source_network &&
                                .self$networkLinks[[i]]$sourceNode == source_node &&
                                .self$networkLinks[[i]]$targetNetwork == target_network &&
                                .self$networkLinks[[i]]$targetNode == target_node) {
                               networkLinks <<- networkLinks[-i]
                               break
                             } else {
                               cat("This network link is not in the model")
                             }
                           } 
                         } else {
                           cat("This model does not have any network links")
                         }
                       },
                       remove_all_network_links = function(){
                         networkLinks <<- list()
                       },
                       create_scenario = function(id){
                         dataSets <<- append(dataSets,Dataset$new(id=id, observations=NULL))
                       },
                       get_scenarios = function(){
                         scenList <- c()
                         if (length(.self$dataSets)>0) {
                           for (i in seq_along(.self$dataSets)) {
                             scenList[i] <- .self$dataSets[[i]]$id
                           }
                         }
                         else {
                           scenList <- NULL
                         }
                         return(scenList)
                         
                       },
                       remove_scenario = function(oldScenario){
                         if (oldScenario %in% .self$get_scenarios()) {
                           for (i in seq_along(.self$dataSets)) {
                             if (oldScenario == .self$dataSets[[i]]$id) {
                               dataSets <<- dataSets[-i]
                               break
                             }
                           }
                           
                           cat(oldScenario, "is successfully removed from the model's scenarios.")
                         } else {
                           cat("This scenario is not in the model")
                         }
                       },
                       enter_observation = function(scenario=NULL, node, network, value, variable_input = FALSE, soft_evidence = FALSE){
                         if(is.null(scenario)){
                           if(!variable_input){
                             new_obs <- list(node=node,
                                             network = network,
                                             entries = list())
                             if(!soft_evidence) {
                               new_obs$entries[[1]] <- list(weight = 1, value = value)
                             } else {
                               obs_num <- length(value)/2
                               for (i in 1:obs_num){
                                 new_obs$entries[[i]] <- list(weight = as.numeric(value[2*i]), value = value[2*i-1])
                               }
                             }
                           } else {
                             new_obs <- list(node=node,
                                             network = network,
                                             constantName = value,
                                             entries = list())
                             
                             for (i in seq_along(.self$networks)){
                               if(.self$networks[[i]]$id == network) {
                                 this_network <- .self$networks[[i]]
                                 for (j in seq_along(this_network$nodes)) {
                                   if (this_network$nodes[[j]]$id == node) {
                                     this_node <- this_network$nodes[[j]]
                                     for (k in seq_along(this_node$variables)) {
                                       if (this_node$variables[[k]][[1]] == value) {
                                         obs_value <- this_node$variables[[k]][[2]]
                                       }
                                     }
                                   }
                                 }
                               }
                             }
                             new_obs$entries[[1]] <- list(weight = 1, value = obs_value)
                           }
                           
                           
                           exist_check <- 0
                           if(length(dataSets[[1]]$observations)>0){
                             for (i in seq_along(dataSets[[1]]$observations)){
                               if(node == dataSets[[1]]$observations[[i]]$node && network == dataSets[[1]]$observations[[i]]$network){
                                 dataSets[[1]]$observations[[i]]$entries <<- new_obs$entries[[1]]
                                 exist_check <- 1
                               }
                             }
                           }

                           
                           if(exist_check == 0){
                             cur_length <- length(dataSets[[1]]$observations)
                             dataSets[[1]]$observations[[cur_length+1]] <<- new_obs
                           }

                         } else {
                           
                           if(!variable_input){
                             new_obs <- list(node=node,
                                             network = network,
                                             entries = list())
                             if(!soft_evidence) {
                               new_obs$entries[[1]] <- list(weight = 1, value = value)
                             } else {
                               obs_num <- length(value)/2
                               for (i in 1:obs_num){
                                 new_obs$entries[[i]] <- list(weight = as.numeric(value[2*i]), value = value[2*i-1])
                               }
                             }                           } else {
                             new_obs <- list(node=node,
                                             network = network,
                                             constantName = value,
                                             entries = list())
                             
                             for (i in seq_along(.self$networks)){
                               if(.self$networks[[i]]$id == network) {
                                 this_network <- .self$networks[[i]]
                                 for (j in seq_along(this_network$nodes)) {
                                   if (this_network$nodes[[j]]$id == node) {
                                     this_node <- this_network$nodes[[j]]
                                     for (k in seq_along(this_node$variables)) {
                                       if (this_node$variables[[k]][[1]] == value) {
                                         obs_value <- this_node$variables[[k]][[2]]
                                       }
                                     }
                                   }
                                 }
                               }
                             }
                             new_obs$entries[[1]] <- list(weight = 1, value = obs_value)
                           }
                           
                           
                           for (i in seq_along(dataSets)){
                             if(scenario == dataSets[[i]]$id){
                               
                               exist_check <- 0
                               if(length(dataSets[[i]]$observations)>0){
                                 for (j in seq_along(dataSets[[i]]$observations)){
                                   if(node == dataSets[[i]]$observations[[j]]$node && network == dataSets[[i]]$observations[[j]]$network){
                                     dataSets[[i]]$observations[[j]]$entries <<- new_obs$entries[[1]]
                                     exist_check <- 1
                                   }
                                 }
                               }
                               
                               
                               if(exist_check == 0){
                                 cur_length <- length(dataSets[[i]]$observations)
                                 dataSets[[i]]$observations[[cur_length+1]] <<- new_obs
                               }

                             }
                           }
                         }
                       },
                       remove_observation = function(scenario=NULL, node, network){
                         if(is.null(scenario)){
                           
                           for (i in seq_along(dataSets[[1]]$observations)){
                             if(node == dataSets[[1]]$observations[[i]]$node && network == dataSets[[1]]$observations[[i]]$network){
                               dataSets[[1]]$observations <<- dataSets[[1]]$observations[-i]
                               break
                             }
                           }
                         } else {
                           for (i in seq_along(dataSets)){
                             if(scenario == dataSets[[i]]$id){
                               for (j in seq_along(dataSets[[i]]$observations)){
                                 if(node == dataSets[[i]]$observations[[j]]$node && network == dataSets[[i]]$observations[[j]]$network){
                                   dataSets[[i]]$observations <<- dataSets[[i]]$observations[-j]
                                   break
                                 }
                               }
                               }}
                         }
                       },
                       clear_scenario_observations = function(scenario){
                         for (i in seq_along(.self$dataSets)){
                           if(scenario == .self$dataSets[[i]]$id){
                             dataSets[[i]]$observations <<- list()
                           }}
                       },
                       clear_all_observations = function(){
                         
                         for (i in seq_along(.self$dataSets)){
                           dataSets[[i]]$observations <<- list()
                         }
                       },
                       to_cmpx = function(filename=NULL, settings=NULL){
                         if(is.null(settings)){
                           json_object <- generate_cmpx(.self)
                         } else {
                           json_object <- generate_cmpx(.self, settings = settings)
                         }
                         
                         if(is.null(filename)){
                           file_name <- paste0(.self$id,".cmpx")
                         } else {
                           file_name <- paste0(filename,".cmpx")
                         }
                         write(json_object,file_name)
                       },
                       to_json = function(filename=NULL, settings=NULL){
                         if(is.null(settings)){
                           json_object <- generate_cmpx(.self)
                         } else {
                           json_object <- generate_cmpx(.self, settings = settings)
                         }
                         
                         if(is.null(filename)){
                           file_name <- paste0(.self$id,".json")
                         } else {
                           file_name <- paste0(filename,".json")
                         }
                         write(json_object,file_name)
                       }
                     )) 

#function to read input CMPX file to create Model and its Networks and their Nodes
from_cmpx <- function(modelPath){
  
  #read CMPX file, assign elements to R lists
  cmpx_input <- rjson::fromJSON(file=modelPath)
  
  cmpx_model <- cmpx_input$model
  cmpx_networks <- cmpx_model$networks
  cmpx_dataSets <- cmpx_model$dataSets 
  cmpx_networkLinks <- cmpx_model$links  #currently unused parts of CMPX: cmpx_model$settings, cmpx_model$riskTable, cmpx_model$graphics

  #creating empty lists for Network and Node objects with the correct number of Networks in the CMPX model
  networks <- vector(mode = "list",length = length(cmpx_networks))
  nodes <- vector(mode = "list",length = length(cmpx_networks))
  links <- vector(mode = "list",length = length(cmpx_networks))
  datasets <- vector(mode = "list",length = length(cmpx_dataSets))
  
  #filling in the list of Network objects with each network in the CMPX model
  for (i in seq_along(cmpx_networks)) {
    networks[[i]] <- Network$new(id = cmpx_networks[[i]]$id,
                                 name = cmpx_networks[[i]]$name,
                                 description = cmpx_networks[[i]]$description)
    
    #filling in the list of Node objects with each node of each network
    #keep in mind this list is two dimensional, each list element is a list of Nodes itself
    for (j in seq_along(cmpx_networks[[i]]$nodes)) {
      nodes[[i]][[j]] <- Node$new(id = cmpx_networks[[i]]$nodes[[j]]$id,
                                  name = cmpx_networks[[i]]$nodes[[j]]$name,
                                  description = cmpx_networks[[i]]$nodes[[j]]$description,
                                  type = cmpx_networks[[i]]$nodes[[j]]$configuration$type)
      
      if (is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$simulated)) {
        nodes[[i]][[j]]$simulated <- FALSE
      } else {
        nodes[[i]][[j]]$simulated <- TRUE
      }
      
      nodes[[i]][[j]]$distr_type = cmpx_networks[[i]]$nodes[[j]]$configuration$table$type
      
    }
    networks[[i]]$nodes <- nodes[[i]]
    links[[i]] <- cmpx_networks[[i]]$links
  }

  for (i in seq_along(networks)) {
    for (j in seq_along(networks[[i]]$nodes)) {
      for (k in seq_along(links[[i]])) {
        if (links[[i]][[k]]$child == networks[[i]]$nodes[[j]]$id) {
          networks[[i]]$nodes[[j]]$addParent_byID(links[[i]][[k]]$parent, networks[[i]]$nodes)
        }
      }
    }
  }
  
  
  for (i in seq_along(cmpx_networks)) {
    for (j in seq_along(cmpx_networks[[i]]$nodes)) {
      if (!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$states)) {
        nodes[[i]][[j]]$states <- cmpx_networks[[i]]$nodes[[j]]$configuration$states
        nodes[[i]][[j]]$probabilities <- vector(mode = "list", length = length(nodes[[i]][[j]]$states))
        for (k in seq_along(nodes[[i]][[j]]$states)) {
          nodes[[i]][[j]]$probabilities[[k]] <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities[[k]]
        }
      }
      
      if (!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions)) {
        nodes[[i]][[j]]$expressions <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions
      }
      
      if (!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$partitions)) {
        nodes[[i]][[j]]$partitions <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$partitions
      }
    }
  }
  
  for (i in seq_along(networks)) {
    for (j in seq_along(networks[[i]]$nodes)) {
      if (!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$variables)) {
        nodes[[i]][[j]]$variables <- list()
        
        for (k in seq_along(cmpx_networks[[i]]$nodes[[j]]$configuration$variables)) {
          nodes[[i]][[j]]$variables[[k]] <- list(cmpx_networks[[i]]$nodes[[j]]$configuration$variables[[k]]$name,
                                                 cmpx_networks[[i]]$nodes[[j]]$configuration$variables[[k]]$value)
        }

      }

      
    }}

  
  for (i in seq_along(cmpx_dataSets)) {
    datasets[[i]] <- Dataset$new(id = cmpx_dataSets[[i]]$id, observations=NULL)
    
    datasets[[i]]$observations <- cmpx_dataSets[[i]]$observations
    datasets[[i]]$results <- cmpx_dataSets[[i]]$results
  }
  

  outputModel <- Model$new(from_cmpx=TRUE,
                           networks = networks,
                           networkLinks = cmpx_networkLinks,
                           dataSets = datasets)
  
  return(outputModel)
}






generate_cmpx <- function(inputModel, settings=NULL) {
  
  if(is.null(settings)){
    settings_list <- list(parameterLearningLogging = FALSE,
                          discreteTails = FALSE,
                          sampleSizeRanked = 5,
                          convergence = 0.001,
                          simulationLogging = FALSE,
                          iterations = 50,
                          tolerance = 1)
  } else {
    settings_list <- settings
  }
  

  #creating empty lists and sublists with correct length to be filled in later
  networks_list <- vector(mode = "list", length = length(inputModel$networks))
  
  nodes_list <- vector(mode = "list", length = length(inputModel$networks))
  for (i in seq_along(nodes_list)) {
    nodes_list[[i]] <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes))
  }
  
  config_list <- vector(mode = "list", length = length(inputModel$networks))
  for (i in seq_along(config_list)) {
    config_list[[i]] <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes))
  }
  
  table_list <- vector(mode = "list", length = length(inputModel$networks))
  for (i in seq_along(table_list)) {
    table_list[[i]] <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes))
  }
  
  variables_list <- vector(mode = "list", length = length(inputModel$networks))
  for (i in seq_along(table_list)) {
    variables_list[[i]] <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes))
  }
  
  datasets_list <- vector(mode = "list", length = length(inputModel$dataSets))
  
  obs_list <- vector(mode = "list", length = length(inputModel$dataSets))
  for (i in seq_along(obs_list)){
    obs_list[[i]] <- vector(mode = "list", length = length(inputModel$dataSets[[i]]$observations))
  }
  

  
  
  links_list <- vector(mode = "list", length = length(inputModel$networks))
  for (i in seq_along(nodes_list)) {
    links_amount <- 0
    for (nd in inputModel$networks[[i]]$nodes){
      links_amount <- links_amount + length(nd$parents)
    }
    links_list[[i]] <- vector(mode = "list", length = links_amount)
  }
  
  
  #table_list generation
  for (i in seq_along(table_list)){
    for (j in seq_along(table_list[[i]])){
      table_list[[i]][[j]] <- list(type = inputModel$networks[[i]]$nodes[[j]]$distr_type)
      if(inputModel$networks[[i]]$nodes[[j]]$distr_type == "Manual"){
        #table_list[[i]][[j]]$probabilities <- inputModel$networks[[i]]$nodes[[j]]$probabilities
        temp_probs_list <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes[[j]]$probabilities))
        for (k in seq_along(temp_probs_list)){
          temp_probs_sublist <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes[[j]]$probabilities[[k]]))
          for (m in seq_along(temp_probs_sublist)){
            temp_probs_list[[k]][[m]] <- inputModel$networks[[i]]$nodes[[j]]$probabilities[[k]][[m]]
          }
        }
        table_list[[i]][[j]]$probabilities <- temp_probs_list
      } else if (inputModel$networks[[i]]$nodes[[j]]$distr_type == "Expression"){
        temp_exp_list <- vector(mode = "list", length = 1)
        temp_exp_list[1] <- inputModel$networks[[i]]$nodes[[j]]$expressions
        table_list[[i]][[j]]$expressions <- temp_exp_list
      } else if (inputModel$networks[[i]]$nodes[[j]]$distr_type == "Partitioned"){
        temp_part_list <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes[[j]]$partitions))
        temp_exp_list <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes[[j]]$expressions))
        for (k in seq_along(temp_part_list)){
          temp_part_list[k] <- inputModel$networks[[i]]$nodes[[j]]$partitions[[k]]
        }
        for (k in seq_along(temp_exp_list)){
          temp_exp_list[k] <- inputModel$networks[[i]]$nodes[[j]]$expressions[[k]]
        }
        table_list[[i]][[j]]$partitions <- temp_part_list
        table_list[[i]][[j]]$expressions <- temp_exp_list
      }
    }
  }
  
  #variables_list generation
  for (i in seq_along(variables_list)){
    for (j in seq_along(variables_list[[i]])){
      
      if(length(inputModel$networks[[i]]$nodes[[j]]$variables)>0){
        temp_vars_length <- length(inputModel$networks[[i]]$nodes[[j]]$variables)
        variables_list[[i]][[j]] <- vector(mode = "list", length = temp_vars_length)
        
        for (k in 1:temp_vars_length){
          variables_list[[i]][[j]][[k]] <- list(name = inputModel$networks[[i]]$nodes[[j]]$variables[[k]][[1]],
                                                value = inputModel$networks[[i]]$nodes[[j]]$variables[[k]][[2]])
        }
      }
      

    }}
      
      
      
  for (i in seq_along(config_list)){
    for (j in seq_along(config_list[[i]])){
      config_list[[i]][[j]] <- list(type = inputModel$networks[[i]]$nodes[[j]]$type,
                                    simulated = inputModel$networks[[i]]$nodes[[j]]$simulated,
                                    input = FALSE,
                                    output = FALSE,
                                    table = table_list[[i]][[j]],
                                    variables = variables_list[[i]][[j]]
                                    )
      if(!inputModel$networks[[i]]$nodes[[j]]$simulated){
        temp_states_list <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes[[j]]$states))
        for (k in seq_along(inputModel$networks[[i]]$nodes[[j]]$states)){
          temp_states_list[k] <- inputModel$networks[[i]]$nodes[[j]]$states[k]
        }
        config_list[[i]][[j]]$states <- temp_states_list
      }
    }
  }
  

  
  for (i in seq_along(nodes_list)) {
    for (j in seq_along(nodes_list[[i]])){
      nodes_list[[i]][[j]] <- list(id = inputModel$networks[[i]]$nodes[[j]]$id,
                                   name = inputModel$networks[[i]]$nodes[[j]]$name,
                                   description = inputModel$networks[[i]]$nodes[[j]]$description,
                                   configuration = config_list[[i]][[j]])
    }
  }
  
  temp_parents_list <- list()
  temp_children_list <- list()
  
  for (i in seq_along(inputModel$networks)){
    temp_parents_list[[i]] <- list()
    temp_children_list[[i]] <- list()
    for (j in seq_along(inputModel$networks[[i]]$nodes)){
      if(length(inputModel$networks[[i]]$nodes[[j]]$parents) != 0){
        for (k in seq_along(inputModel$networks[[i]]$nodes[[j]]$parents)){
          temp_parents_list[[i]] <- append(temp_parents_list[[i]], inputModel$networks[[i]]$nodes[[j]]$parents[[k]]$id)
          temp_children_list[[i]] <- append(temp_children_list[[i]], inputModel$networks[[i]]$nodes[[j]]$id)
        }
      }
    }
  }
  
  
  for (i in seq_along(links_list)) {
    for (j in seq_along(links_list[[i]])){
      links_list[[i]][[j]] <- list(parent = temp_parents_list[[i]][[j]],
                                   child = temp_children_list[[i]][[j]])
    }
  }
  
      

  for (i in seq_along(networks_list)) {
    networks_list[[i]] <- list(id = inputModel$networks[[i]]$id,
                               name = inputModel$networks[[i]]$name,
                               description = inputModel$networks[[i]]$description,
                               nodes = nodes_list[[i]],
                               links = links_list[[i]])
  }
  

  if(length(inputModel$networkLinks) == 0){
    networklinks_list <- list()
  } else {
    networklinks_list <- inputModel$networkLinks
  }
  
  if (length(inputModel$dataSets) == 1 && length(inputModel$dataSets[[1]]$observations) == 0) {
    datasets_list[[1]] <- list(id = inputModel$dataSets[[1]]$id,
                               observations = inputModel$dataSets[[1]]$observations) 
  } else {
    entries_list <- vector(mode = "list", length = length(inputModel$dataSets))
    for (i in seq_along(entries_list)) {
      entries_list[[i]] <- vector(mode = "list", length = length(inputModel$dataSets[[i]]$observations))
      for (j in seq_along(entries_list[[i]])) {
        entries_list[[i]][[j]] <- vector(mode = "list",length = length(inputModel$dataSets[[i]]$observations[[j]]$entries))
      }
    }
    
    for (i in seq_along(datasets_list)) {
      for (j in seq_along(inputModel$dataSets[[i]]$observations)) {
        for (k in seq_along(inputModel$dataSets[[i]]$observations[[j]]$entries)){
          entries_list[[i]][[j]][[k]] <- list(weight = inputModel$dataSets[[i]]$observations[[j]]$entries[[k]]$weight,
                                              value = inputModel$dataSets[[i]]$observations[[j]]$entries[[k]]$value)
        }
      }
    }
    
    for (i in seq_along(datasets_list)) {
      for (j in seq_along(inputModel$dataSets[[i]]$observations)) {
        if (!is.null(inputModel$dataSets[[i]]$observations[[j]]$constantName)) {
          obs_list[[i]][[j]] <- list(node = inputModel$dataSets[[i]]$observations[[j]]$node,
                                     network = inputModel$dataSets[[i]]$observations[[j]]$network,
                                     constantName = inputModel$dataSets[[i]]$observations[[j]]$constantName,
                                     entries = entries_list[[i]][[j]])
        } else {
          obs_list[[i]][[j]] <- list(node = inputModel$dataSets[[i]]$observations[[j]]$node,
                                     network = inputModel$dataSets[[i]]$observations[[j]]$network,
                                     entries = entries_list[[i]][[j]])
        }
        
      }
      datasets_list[[i]] <- list(id = inputModel$dataSets[[i]]$id,
                                 observations = obs_list[[i]])
    }
  }
  
  
  
  
  
  # networklinks_list <- list(sourceNetwork = "placeholder",
  #                           sourceNode = "placeholder",
  #                           targetNetwork = "placeholder",
  #                           targetNode = "placeholder",
  #                           type = "placeholder",
  #                           passState = "placeholder")
  
  model_list <- list(settings = settings_list,
                     dataSets = datasets_list,
                     networks = networks_list,
                     links = networklinks_list)
  
  json_list <- list(model = model_list)
  json_object <- rjson::toJSON(json_list)
  
  return(json_object)

}





create_batch_cases <- function(inputModel, inputData){
  
  inputTable <- read.csv(file=inputData)
  col_headers <- names(inputTable)[-1]
  obs_nodes <- c()
  obs_networks <- c()
  for (nm in col_headers){
    obs_nodes <- append(obs_nodes,gsub("\\..*", "", nm))
    obs_networks <- append(obs_networks,gsub(".*\\.", "", nm))
  }

  for (i in seq_along(inputTable)){
    temp_id <- as.character(as.character(inputTable[i,][[1]]))
    inputModel$create_scenario(id = temp_id)
    for (j in seq_along(col_headers)){
      inputModel$enter_observation(scenario = temp_id,
                                   node = obs_nodes[j], network = obs_networks[j], 
                                   value = inputTable[i,][[j+1]])
    }

  }
  filename <- paste0(inputModel$id, "_Batch_Cases")
  inputModel$to_json(filename = filename)
  inputModel$clear_all_observations()
  
  for (i in seq_along(inputTable)){
    temp_id <- as.character(as.character(inputTable[i,][[1]]))
    inputModel$remove_scenario(temp_id)
  }
  
}


get_node_by_ID <- function(node_id, inputNetwork) {
  nodes <- inputNetwork$nodes
  node_ids <- inputNetwork$get_nodes()
  
  for (i in seq_along(node_ids)) {
    if (node_id == node_ids[[i]]) {
      node <- nodes[[i]]
    }
  }
  
  return(node)
}

create_network_matrix <- function(inputNetwork) {
  nodes <- inputNetwork$get_nodes()
  edge_matrix <- matrix(0, nrow = length(nodes), ncol = length(nodes),
                        dimnames = list(nodes,nodes))

  for (i in seq_along(nodes)) {
    the_parents <- get_node_by_ID(nodes[i],inputNetwork)$get_parents()
    for (j in seq_along(nodes)) {
      if (nodes[j] %in% the_parents) {
        edge_matrix[j,i] <- 1
      }
    }
  }
  return(edge_matrix)
}

plot_network <- function(inputNetwork) {
  
  edge_matrix <- create_network_matrix(inputNetwork)
  nodes <- inputNetwork$get_nodes()
  
  edge_matrix_network <- new("graphAM", adjMat=edge_matrix, edgemode="directed")
  edge_matrix_network <- Rgraphviz::layoutGraph(edge_matrix_network)

  Rgraphviz::renderGraph(edge_matrix_network)
}

colname_list_generator <- function(inputModel){
  colname_list <- c("Case")
  for (nt in inputModel$networks){
    for (nd in nt$nodes){
      colname_list <- append(colname_list,paste0(nd$id,".",nt$id))
    }
  }
  return(colname_list)
}

create_csv_template <- function(inputModel){
  colname_list <- colname_list_generator(inputModel)
  filename <- paste0(inputModel$id,"_DataSet.csv")
  
  write.table(t(colname_list),sep = ",", file = filename, row.names = FALSE, col.names = FALSE)
}