
#' BN Node object
#'
#' These represent the nodes in a BN.
#'
#' @field id node id
#' @field name node display name
#' @field description node description
#' @field type node type
#' @field parents node parent nodes
#' @field simulated whether node is simulated
#' @field distr_type node distribution type
#' @field states node states
#' @field probabilities node probabilities
#' @field expressions node expressions
#' @field partitions node expression partitions
#' @field variables node variables or constants
#' @export
#' @importFrom methods new
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

                        'A method to list all the existing parent nodes of a Node.'

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

                          message("Node ", newParent$id, " has been added to the parents list of ", .self$id, "\nNPT values for ", .self$id, " are reset to uniform\n")
                        } else {
                          message("Node ", newParent$id, " has been added to the parents list of ", .self$id, "\nNow you can use ", newParent$id, " in the expression of ", .self$id, "\n")
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
                          message("Node ", oldParent$name, " has been removed from the parents list of ", .self$name, "\nNPT values for ", .self$name, " are reset to uniform\n")
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
                            message("Node ", oldParent$name, " has been removed from the parents list of ", .self$name, "\nPartitioned expressions for ", .self$name, " are reset to Normal distribution\n")
                          } else {
                            message("Node ", oldParent$name, " has been removed from the parents list of ", .self$name, "\n")
                          }
                        } else {
                          expressions <<- "Normal(0,1000000)"
                          message("Node ", oldParent$name, " has been removed from the parents list of ", .self$name, "\nExpression for ", .self$name, " is reset to Normal distribution\n")
                        }
                      },
                      set_distribution_type = function(new_distr_type) {

                        'A method to set the table type (distr_type) of a node. If a Node is simulated,
                        its table type can be "Expression" or "Partitioned" - the latter is only if the
                        node has parent nodes. If a Node is not simulated, its table type can be "Manual",
                        "Expression", or "Partitioned Expression (if the node has parent nodes)".'

                        if (.self$simulated) {
                          if (new_distr_type == "Partitioned" && length(.self$parents) > 0) {
                              distr_type <<- "Partitioned" ######if successfully changed to Partitioned, we need to reset temp default expressions
                            } else {
                              distr_type <<- "Expression"
                              message("Node ", .self$id, " has no parents. Distribution type is set to Expression instead.\n")
                            }
                        } else {
                          if (new_distr_type == "Manual" || new_distr_type == "Expression") {
                            distr_type <<- new_distr_type
                          } else if (new_distr_type == "Partitioned") { ######if successfully changed to Partitioned, we need to reset temp default expressions
                              if (length(.self$parents > 0)) {
                                distr_type <<- new_distr_type
                              } else {
                                distr_type <<- "Expression" #if Node has no parents, do not allow Partitioned
                                message("Node ", .self$id, " has no parents. Distribution type is set to Expression instead.\n")
                              }
                            } else {
                              distr_type <<- "Manual" #if incorrect input, set it to default Manual
                              message("Incorrect input. Distribution type is set to Manual instead.\n")
                            }
                        }
                      },
                      set_probabilities = function(new_probs, by_rows=TRUE) {

                        'The method to set the probability values if the table type (distr_type) of a Node is "Manual".
                        new_probs is a list of numerical values, and the length of the input list depends on the number
                        of the states of the node and of its parents.

                        You can format the input list in two different orders. If the parameter by_rows is set to true,
                        the method will read the input list to fill in the NPT row by row; if set to false, the method
                        will read the input list to fill in the NPT column by columnn. See README.md for examples.'
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

                        'The method to set the probability values if the table type (distr_type) of a Node is "Expression"
                        or "Partitioned". If the table type is "Expression", new_expr is a single string and partition_parents
                        is left NULL. If the table type is "Partitioned", new_expr is a list of expressions for each parent
                        state, and partition_parents is a list of strings for each partitioned parent node id.'

                        if(!is.null(partition_parents)) {
                          partitions <<- partition_parents
                          expressions <<- new_expr
                        } else {
                            expressions <<- new_expr
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

                        'A method to set variables (constants) for a node. Takes the variable_name and
                        variable_value inputs which define a new variable (constant) for the node.'

                        cur_vars <- .self$get_variables()
                        if (is.null(cur_vars)){
                          var_num <- 0
                        } else {
                          var_num <- length(cur_vars)
                        }

                        if(!is.null(cur_vars) && variable_name %in% cur_vars){
                          message("There is already a variable defined with this name")
                        } else {
                          variables[[var_num+1]] <<- list(variable_name, variable_value)
                        }
                      },
                      remove_variable = function(variable_name){

                        'A method to remove one of the existing variables (constants) from a node, using the variable_name.'

                        cur_vars <- .self$get_variables()

                        if(is.null(cur_vars)){
                          message("This node has no variables")
                        } else {
                          for (i in seq_along(.self$variables)) {
                            if (.self$variables[[i]][[1]] == variable_name) {
                              variables <<- variables[-i]
                              message(variable_name, " has been removed from the node's variables")
                              break
                            } else {
                              message("This node does not have a variable called ", variable_name)
                            }
                          }
                        }
                      })
                    )

#' BN Network object
#'
#' These represent each network in a BN. Networks consist of nodes and
#' in a BN model there might be more than one network. These networks can
#' also be linked to each other with the use of input and output nodes. For such
#' links, see Model$networkLinks.
#'
#' @field id network id
#' @field name network display name
#' @field description network description
#' @field nodes list of nodes in the network
#'
#' @export
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

                           'Creates a new Network object, a unique id is required,
                           other fields are filled with defauls unless specified.'

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

                           'A method to plot the graphical structure of a BN network.'

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
                             message("There is already a node in the network with this ID")
                           } else {
                             nodes <<- append(nodes,newNode)
                             message(newNode$name, " is successfully added to the network")
                           }

                         },
                         remove_node = function(oldNode) {
                           'A method to remove an existing Node object from the network.
                           Note that removing a Node from a network does not automatically remove it from its
                           previous parent-child relationships in the network. You need to adjust such relationships
                           separately on the Node level.'
                           if (oldNode$id %in% .self$get_nodes()) {
                             for (i in seq_along(.self$nodes)) {
                               if (oldNode$id == .self$nodes[[i]]$id) {
                                 nodes <<- nodes[-i]
                                 break
                               }
                             }

                             message(oldNode$name, " is successfully removed from the network. If ", oldNode$name, " had any child nodes in the network, make sure to adjust their parents accordingly")
                           } else {
                             message("This node is not in the network")
                           }
                         })
                       )

#' Dataset
#'
#' @field id dataSet id
#' @field observations list ob observations.
#' @field results list of inference results.
#'
#' @export
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


#' BN Model object
#'
#' @field id model id
#' @field networks list of networks in the model
#' @field dataSets list of dataSets in the model
#' @field networkLinks list of network links in the model
#' @field settings list of settings of the model
#'
#' @export
Model <- setRefClass("Model",
                     fields = list(id = "character",
                                   networks = "list",
                                   dataSets = "list",
                                   networkLinks = "list",
                                   settings = "list"
                                   ),
                     methods = list(
                       show = function(){
                         cat(paste0("BN Model: \"",.self$id,"\""))
                         cat("\nNetworks in this model are:")
                         for (nt in .self$networks){
                           cat("\n-",nt$id)
                         }
                       },
                       initialize = function(id=NULL, networks, from_cmpx=FALSE, dataSets = NULL, networkLinks = NULL, settings = NULL){

                         'Creates a new Model object, a list of networks (at least one) is required.
                         id and settings are set with defaults unless specified.'
                         if(is.null(settings)){
                           .self$settings <<- list(parameterLearningLogging = FALSE,
                                                   discreteTails = FALSE,
                                                   sampleSizeRanked = 5,
                                                   convergence = 0.001,
                                                   simulationLogging = FALSE,
                                                   iterations = 50,
                                                   tolerance = 1)
                         } else {
                           .self$settings <<- settings
                         }

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
                         'A method to see ids of all the networks in a model.'
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
                         'A method to add a new Network object to the networks field of a Model object.
                         The input newNetwork is a Network object and it is added to the model if it is
                         not already in it.'
                         if (newNetwork$id %in% .self$get_networks()) {
                           message("There is already a network in the model with this ID")
                         } else {
                           networks <<- append(networks,newNetwork)
                           message(newNetwork$id, " is successfully added to the model")
                         }
                       },
                       remove_network = function(oldNetwork) {
                         'A method to remove an existing Network object from the model.
                         Note that removing a Node from a network does not automatically remove its possible network links
                         to other networks in the model. networkLinks field of a Model should be adjusted accordingly if needed.'
                         if (oldNetwork$id %in% .self$get_networks()) {
                           for (i in seq_along(.self$networks)) {
                             if (oldNetwork$id == .self$networks[[i]]$id) {
                               networks <<- networks[-i]
                               break
                             }
                           }

                           message(oldNetwork$id, " is successfully removed from the model. If ", oldNetwork$id, " had any links to other networks, make sure to adjust network links accordingly")
                         } else {
                           message("This network is not in the model")
                         }
                       },
                       add_network_link = function(source_network, source_node, target_network, target_node, link_type, pass_state=NULL){
                         'This is the method to add links to a model between its networks.
                         These links start from a "source node" in a network and go to a "target node" in another network.
                         To create the link, the source and target nodes in the networks need to be specified together
                         with the network they belong to (by the Node and Network ids). See README.md for details.'
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
                           message("Target node is a child node in its network, it cannot be a target node")
                         } else {
                           if (!is.null(targets) && target_node %in% targets) {
                             message("The required target node is already an target for another link")
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
                                   message("\nPlease enter the source node state to be passed on")
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
                               message("\nThe link between source node and target node is not valid")
                             }
                           }
                           }
                       },
                       remove_network_link = function(source_network,source_node,target_network,target_node){
                         'A method to remove network links, given the ids of the source and target nodes
                         (and the networks they belong to).'
                         if(length(.self$networkLinks)>0){
                           for (i in seq_along(.self$networkLinks)){
                             if(.self$networkLinks[[i]]$sourceNetwork == source_network &&
                                .self$networkLinks[[i]]$sourceNode == source_node &&
                                .self$networkLinks[[i]]$targetNetwork == target_network &&
                                .self$networkLinks[[i]]$targetNode == target_node) {
                               networkLinks <<- networkLinks[-i]
                               break
                             } else {
                               message("This network link is not in the model\n")
                             }
                           }
                         } else {
                           message("This model does not have any network links\n")
                         }
                       },
                       remove_all_network_links = function(){
                         'A method to remove all existing network links in a model.'
                         networkLinks <<- list()
                         message("All network links are removed from the model")
                       },
                       create_dataSet = function(id){
                         'It is possible to add multiple scenarios to a model. These scenarios are new DataSet
                         objects added to the dataSets field of a model. Initially these scenarios have no observations
                         and are only defined by their ids. The scenarios are populated with the enter_observation() function.'
                         dataSets <<- append(dataSets,Dataset$new(id=id, observations=NULL))
                         message("Dataset ",id," is added to the model")
                       },
                       import_results = function(result_file){
                         'A method to import results of a calculated dataSet from a json file. This correct format for
                         the results json file for this method is the file generated with the local agena.ai developer
                         API calculation (see README.md for details.'
                         dataset_import(.self, result_file)
                       },
                       get_dataSets = function(){
                         'A method to list the ids of all existing scenarios in a model.'
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
                       remove_dataSet = function(olddataSet){
                         'A method to remove an existing scenario from the model. Input parameter olddataSet
                         is the string which is the id of a dataset (scenario).'
                         if (olddataSet %in% .self$get_dataSets()) {
                           for (i in seq_along(.self$dataSets)) {
                             if (olddataSet == .self$dataSets[[i]]$id) {
                               dataSets <<- dataSets[-i]
                               break
                             }
                           }

                           message(olddataSet, " is successfully removed from the model's dataSets")
                         } else {
                           message("This dataSet is not in the model")
                         }
                       },
                       get_results = function(filename=NULL){
                         'A method to generate a .csv file based on the calculation results a Model contains.'
                         output_table <- generate_results_csv(.self)
                         if(!is.null(filename)){
                           file_name <- paste0(filename,".csv")
                         } else {
                           file_name <- paste0(.self$id,"_results.csv")
                         }
                         write.csv(output_table, file_name, row.names = FALSE)
                       },
                       enter_observation = function(dataSet=NULL, node, network, value, variable_input = FALSE, soft_evidence = FALSE){
                         'A method to enter observation to a model. To enter the observation to a specific dataset (scenario), the dataset id
                         must be given as the input parameter dateSet. If dataSet is left NULL, the entered observation will by default go to
                         "Scenario 1". This means that if there is no extra datasets created for a model (which by default comes with "Scenario 1"),
                         any observation entered will be set for this dataset. See README.md for details and examples.'
                         if(is.null(dataSet)){
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
                                 dataSets[[1]]$observations[[i]]$entries <<- new_obs$entries[1]
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


                           for (i in seq_along(dataSets)){
                             if(dataSet == dataSets[[i]]$id){

                               exist_check <- 0
                               if(length(dataSets[[i]]$observations)>0){
                                 for (j in seq_along(dataSets[[i]]$observations)){
                                   if(node == dataSets[[i]]$observations[[j]]$node && network == dataSets[[i]]$observations[[j]]$network){
                                     dataSets[[i]]$observations[[j]]$entries <<- new_obs$entries[1]
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
                       remove_observation = function(dataSet=NULL, node, network){
                         'A method to remove a specific observation from the model. It requires the id of the node which
                         has the observation to be removed and the id of the network the node belongs to.'
                         if(is.null(dataSet)){

                           for (i in seq_along(dataSets[[1]]$observations)){
                             if(node == dataSets[[1]]$observations[[i]]$node && network == dataSets[[1]]$observations[[i]]$network){
                               dataSets[[1]]$observations <<- dataSets[[1]]$observations[-i]
                               message("Observation is removed from the dataSet")
                               break
                             }
                           }
                         } else {
                           for (i in seq_along(dataSets)){
                             if(dataSet == dataSets[[i]]$id){
                               for (j in seq_along(dataSets[[i]]$observations)){
                                 if(node == dataSets[[i]]$observations[[j]]$node && network == dataSets[[i]]$observations[[j]]$network){
                                   dataSets[[i]]$observations <<- dataSets[[i]]$observations[-j]
                                   message("Observation is removed from the dataset")
                                   break
                                 }
                               }
                               }}
                         }
                       },
                       clear_dataSet_observations = function(dataSet){
                         'A method to clear all observations in a specific dataset (scenario) in the model.'
                         for (i in seq_along(.self$dataSets)){
                           if(dataSet == .self$dataSets[[i]]$id){
                             dataSets[[i]]$observations <<- list()
                             message("All observations are removed from the dataset ", dataSet)
                           }}
                       },
                       clear_all_observations = function(){
                         'A method to clear all observations defined in a model. This function removes all observations
                         from all datasets (scenarios).'
                         for (i in seq_along(.self$dataSets)){
                           dataSets[[i]]$observations <<- list()
                           message("All observations are removed from the model")
                         }
                       },
                       change_settings = function(settings){
                         'A method to change model settings. The input parameter settings must be a list with the correctly
                         named elements, see README.md for example.'
                         settings <<- settings
                         message("Model settings updated")
                       },
                       default_settings = function(){
                         'A method to reset model settings back to default values.'
                         settings <<- list(parameterLearningLogging = FALSE,
                                                 discreteTails = FALSE,
                                                 sampleSizeRanked = 5,
                                                 convergence = 0.001,
                                                 simulationLogging = FALSE,
                                                 iterations = 50,
                                                 tolerance = 1)
                         message("Model settings reset to default values")
                       },
                       to_cmpx = function(filename=NULL){
                         'A method to export the Model to a .cmpx file. This method passes on all the information about the model,
                         its datasets, its networks, their nodes, and model settings to a .cmpx file in the correct format
                         readable by agena.ai.
                         If the input parameter filename is not specified, it will use the Model$id for the filename.'
                           json_list <- generate_cmpx(.self)
                           json_object <- rjson::toJSON(json_list)

                         if(is.null(filename)){
                           file_name <- paste0(.self$id,".cmpx")
                         } else {
                           file_name <- paste0(filename,".cmpx")
                         }
                         write(json_object,file_name)
                       },
                       to_json = function(filename=NULL){
                         'A method to export the Model to a .json file. This method passes on all the information about the model,
                         its datasets, its networks, their nodes, and model settings to a .json file in the correct format
                         readable by agena.ai.
                         If the input parameter filename is not specified, it will use the Model$id for the filename.'
                           json_list <- generate_cmpx(.self)
                           json_object <- rjson::toJSON(json_list)

                         if(is.null(filename)){
                           file_name <- paste0(.self$id,".json")
                         } else {
                           file_name <- paste0(filename,".json")
                         }

                         write(json_object,file_name)
                       }
                     ))

#' Load a Model from CMPX
#'
#' This function reads an input agena.ai model file with the .cmpx extension
#' to create an R model object. The model object includes the networks and
#' the nodes of the imported model.
#'
#' @param modelPath Path to the input file
#' @returns An R model object
#' @export
from_cmpx <- function(modelPath){

  #read CMPX file, assign elements to R lists
  cmpx_input <- rjson::fromJSON(file=modelPath)

  cmpx_model <- cmpx_input$model
  cmpx_networks <- cmpx_model$networks
  cmpx_dataSets <- cmpx_model$dataSets
  cmpx_networkLinks <- cmpx_model$links
  cmpx_settings <- cmpx_model$settings

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

      if ((is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$simulated)) || (cmpx_networks[[i]]$nodes[[j]]$configuration$simulated == FALSE)) {
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
    if (!is.null(cmpx_dataSets[[i]]$results)) {
      datasets[[i]]$results <- cmpx_dataSets[[i]]$results
    }
  }


  outputModel <- Model$new(from_cmpx=TRUE,
                           networks = networks,
                           networkLinks = cmpx_networkLinks,
                           dataSets = datasets,
                           settings = cmpx_settings)

  return(outputModel)
}


#' Generate a CMPX file from R Model
#'
#' This function generates a .cmpx file for an agena.ai model
#' based on an R model object
#'
#' @param inputModel an R model object
#' @returns cmpx file
#' @export
generate_cmpx <- function(inputModel) {

  settings_list <- inputModel$settings

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

  res_list <- vector(mode = "list", length = length(inputModel$dataSets))
  for (i in seq_along(obs_list)){
    res_list[[i]] <- vector(mode = "list", length = length(inputModel$dataSets[[i]]$results))
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

  res_val_list <-  vector(mode = "list", length = length(inputModel$dataSets))
  if(length(inputModel$dataSets[[1]]$results)>0) {
    for (i in seq_along(res_val_list)) {
      res_val_list[[i]] <- vector(mode = "list", length = length(inputModel$dataSets[[i]]$results))
      for (j in seq_along(res_val_list[[i]])) {
        res_val_list[[i]][[j]] <- vector(mode = "list", length = length(inputModel$dataSets[[i]]$results[[j]]$resultValues))
      for (k in seq_along(res_val_list[[i]][[j]])) {
        res_val_list[[i]][[j]][[k]] <- inputModel$dataSets[[i]]$results[[j]]$resultValues[[k]]
      }}
    }
  }



  sum_stat_list <- vector(mode = "list", length = length(inputModel$dataSets))
  for (i in seq_along(sum_stat_list)) {
    sum_stat_list[[i]] <- vector(mode = "list", length = length(inputModel$dataSets[[i]]$results))
    for (j in seq_along(sum_stat_list[[i]])) {
      if(!is.null(inputModel$dataSets[[i]]$results[[j]]$summaryStatistics)){
        sum_stat_list[[i]][[j]] <- inputModel$dataSets[[i]]$results[[j]]$summaryStatistics
      }
    }
  }

  for (i in seq_along(inputModel$dataSets)) {
    if(length(inputModel$dataSets[[i]]$results)>0) {
      for (j in seq_along(inputModel$dataSets[[i]]$results)) {

        res_list[[i]][[j]] <- list(node = inputModel$dataSets[[i]]$results[[j]]$node,
                                   network = inputModel$dataSets[[i]]$results[[j]]$network)
        if(!is.null(inputModel$dataSets[[i]]$results[[j]]$summaryStatistics)) {
          res_list[[i]][[j]]$summaryStatistics <- sum_stat_list[[i]][[j]]
        }
        if(!is.null(inputModel$dataSets[[i]]$results)) {
          res_list[[i]][[j]]$resultValues <- res_val_list[[i]][[j]]
        }
      }
    }
  }

  for (i in seq_along(datasets_list)) {
    if(length(inputModel$dataSets[[i]]$results) > 0) {
      datasets_list[[i]]$results <- res_list[[i]]
    }

  }


  model_list <- list(settings = settings_list,
                     dataSets = datasets_list,
                     networks = networks_list,
                     links = networklinks_list)

  json_list <- list(model = model_list)
  return(json_list)

}

#' Create a json file of a BN model with observations
#'
#' This function takes an R Model objectand a CSV file of observations for cases
#' and creates a batch of datasets / cases for each row in the input data
#' and generates a .json file for the BN
#'
#' @param inputModel an R model object
#' @param inputData CSV file of observations
#' @returns json file
#' @export
#' @importFrom utils read.csv
create_batch_cases <- function(inputModel, inputData){

  inputTable <- read.csv(file=inputData, check.names = FALSE, na.strings = c(""))
  col_headers <- names(inputTable)[-1]
  obs_nodes <- c()
  obs_networks <- c()
  for (nm in col_headers){
    obs_nodes <- append(obs_nodes,gsub("\\..*", "", nm))
    obs_networks <- append(obs_networks,gsub(".*\\.", "", nm))
  }

  for (i in seq_along(inputTable)){
    temp_id <- as.character(as.character(inputTable[i,][[1]]))
    inputModel$create_dataSet(id = temp_id)
    for (j in seq_along(col_headers)){

      if(!is.na(inputTable[i,][[j+1]])){
        inputModel$enter_observation(dataSet = temp_id,
                                     node = obs_nodes[j], network = obs_networks[j],
                                     value = inputTable[i,][[j+1]])
      }

    }

  }
  filename <- paste0(inputModel$id, "_Batch_Cases")
  inputModel$to_json(filename = filename)

  for (i in seq_along(inputTable)){
    temp_id <- as.character(as.character(inputTable[i,][[1]]))
    inputModel$remove_dataSet(temp_id)
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

#' Create a CSV template for a model for batch observations
#'
#' This function creates an empty CSV file with the correct format so that it can
#' be filled in and used for create_batch_bases.
#'
#' @param inputModel an R model object
#' @returns csv file
#' @export
#' @importFrom utils write.table
create_csv_template <- function(inputModel){
  colname_list <- colname_list_generator(inputModel)
  filename <- paste0(inputModel$id,"_DataSet.csv")

  write.table(t(colname_list),sep = ",", file = filename, row.names = FALSE, col.names = FALSE)
}

generate_results_csv <- function(inputModel){

  results <- vector(mode = "list", length = length(inputModel$dataSets))

  for (i in seq_along(inputModel$dataSets)) {
    results[[i]] <- inputModel$dataSets[[i]]$results
  }

  first_column <- c()
  second_column <- c()
  third_column <- c()
  fourth_column <- c()
  fifth_column <- c()

  for (i in seq_along(inputModel$dataSets)){
    for (j in seq_along(inputModel$networks)){
      for (k in seq_along(inputModel$networks[[j]]$nodes)){
        for (l in seq_along(inputModel$dataSets)){
          for (m in seq_along(results[[i]])){
            if(inputModel$networks[[j]]$nodes[[k]]$id == results[[l]][[m]]$node &&
               inputModel$networks[[j]]$id == results[[l]][[m]]$network){
              for (n in seq_along(results[[l]][[m]]$resultValues)){
                first_column <- append(first_column, inputModel$dataSets[[i]]$id)
                second_column <- append(second_column, inputModel$networks[[j]]$id)
                third_column <- append(third_column, inputModel$networks[[j]]$nodes[[k]]$id)
                fourth_column <- append(fourth_column, results[[l]][[m]]$resultValues[[n]]$label)
                fifth_column <- append(fifth_column, results[[l]][[m]]$resultValues[[n]]$value)
              }
            }


          }
        }
      }}}

  output_table <- cbind(Scenario = first_column,
                Network = second_column,
                Node = third_column,
                State = fourth_column,
                Probability = fifth_column)
  return(output_table)

}

dataset_import <- function(inputModel, dataSet){

  input_results <- rjson::fromJSON(file = dataSet)
  results_id <- input_results[[1]]$id
  results <- input_results[[1]]$results

  for (i in seq_along(inputModel$dataSets)){
    if(inputModel$dataSets[[i]]$id == results_id){
      inputModel$dataSets[[i]]$results <- results
      message("Results in the file ",dataSet," are imported to the model dataset called ",results_id,"\n")
    }
  }
}


###### Agena AI Cloud Server Functionalities

### Login and Authentication

#' Login to agena.ai cloud
#'
#' @param username agena.ai cloud username
#' @param password agena.ai cloud password
#' @returns no return value, used in to authenticate user credentials
#' @export
#'
login <- function(username, password){

  auth_endpoint <- "https://auth.agena.ai/realms/cloud/protocol/openid-connect/token"
  body <- list(client_id = "agenarisk-cloud",
               username = username,
               password = password,
               grant_type = "password")

  response <- httr::POST(auth_endpoint, body = body, encode = "form")
  login_time <- as.integer(Sys.time())

  if(response$status_code == 200) {
    message("Authentication to Agena AI Cloud servers is successful\n")
    return(list(response, login_time))
  } else {
    message("Authentication failed\n")
    return(NULL)
  }

}


check_auth <- function(login) {

  'authentication checker function used in model operations such as calculate()'
  #if status == 200

  login_time <- login[[2]]
  access_duration <- httr::content(login[[1]])$expires_in
  access_expire <- login_time + access_duration

  refresh_duration <- httr::content(login[[1]])$refresh_expires_in
  refresh_expire <- login_time + refresh_duration

  if (as.integer(Sys.time()) < access_expire){
    status_check <- 0 #means the login is still active
  }
  if (as.integer(Sys.time()) > access_expire && as.integer(Sys.time()) < refresh_expire) {
    status_check <- 1 #means login has expired but refresh token is still active
  }
  if (as.integer(Sys.time() > refresh_expire)){
    status_check <- 2 #means login and refresh have expired
  }

  return(status_check)

}

refresh_auth <- function(cur_login){

  'authentication by refresh token. if the login access has expired but the refresh token has not
  at the time of calculation, authentication will be refreshed'

  auth_endpoint <- "https://auth.agena.ai/realms/cloud/protocol/openid-connect/token"
  cur_refresh_token <- httr::content(cur_login[[1]])$refresh_token
  body <- list(client_id = "agenarisk-cloud",
               refresh_token = cur_refresh_token,
               grant_type = "refresh_token")

  response <- httr::POST(auth_endpoint, body = body, encode = "form")
  login_time <- cur_login[[2]]

  return(list(response, login_time))
}

###### Calculation

calc_model <- function(input_model, cur_login, dataSet=NULL){

  'backend function to create POST request for model calculation'

  model_to_send <- generate_cmpx(input_model)

  if(!is.null(dataSet)) {
    for (i in seq_along(input_model$dataSets)) {
      if (input_model$dataSets[[i]]$id == dataSet) {
        obs_num <- length(model_to_send$model$dataSets[[i]]$observations)
        dataset_to_send <- model_to_send$model$dataSets[[i]]
        dataset_to_send$results <- NULL
        break
      } else {
        obs_num <- 0
        dataset_to_send <- NULL
      }
    }
  } else {
    obs_num <- length(model_to_send$model$dataSets[[1]]$observations)
    if (obs_num == 0) {
      dataset_to_send <- NULL
    } else {
      dataset_to_send <- model_to_send$model$dataSets[[1]]
      dataset_to_send$results <- NULL
    }
  }

  if (is.null(dataset_to_send) || obs_num == 0) {
    body <- list("sync-wait" = "true",
                 "model" = model_to_send$model)
  } else {
    body <- list("sync-wait" = "true",
                 "model" = model_to_send$model,
                 "dataSet" = dataset_to_send)
    }


  calculate_endpoint <- "https://api.agena.ai/public/v1/calculate"
  access_token <- httr::content(cur_login[[1]])$access_token


  response <- httr::POST(calculate_endpoint, body = body,
                   httr::add_headers("Authorization" = paste("Bearer",access_token)),
                   encode = "json", httr::accept_json())

  return(response)
}


#' Calculate posterior probabilities in a BN on agena.ai cloud
#'
#'A function to send an input Bayesian network model to Agena AI Cloud servers.
#'Once called, the function will check authentication status, if it has not expired,
#'it will send the POST request with the model to the servers, and receive calculation
#'results to update the Bayesian network model (filling the results field with calculation results).
#'
#' @param input_model an R model object
#' @param login an agena.ai cloud login
#' @param dataSet a dataSet in the R model object
#' @param debug boolean parameter to display debug messages or not
#'
#' @returns BN inference results in the model
#' @export
calculate <- function(input_model, login, dataSet=NULL, debug=FALSE) {

  if (check_auth(login) == 2){
    message("Authentication expired, please log in again")
    runfunc = FALSE
  }
  if (check_auth(login) == 1){
    runfunc = TRUE
    new_login <- refresh_auth(login)
    if(!is.null(dataSet)){
      response <- calc_model(input_model, new_login, dataSet)
    } else {
      response <- calc_model(input_model, new_login)
    }
  }
  if (check_auth(login) == 0){
    runfunc = TRUE
    if(!is.null(dataSet)){
      response <- calc_model(input_model, login, dataSet)
    } else {
      response <- calc_model(input_model, login)
    }
  }

  if(runfunc){
    #this function returns a Model object with results field filled in
    if (response$status_code == 200 && !is.null(httr::content(response)$results)) {
      if (!is.null(dataSet)) {
        for (i in seq_along(input_model$dataSets)) {
          if (input_model$dataSets[[i]]$id == dataSet) {
            input_model$dataSets[[i]]$results <- httr::content(response)$results
          }
        }
      } else {
        input_model$dataSets[[1]]$results <- httr::content(response)$results
      }
      message("Calculation successful, Model object now contains new results\n")

      if(debug){
        debug_messages = httr::content(response)$debug
        for (i in seq_along(debug_messages)){
          message(paste(debug_messages[[i]][[1]],":",debug_messages[[i]][[2]],"\n"))
        }
      }
    } else if (response$status_code == 202){
      message("Polling has started, polling for calculation results will update every 3 seconds\n")
      poll_url <- httr::content(response)$pollingUrl
      poll_status = 202

      while(poll_status==202){
        if (check_auth(login) == 0){
          access_token <- httr::content(login[[1]])$access_token
          polled_response <- httr::GET(poll_url, httr::add_headers("Authorization" = paste("Bearer",access_token)),
                                       encode = "json", httr::accept_json())
          poll_status <- polled_response$status_code
        } else if (check_auth(login) == 1){
          new_login <- refresh_auth(login)
          access_token <- httr::content(new_login[[1]])$access_token
          polled_response <- httr::GET(poll_url, httr::add_headers("Authorization" = paste("Bearer",access_token)),
                                       encode = "json", httr::accept_json())
          poll_status <- polled_response$status_code
        } else{
          message("Authentication expired, please log in again")
          poll_status = 0
        }
        Sys.sleep(3)
      }

      if(poll_status == 200){
        if (!is.null(dataSet)) {
          for (i in seq_along(input_model$dataSets)) {
            if (input_model$dataSets[[i]]$id == dataSet) {
              input_model$dataSets[[i]]$results <- httr::content(polled_response)$results
            }
          }
        } else {
          input_model$dataSets[[1]]$results <- httr::content(polled_response)$results
        }
        message("Calculation successful, Model object now contains new results\n")
      }

      if(debug){
        debug_messages = httr::content(polled_response)$debug
        for (i in seq_along(debug_messages)){
          message(paste(debug_messages[[i]][[1]],":",debug_messages[[i]][[2]],"\n"))
        }
      }

      } else {
      messages <- httr::content(polled_response)$messages
      for (i in seq_along(messages)){
        message(paste0(messages[[i]],"\n"))
      }

      if(debug){
        debug_messages = httr::content(polled_response)$debug
        for (i in seq_along(debug_messages)){
          message(paste(debug_messages[[i]][[1]],":",debug_messages[[i]][[2]],"\n"))
        }
      }
    }
  }

  return(input_model)

}

#' Create configuration object for sensitivity analysis
#'
#' See agena.ai manual for sensitivity analyis configuration
#'
#' @param target target node in the network
#' @param sensitivity_nodes a set of sensitivity nodes in the network
#' @param dataset a dataSet in the model
#' @param network a network in the model
#' @param report_settings a list of report settings
#'
#' @returns sensitivity config object
#' @export
create_sensitivity_config <- function(target, sensitivity_nodes, dataset = NULL, network= NULL,
                                      report_settings = NULL){

  sens_config <- list(targetNode = target,
                      sensitivityNodes = sensitivity_nodes)
  if(!is.null(network)) {
    sens_config$network <- network
  }
  if(!is.null(dataset)) {
    sens_config$dataSet <- dataset
  }
  if(!is.null(report_settings)) {
    sens_config$report_settings <- report_settings
  }

  return(sens_config)
}

analyse_sens <- function(input_model, cur_login, sens_config){

  sa_endpoint <- "https://api.agena.ai/public/v1/tools/sensitivity"

  model_to_send <- generate_cmpx(input_model)

  body <- list("sync-wait" = "true",
               "model" = model_to_send$model,
               "sensitivityConfig" = sens_config)

  access_token <- httr::content(cur_login[[1]])$access_token

  response <- httr::POST(sa_endpoint, body = body,
                         httr::add_headers("Authorization" = paste("Bearer",access_token)),
                         encode = "json", httr::accept_json())

  return(response)

}

#' Sensitivity analysis on agena.ai cloud
#'
#' @param input_model an R model object
#' @param login an agena.ai cloud login
#' @param sensitivity_config a sensitivity analysis config object
#' @param debug boolean parameter to display debug messages or not
#'
#' @returns sensitivity analysis report
#' @export
sensitivity_analysis <- function(input_model, login, sensitivity_config, debug=FALSE){

  if (check_auth(login) == 2){
    message("Authentication expired, please log in again")
    runfunc = FALSE
  }
  if (check_auth(login) == 1){
    runfunc = TRUE
    new_login <- refresh_auth(login)
    response <- analyse_sens(input_model, new_login, sensitivity_config)
  }
  if (check_auth(login) == 0){
    runfunc = TRUE
    response <- analyse_sens(input_model, login, sensitivity_config)
  }

  if(runfunc){
    if (response$status_code == 200 && !is.null(httr::content(response)$results)) {
      message("Sensitivity analysis finished\n")
      success_check = TRUE
      results <- httr::content(response)$results
      res_filename <- paste0(input_model$id,"_sens_results.json")
      write(rjson::toJSON(results), res_filename)
      message("A json file of the sensitivity analysis results, called \"", res_filename, "\" is created in the directory\n")

      if(debug){
        debug_messages = httr::content(response)$debug
        for (i in seq_along(debug_messages)){
          message(paste(debug_messages[[i]][[1]],":",debug_messages[[i]][[2]],"\n"))
        }
      }


    } else if (response$status_code == 202){
      messages <- httr::content(response)$messages
      for (i in seq_along(messages)){
        message(paste(messages[[i]],"\n"))
      }
      message("Polling has started, polling for calculation results will update every 3 seconds\n")
      poll_url <- httr::content(response)$pollingUrl
      poll_status = 202

      while(poll_status==202){
        if (check_auth(login) == 0){
          access_token <- httr::content(login[[1]])$access_token
          polled_response <- httr::GET(poll_url, httr::add_headers("Authorization" = paste("Bearer",access_token)),
                                       encode = "json", httr::accept_json())
          poll_status <- polled_response$status_code
        } else if (check_auth(login) == 1){
          new_login <- refresh_auth(login)
          access_token <- httr::content(new_login[[1]])$access_token
          polled_response <- httr::GET(poll_url, httr::add_headers("Authorization" = paste("Bearer",access_token)),
                                       encode = "json", httr::accept_json())
          poll_status <- polled_response$status_code
        } else{
          message("Authentication expired, please log in again")
          poll_status = 0
        }
        Sys.sleep(3)
      }

      if (poll_status == 200){
        results <- httr::content(polled_response)$results
        success_check = TRUE
        res_filename <- paste0(input_model$id,"_sens_results.json")
        write(rjson::toJSON(results), res_filename)
        message("A json file of the sensitivity analysis results, called \"", res_filename, "\" is created in the directory\n")

        if(debug){
          debug_messages = httr::content(polled_response)$debug
          for (i in seq_along(debug_messages)){
            message(paste(debug_messages[[i]][[1]],":",debug_messages[[i]][[2]],"\n"))
          }
        }
      } else{
        success_check = FALSE
        messages <- httr::content(polled_response)$messages
        for (i in seq_along(messages)){
          message(paste0(messages[[i]],"\n"))
        }

        if(debug){
          debug_messages = httr::content(polled_response)$debug
          for (i in seq_along(debug_messages)){
            message(paste(debug_messages[[i]][[1]],":",debug_messages[[i]][[2]],"\n"))
          }
        }

      }

    } else {
      success_check = FALSE
      messages <- httr::content(response)$messages
      for (i in seq_along(messages)){
        message(paste0(messages[[i]],"\n"))
      }

      if(debug){
        debug_messages = httr::content(response)$debug
        for (i in seq_along(debug_messages)){
          message(paste(debug_messages[[i]][[1]],":",debug_messages[[i]][[2]],"\n"))
        }
      }

    }
  }

  if(success_check){
    result_tables <- results$tables
    tables <- vector(mode="list", length=length(result_tables))

    for (i in seq_along(result_tables)){
      this_title <- result_tables[[i]]$title
      this_headers <- result_tables[[i]]$headerRow
      this_rows <- result_tables[[i]]$rows

      this_columns <- vector(mode = "list", length=length(this_headers))
      for (j in seq_along(this_columns)){
        for (k in seq_along(this_rows)){
          this_columns[[j]] <- append(this_columns[[j]], this_rows[[k]][[j]])
        }
      }

      this_table <- data.frame(row.names=seq_along(this_columns[[1]]))
      for (l in 1:length(this_headers)){
        this_table$new <- this_columns[[l]]
        colnames(this_table)[[l]] <- this_headers[[l]]
      }

      tables[[i]] <- this_table
      names(tables)[[i]] <- this_title
    }

    OUT <- openxlsx::createWorkbook()

    for (i in seq_along(tables)){
      openxlsx::addWorksheet(OUT, i)
    }

    for (i in seq_along(tables)){
      openxlsx::writeData(OUT, sheet = i, x = tables[[i]])
    }

    filename = paste0("sens_results_table_",input_model$id,".xlsx")

    if (file.exists(filename)){
      filename_new = paste0("sens_results_table_",input_model$id,"_",as.integer(Sys.time()),".xlsx")
      openxlsx::saveWorkbook(OUT, file = filename_new)
      message("The file \"", filename_new, "\" in the working directory contains report tables\n")
    } else {
      openxlsx::saveWorkbook(OUT, file = filename)
      message("The file \"", filename, "\" in the working directory contains report tables\n")
    }

  }

}

#' Local agena.ai API clone
#'
#' clones the local agena.ai developer API in the working directory
#' @returns no return value, used to clone local API to directory
#' @export
local_api_clone <- function(){
  git_api_http <- "https://github.com/AgenaRisk/api.git"
  system2("git", args=c("clone", git_api_http))
}

#' Local agena.ai API compilation
#'
#' sets the version and compiles the maven environment for
#' the local agena.ai developer API in the working directory
#' @returns no return value, used to compile maven in the local API directory
#' @export
local_api_compile <- function(){
  'currently not working as maven compiler requires the latest release
  of API and not the snapshot. and the latest release cannot be checked out
  through R'

  cur_wd <- getwd()
  on.exit(setwd(cur_wd))
  setwd("./api")

  system2("git", args="checkout master")
  system2("git", args="pull")

  latest_release <- system2("git", args="describe --tags --abbrev=0", stdout = TRUE)

  system2("git", args=c("checkout", latest_release))

  os <- Sys.info()[["sysname"]]

  if(os == "Windows"){
    system2("powershell", args=c("mvn","clean","compile"))
  }
  if(os == "Linux" || os == "Darwin"){
    system2("mvn", args=c("clean","compile"))
  }
  setwd(cur_wd)
}

#' Local agena.ai developer license activation
#'
#' sends the agena.ai developer license for activation
#'
#' @param key agena.ai developer license key
#' @returns no return value, activates user's local API license
#' @export
local_api_activate_license <- function(key){
  cur_wd <- getwd()
  on.exit(setwd(cur_wd))
  setwd("./api")

  os <- Sys.info()[["sysname"]]

  if(os == "Windows"){
    activate_command <- paste0("'-Dexec.args=\"--keyActivate --key ",key,"\"'")
    system2("powershell", args=c("mvn", "exec:java@activate", shQuote(activate_command)))
  }
  if(os == "Linux" || os == "Darwin"){
    activate_command <- paste0("-Dexec.args=\"--keyActivate --key ",key,"\"")
    system2("mvn", args=c("exec:java@activate", activate_command))
  }

  setwd(cur_wd)
}

#' Local agena.ai API model calculation
#'
#' @param model an R model object
#' @param dataSet a dataSet in the model
#' @param output file name for the output json - just the filename using the working directory, not a full file path
#'
#' @returns a json file for the model with results
#' @export
local_api_calculate <- function(model, dataSet, output){

  modelname <- paste0("local_",model$id)
  model$to_cmpx(filename=paste0(tempdir(),"/",modelname))

  model_to_send <- generate_cmpx(model)

  for (i in seq_along(model$dataSets)) {
    if (model$dataSets[[i]]$id == dataSet) {
      dataset_to_send <- model_to_send$model$dataSets[[i]]
      dataset_to_send$results <- NULL
      break
    } else {
      dataset_to_send <- NULL
    }
  }

  datasetname <- paste0("local_",dataSet,".json")
  write(rjson::toJSON(list(dataset_to_send)),paste0(tempdir(),"/",datasetname))

  cur_wd <- getwd()
  on.exit(setwd(cur_wd))
  os <- Sys.info()[["sysname"]]

  if(os == "Windows"){

    model_path <- paste0(tempdir(),"\\",modelname,".cmpx")
    dataset_path <- paste0(tempdir(),"\\",datasetname)
    output_path <- gsub("/","\\\\",paste0(cur_wd,"/",output))

    setwd("./api")
    calc_com <- paste0("\"-Dexec.args=`\"--model '",model_path,"' --out '",output_path,"' --data '",dataset_path,"'`\"\"")
    system2("powershell", args=c("mvn", "exec:java@calculate", shQuote(calc_com)))
    setwd(cur_wd)
  }

  if(os == "Linux" || os == "Darwin" ){

    model_path <- paste0(tempdir(),"/",modelname,".cmpx")
    dataset_path <- paste0(tempdir(),"/",datasetname)
    output_path <- paste0(cur_wd,"/",output)

    setwd("./api")
    calc_com <- paste0("-Dexec.args=\"--model '",model_path,"' --out '",output_path,"' --data '",dataset_path,"'\"")
    system2("mvn", args=c("exec:java@calculate", calc_com))
    setwd(cur_wd)
  }

  if (file.exists(output_path)){
    model$import_results(output_path)
    message("Calculation results are imported to the model object under relevant dataSet\n")
  }
}

#' Local agena.ai API sensitivity analysis
#'
#' @param model an R model object
#' @param sens_config a sensitivity config object
#' @param output file name for the output json - just the filename using the working directory, not a full file path
#'
#' @returns a json file report of the sensitivity analysis results
#' @export
local_api_sensitivity <- function(model, sens_config, output){

  modelname <- paste0("local_",model$id)
  model$to_cmpx(filename=paste0(tempdir(),"/",modelname))

  model_to_send <- generate_cmpx(model)

  if(!is.null(sens_config$dataSet)) {
    sens_config_name <- paste0("local_",sens_config$dataSet,"_sens_config.json")
  } else {
    sens_config_name <- paste0("local_no_data_sens_config.json")
  }
  write(rjson::toJSON(sens_config),paste0(tempdir(),"/",sens_config_name))

  cur_wd <- getwd()
  on.exit(setwd(cur_wd))
  os <- Sys.info()[["sysname"]]

  if(os == "Windows"){

    model_path <- paste0(tempdir(),"\\",modelname,".cmpx")
    sens_config_path <- paste0(tempdir(),"\\",sens_config_name)
    output_path <- gsub("/","\\\\",paste0(cur_wd,"/",output))

    setwd("./api")
    sens_com <- paste0("\"-Dexec.args=`\"--model '",model_path,"' --out '",output_path,"' --config '",sens_config_path,"'`\"\"")
    system2("powershell", args=c("mvn", "exec:java@sensitivity", shQuote(sens_com)))
    setwd(cur_wd)
  }

  if(os == "Linux" || os == "Darwin" ){
    model_path <- paste0(tempdir(),"/",modelname,".cmpx")
    sens_config_path <-  paste0(tempdir(),"/",sens_config_name)
    output_path <- paste0(cur_wd,"/",output)

    setwd("./api")
    sens_com <- paste0("-Dexec.args=\"--model '",model_path,"' --out '",output_path,"' --config '",sens_config_path,"'\"")
    system2("mvn", args=c("exec:java@sensitivity", sens_com))
    setwd(cur_wd)
  }
}


#' Local agena.ai API model calculation for batch cases
#'
#' Runs inference for every dataSet in the model
#'
#' @param model an R model object
#'
#' @returns a json file for the model with results
#' @export
local_api_batch_calculate <- function(model){

  for (ds in model$dataSets) {

    this_dataSet <- ds$id
    this_output <- paste0(model$id, "_", ds$id, "_results.json")
    local_api_calculate(model, this_dataSet, this_output)
    if (file.exists(this_output)) {
      model$import_results(this_output)
      unlink(this_output)
    } else {
      message("Error importing results from the file ",this_output,"\n")
    }
  }
  message("\nAll cases are calculated, the model object now contains results under its dataSets\n")
}
