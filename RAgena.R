
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
#variables: ***
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
                        if(!verbose){
                          cat(paste0("Bayesian Network Node: \"",.self$name,"\"\nUnique identifier: ",.self$id))
                          if(.self$simulated==TRUE){
                            cat("\nNode type:",.self$type,"(simulated)")
                          }else{
                            cat("\nNode type:",.self$type)
                          }
                        } else{
                          cat(paste0("Bayesian Network Node: \"",.self$name,"\"\nUnique identifier: ",.self$id))
                          if(.self$simulated==TRUE){
                            cat("\nNode type:",.self$type,"(simulated)")
                          }else{
                            cat("\nNode type:",.self$type)
                          }
                          if(length(.self$parents)==0){
                            cat("\nThe node has no parents.")
                          } else if(length(.self$parents)==1){
                            cat("\nParent node:",.self$parents[[1]]$name)
                          } else if(length(.self$parents)==2){
                            cat(paste0("\nParent nodes: ",.self$parents[[1]]$name,"; ",.self$parents[[2]]$name))
                          } else if(length(.self$parents)>2){
                            cat(paste0("\nParent nodes: ",.self$parents[[1]]$name,"; "))
                            for (pr in 2:(length(.self$parents)-1)){
                              cat(paste0(.self$parents[[pr]]$name,"; "))
                            }
                            cat(.self$parents[[length(.self$parents)]]$name)
                          }
                          cat("\nNPT type:",.self$distr_type)
                          if(.self$distr_type=="Manual"){
                          }
                        }
                      },
                      initialize = function(id,name=NULL,description=NULL,type=NULL,simulated=FALSE,states=NULL){
                        
                        'Creates a new Node object, a unique id is required, other fields are filled with defaults unless specified.
                        Node id, name, description, type, states, and whether it is a simulation or regular node do not depend on its edges and parents in a network,
                        a Node object can be defined with all this information outside a Network as well.
                        To add/remove/modify parents, distr_type, probabilities, expressions, and partitions; use the corresponding method.'
                        
                        #assigning $id - mandatory input to create a new Node object
                        .self$id <<- id
                        
                        #assigning $name - input name if given, id otherwise
                        if(is.null(name)){
                          .self$name <<- id
                        } else{
                          .self$name <<- name
                        }
                        
                        #assigning $description - input desc if given, New Node Object otherwise
                        if(is.null(description)){
                          .self$description <<- "New Node"
                        } else {
                          .self$description <<- description
                        }
                        
                        #setting $simulated - false by default
                        if(simulated){
                          .self$simulated <<- TRUE
                        } else {
                          .self$simulated <<- FALSE
                        }
                        
                        #setting $type - Boolean by default, input type if given correctly
                        if(.self$simulated){
                          if(is.null(type)){
                            .self$type <<- "ContinuousInterval"
                          } else {
                            if(type == "IntegerInterval"){
                              .self$type <<- type
                            } else {
                              .self$type <<- "ContinuousInterval"
                            }
                          }
                        } else {
                          if(is.null(type)){
                            .self$type <<- "Boolean"
                          } else {
                            if(type=="Labelled" || type=="Ranked" || type=="DiscreteReal" || type=="ContinuousInterval" || type=="IntegerInterval") {
                              .self$type <<- type
                            } else{
                              .self$type <<- "Boolean"
                            }
                          }
                        }

                        #setting $states - null if simulated, depends on type if not simulated
                        if(.self$simulated){
                          .self$states <<- character(0)
                        } else {
                          if(is.null(states)){ #if no states given, set defaults based on type
                            if(.self$type == "Boolean"){
                              .self$states <<- c("False", "True") #Boolean type default states
                            } else if(.self$type == "Ranked"){
                              .self$states <<- c("Low", "Medium", "High") #Ranked type default states
                            } else if(.self$type == "Labelled"){
                              .self$states <<- c("False", "True") #Labelled type default states
                            } else if(.self$type =="DiscreteReal"){
                              .self$states <<- c("0.0","1.0") #DiscreteReal type default states
                            }
                          } else { #if input states given, check if it's fine based on type
                            if(.self$type == "Boolean"){
                              if(length(states) == 2){ #if Boolean and input states have two names, use them
                                .self$states <<- states 
                              } else { #if Boolean and input states do not have two names, use default
                                .self$states <<- c("False", "True")
                              }
                            } else if (.self$type == "Ranked" || .self$type == "Labelled" || .self$type == "DiscreteReal"){
                              .self$states <<- states
                            } else {
                              .self$states <<- NULL
                            }
                          } 
                        }
                        
                        #sensible defaults are assigned to probabilitiess/expressions which will be rewritten with the specific methods
                        if(.self$simulated){
                          .self$distr_type <<- "Expression"
                          .self$expressions <<- "Normal(0,1000000)"
                        } else {
                          if(.self$type == "ContinuousInterval" || .self$type == "IntegerInterval"){
                            .self$distr_type <<- "Expression"
                            .self$expressions <<- "Normal(0,1000000)"
                          } else {
                            .self$distr_type <<- "Manual"
                            .self$probabilities <<- vector(mode = "list", length = length(.self$states))
                            for (i in 1:length(.self$probabilities)){
                              probabilities[[i]] <<- 1/length(.self$probabilities)
                            }
                          }
                        }
                      },
                      getParents = function(){
                        parList <- c()
                        if(length(.self$parents)>0){
                          for (i in 1:length(.self$parents)) {
                            parList[i] <- .self$parents[[i]]$id
                          }
                        }
                        else{
                          parList <- NULL
                        }
                        return(parList)
                      },
                      addParent = function(newParent){
                        'Adds a Node object as a new parent node to the current Node object and resets/resizes the NPT values and expressions of the Node as needed.
                        Parents list of a Node object is a list of other Node objects.
                        The input parameter of the function is a Node object variable. A good practice is to use Node ids as their variable names.'
                        temp_par_list <- c()
                        for (pr in .self$parents){
                          temp_par_list <- append(temp_par_list,pr$id)
                        }
                        if (!(newParent$id %in% temp_par_list)){
                          parents <<- append(parents,newParent)
                        }
                        
                        if(.self$distr_type == "Manual"){
                          #update probabilities when parent added (reset to uniform with correct number of values)
                          updated_probs <- vector(mode = "list",length = length(.self$states))
                          temp_length <- 1
                          if(length(.self$parents)>0){
                            for (prt in .self$parents){
                              temp_length <- temp_length * length(prt$states)
                            }
                          }
                          
                          .self$setProbabilities(updated_probs)
                          for (i in 1:length(.self$probabilities)){
                            probabilities[[i]] <<- rep(1/length(.self$probabilities),temp_length)
                          }
                          
                          cat("Node",newParent$id,"has been added to the parents list of",.self$id,"\nNPT values for",.self$id,"are reset to uniform\n")
                        } else {
                          cat("Node",newParent$id,"has been added to the parents list of",.self$id,"\nNow you can use",newParent$id,"in the expression of",.self$id,"\n")
                        }
                      },
                      addParent_byID = function(newParentID, varList){
                        'This is a method to add parent Nodes by their ids for cmpx parser capabilities.
                        To add parents to Node objects, please use $addParent(Node) method.'
                        
                        for (i in 1:length(varList)){
                          if(newParentID == varList[[i]]$id){
                            .self$addParent(varList[[i]])
                          }
                        }
                        #This does not do any smart adjustments to NPTs/expressions - only used in cmpx parser
                      },
                      removeParent = function(oldParent){
                        'Removes a Node object from parents of the current Node object and resets/resizes the NPT values and expressions of the Node as needed.
                        The input parameter of the function is a Node object variable. A good practice is to use Node ids as their variable names.'
                        if(oldParent$id %in% .self$getParents()){
                          for (i in 1:length(.self$parents)){
                            if(oldParent$id == .self$parents[[i]]$id){
                              parents <<- .self$parents[-i]
                              break
                            }
                          }
                        }
                        
                        if(.self$distr_type == "Manual"){
                          updated_probs <- vector(mode = "list",length = length(.self$states))
                          temp_length <- 1
                          if(length(.self$parents)>0){
                            for (prt in .self$parents){
                              temp_length <- temp_length * length(prt$states)
                            }
                          }
                          
                          .self$setProbabilities(updated_probs)
                          for (i in 1:length(.self$probabilities)){
                            probabilities[[i]] <<- rep(1/length(.self$probabilities),temp_length)
                          }
                          cat("Node",oldParent$name,"has been removed from the parents list of",.self$name,"\nNPT values for",.self$name,"are reset to uniform\n")
                        } else if (.self$distr_type == "Partitioned") {
                          if(oldParentID %in% .self$partitions){
                            partitions <<- partitions[partitions != oldParentID]
                            temp_length <- 1
                            if(length(.self$partitions)>0){
                              for (pt in .self$partitions){
                                for (prt in .self$parents){
                                  if(pt == prt$id){
                                    temp_length <- temp_length * length(prt$states)
                                  }
                                }
                              }
                            }
                            expressions <<- rep("Normal(0,1000000)",temp_length)
                            cat("Node",oldParent$name,"has been removed from the parents list of",.self$name,"\nPartitioned expressions for",.self$name,"are reset to Normal distribution\n")
                          } else {
                            cat("Node",oldParent$name,"has been removed from the parents list of",.self$name,"\n")
                          }
                        } else {
                          expressions <<- "Normal(0,1000000)"
                          cat("Node",oldParent$name,"has been removed from the parents list of",.self$name,"\nExpression for",.self$name,"is reset to Normal distribution\n")
                        }
                      },
                      setDistributionType = function(new_distr_type){
                        'A method to set the distribution type of a Node for the table configurations.'
                        
                        if(.self$simulated){
                          if(new_distr_type == "Partitioned" && length(.self$parents) > 0){
                              distr_type <<- "Partitioned" ######if successfully changed to Partitioned, we need to reset temp default expressions
                            } else {
                              distr_type <<- "Expression"
                              cat("Node",.self$id,"has no parents. Distribution type is set to Expression instead.\n")
                            }
                        } else{
                          if(new_distr_type == "Manual" || new_distr_type == "Expression"){
                            distr_type <<- new_distr_type
                          } else if(new_distr_type == "Partitioned"){ ######if successfully changed to Partitioned, we need to reset temp default expressions
                              if(length(.self$parents > 0)){ 
                                distr_type <<- new_distr_type
                              } else { 
                                distr_type <<- "Expression" #if Node has no parents, do not allow Partitioned
                                cat("Node",.self$id,"has no parents. Distribution type is set to Expression instead.\n")
                              }
                            } else {
                              distr_type <<- "Manual" #if incorrect input, set it to default Manual
                              cat("Incorrect input. Distribution type is set to Manual instead.\n")
                            }
                        }
                      },
                      setProbabilities = function(new_probs, by_rows=TRUE){
                        if(by_rows){
                          if(!.self$simulated && .self$distr_type == "Manual" && (.self$type != "ContinuousInterval" || .self$type != "IntegerInterval")){
                            if(length(new_probs) == length(.self$states)){
                              temp_length <- 1
                              if(length(.self$parents)>0){
                                for (prt in .self$parents){
                                  temp_length <- temp_length * length(prt$states)
                                }
                              }
                              
                              subset_length_control <- 1
                              for (subset in new_probs){
                                if(length(subset) == temp_length){
                                  subset_length_control <- subset_length_control * 1
                                } else {
                                  subset_length_control <- subset_length_control * 0
                                }
                              }
                              
                              ######need another control to check probability sums are 1
                              if(subset_length_control == 1){
                                probabilities <<- new_probs
                              }
                            }
                          }
                        } else {
                          if(!.self$simulated && .self$distr_type == "Manual" && (.self$type != "ContinuousInterval" || .self$type != "IntegerInterval")){
                            temp_length <- 1
                            if(length(.self$parents)>0){
                              for (prt in .self$parents){
                                temp_length <- temp_length * length(prt$states)
                              }
                            }
                            
                            if(length(new_probs)==temp_length){
                              subset_length_control <- 1
                              for (subset in new_probs){
                                if(length(subset)==length(.self$states)){
                                  subset_length_control <- subset_length_control * 1
                                } else {
                                  subset_length_control <- subset_length_control * 0
                                }
                              }
                            }
                            
                            if(subset_length_control == 1){
                              for (i in 1:length(new_probs)){
                                for (j in 1:length(new_probs[[i]])){
                                  probabilities[[j]][[i]] <<- new_probs[[i]][[j]] 
                                }
                              }
                            }
                          }
                        }
                      },
                      setExpressions = function(new_expr,partition_parents=NULL){
                        if(!is.null(partition_parents)){
                          partitions <<- partition_parents ######need to check these are in Node's parents list
                          expressions <<- new_expr ######need to check length of exprs is equal to parent_states_product
                        } else {
                            expressions <<- new_expr ######need to make sure length of exprs is 1 if not partitioned
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
                         show = function(){
                           cat(paste0("Bayesian Network: \"",.self$name,"\"\nID: ",.self$id))
                           cat(paste0("\nNodes in the network: ",.self$nodes[[1]]$name,"; "))
                           for (nd in 2:(length(.self$nodes)-1)){
                             cat(paste0(.self$nodes[[nd]]$name,"; "))
                           }
                           cat(.self$nodes[[length(.self$nodes)]]$name)
                         },
                         initialize = function(id,name=NULL,description=NULL,nodes=NULL){
                           
                           #assigning $id - mandatory input to create a new Network object
                           .self$id <<- id
                           
                           #assigning $name - input name if given, id otherwise
                           if(is.null(name)){
                             .self$name <<- id
                           } else{
                             .self$name <<- name
                           }
                           
                           #assigning $description - input desc if given, New Node Object otherwise
                           if(is.null(description)){
                             .self$description <<- "New Network"
                           } else {
                             .self$description <<- description
                           }
                           
                           if(!is.null(nodes)){
                             .self$nodes <<- nodes
                           }
                         },
                         getNodes = function(){
                           nodeList <- c()
                           if(length(.self$nodes)>0){
                             for (i in 1:length(.self$nodes)) {
                               nodeList[i] <- .self$nodes[[i]]$id
                             }
                           }
                           else{
                             nodeList <- NULL
                           }
                           return(nodeList)
                         },
                         
                         addNode = function(newNode){
                           'A method to add new Node objects to a Network.
                           Note that adding a new Node to the network does not automatically add its parents to the network.
                           You need to add all the parents separately too.'
                           
                           if(newNode$id %in% .self$getNodes()){
                             cat("There is already a node in the network with this ID")
                           } else {
                             nodes <<- append(nodes,newNode)
                             cat(newNode$name,"is successfully added to the network")
                           }
                           
                         },
                         removeNode = function(oldNode){
                           'remove Node from Network'
                           if(oldNode$id %in% .self$getNodes()){
                             for (i in 1:length(.self$nodes)){
                               if(oldNode$id == .self$nodes[[i]]$id){
                                 nodes <<- nodes[-i]
                                 break
                               }
                             }

                             cat(oldNode$name,"is successfully removed from the network. If",oldNode$name,"had any child nodes in the network, make sure to adjust their parents accordingly")
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
                                results = "list")) #these Dataset objects will be compatible with a reasonable data input file / csv to create them


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
                       initialize = function(id=NULL, networks, dataSets=NULL, networkLinks=NULL){
                         if(!is.null(networkLinks)){
                           .self$networkLinks <<- networkLinks
                         }
                         if(!is.null(dataSets)){
                           .self$dataSets <<- dataSets
                         }
                         if(is.null(id)){
                           .self$id <<- paste(networks[[1]]$id,"Model")
                         } else{
                           .self$id <<- id
                         }
                         .self$networks <<- networks
                       },
                       addNetworkLink = function(outNetwork,outNode,inNetwork,inNode,linkType){
                         #check if both nodes are the same type and either of them is simulated
                         #check if both nodes are the same type and neither is simulated and both have the same number of states
                         #check sourcenode is not numeric interval or discrete real and target node is simulated
                         
                         #type in c("Marginals", "Mean", "Median", "Variance", "StandardDeviation", "LowerPercentile", "UpperPercentile", "State")
                         #if type state, passState string must appear
                       },
                       to_cmpx = function(){
                         json_object <- generate_cmpx(.self)
                         fileName <- paste0(.self$id,".json")
                         write(json_object,fileName)
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
  observations <- vector(mode = "list",length = length(cmpx_dataSets))
  results <- vector(mode = "list",length = length(cmpx_dataSets))
  
  #filling in the list of Network objects with each network in the CMPX model
  for (i in 1:length(cmpx_networks)){
    networks[[i]] <- Network$new(id = cmpx_networks[[i]]$id,
                                 name = cmpx_networks[[i]]$name,
                                 description = cmpx_networks[[i]]$description)
    

    #filling in the list of Node objects with each node of each network
    #keep in mind this list is two dimensional, each list element is a list of Nodes itself
    for (j in 1:length(cmpx_networks[[i]]$nodes)){
      nodes[[i]][[j]] <- Node$new(id = cmpx_networks[[i]]$nodes[[j]]$id,
                                  name = cmpx_networks[[i]]$nodes[[j]]$name,
                                  description = cmpx_networks[[i]]$nodes[[j]]$description,
                                  type = cmpx_networks[[i]]$nodes[[j]]$configuration$type)
                                  
      if(is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$simulated)){
        nodes[[i]][[j]]$simulated <- FALSE
      } else{
        nodes[[i]][[j]]$simulated <- TRUE
      }
      
      nodes[[i]][[j]]$distr_type = cmpx_networks[[i]]$nodes[[j]]$configuration$table$type
      
      if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$states)){
        nodes[[i]][[j]]$states <- cmpx_networks[[i]]$nodes[[j]]$configuration$states
        nodes[[i]][[j]]$probabilities <- vector(mode = "list", length = length(nodes[[i]][[j]]$states))
        for (k in 1:length(nodes[[i]][[j]]$states)){
          nodes[[i]][[j]]$probabilities[[k]] <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities[[k]]
        }
      }

      if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions)){
        nodes[[i]][[j]]$expressions <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions
      }
      
      if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$partitions)){
        nodes[[i]][[j]]$partitions <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$partitions
      }
    }
    
    networks[[i]]$nodes <- nodes[[i]]
    links[[i]] <- cmpx_networks[[i]]$links
  }
  
  for (i in 1:length(networks)) {
    for (j in 1:length(networks[[i]]$nodes)){
      for (k in 1:length(links[[i]])){
        if (links[[i]][[k]]$child == networks[[i]]$nodes[[j]]$id){
          networks[[i]]$nodes[[j]]$addParent_byID(links[[i]][[k]]$parent,networks[[i]]$nodes)
        }
      }
    }
  }
  
  for (i in 1:length(cmpx_dataSets)){
    datasets[[i]] <- Dataset$new(id = cmpx_dataSets[[i]]$id)
    
    datasets[[i]]$observations <- cmpx_dataSets[[i]]$observations
  }
  
  outputModel <- Model$new(networks = networks,
                           networkLinks = cmpx_networkLinks,
                           dataSets = datasets)
  
  return(outputModel)
}






generate_cmpx <- function(inputModel){

  #creating empty lists and sublists with correct length to be filled in later
  networks_list <- vector(mode = "list", length = length(inputModel$networks))
  
  nodes_list <- vector(mode = "list", length = length(inputModel$networks))
  for (i in 1:length(nodes_list)) {
    nodes_list[[i]] <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes))
  }
  
  config_list <- vector(mode = "list", length = length(inputModel$networks))
  for (i in 1:length(config_list)) {
    config_list[[i]] <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes))
  }
  
  table_list <- vector(mode = "list", length = length(inputModel$networks))
  for (i in 1:length(table_list)) {
    table_list[[i]] <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes))
  }
  
  links_list <- vector(mode = "list", length = length(inputModel$networks))
  for (i in 1:length(nodes_list)) {
    links_amount <- 0
    for (nd in inputModel$networks[[i]]$nodes){
      links_amount <- links_amount + length(nd$parents)
    }
    links_list[[i]] <- vector(mode = "list", length = links_amount)
  }
  
  
  #table_list generation
  for (i in 1:length(table_list)){
    for (j in 1:length(table_list[[i]])){
      table_list[[i]][[j]] <- list(type = inputModel$networks[[i]]$nodes[[j]]$distr_type)
      if(inputModel$networks[[i]]$nodes[[j]]$distr_type == "Manual"){
        #table_list[[i]][[j]]$probabilities <- inputModel$networks[[i]]$nodes[[j]]$probabilities
        temp_probs_list <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes[[j]]$probabilities))
        for (k in 1:length(temp_probs_list)){
          temp_probs_sublist <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes[[j]]$probabilities[[k]]))
          for (m in 1:length(temp_probs_sublist)){
            temp_probs_list[[k]][m] <- inputModel$networks[[i]]$nodes[[j]]$probabilities[[k]][m]
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
        for (k in 1:length(temp_part_list)){
          temp_part_list[k] <- inputModel$networks[[i]]$nodes[[j]]$partitions[[k]]
        }
        for (k in 1:length(temp_exp_list)){
          temp_exp_list[k] <- inputModel$networks[[i]]$nodes[[j]]$expressions[[k]]
        }
        table_list[[i]][[j]]$partitions <- temp_part_list
        table_list[[i]][[j]]$expressions <- temp_exp_list
      }
    }
  }
  

  for (i in 1:length(config_list)){
    for (j in 1:length(config_list[[i]])){
      config_list[[i]][[j]] <- list(type = inputModel$networks[[i]]$nodes[[j]]$type,
                                    simulated = inputModel$networks[[i]]$nodes[[j]]$simulated,
                                    input = FALSE,
                                    output = FALSE,
                                    table = table_list[[i]][[j]]
                                    )
      if(!inputModel$networks[[i]]$nodes[[j]]$simulated && inputModel$networks[[i]]$nodes[[j]]$distr_type == "Manual"){
        temp_states_list <- vector(mode = "list", length = length(inputModel$networks[[i]]$nodes[[j]]$states))
        for (k in 1:length(inputModel$networks[[i]]$nodes[[j]]$states)){
          temp_states_list[k] <- inputModel$networks[[i]]$nodes[[j]]$states[k]
        }
        config_list[[i]][[j]]$states <- temp_states_list
      }
    }
  } #still needs variables <- vars_list
  

  
  for (i in 1:length(nodes_list)) {
    for (j in 1:length(nodes_list[[i]])){
      nodes_list[[i]][[j]] <- list(id = inputModel$networks[[i]]$nodes[[j]]$id,
                                   name = inputModel$networks[[i]]$nodes[[j]]$name,
                                   description = inputModel$networks[[i]]$nodes[[j]]$description,
                                   configuration = config_list[[i]][[j]])
    }
  }
  
  temp_parents_list <- list()
  temp_children_list <- list()
  
  for (i in 1:length(inputModel$networks)){
    temp_parents_list[[i]] <- list()
    temp_children_list[[i]] <- list()
    for (j in 1:length(inputModel$networks[[i]]$nodes)){
      if(length(inputModel$networks[[i]]$nodes[[j]]$parents) != 0){
        for (k in 1:length(inputModel$networks[[i]]$nodes[[j]]$parents)){
          temp_parents_list[[i]] <- append(temp_parents_list[[i]], inputModel$networks[[i]]$nodes[[j]]$parents[[k]]$id)
          temp_children_list[[i]] <- append(temp_children_list[[i]], inputModel$networks[[i]]$nodes[[j]]$id)
        }
      }
    }
  }
  
  
  for (i in 1:length(links_list)) {
    for (j in 1:length(links_list[[i]])){
      links_list[[i]][[j]] <- list(parent = temp_parents_list[[i]][[j]],
                                   child = temp_children_list[[i]][[j]])
    }
  }
  
      

  for (i in 1:length(networks_list)) {
    networks_list[[i]] <- list(id = inputModel$networks[[i]]$id,
                               name = inputModel$networks[[i]]$name,
                               description = inputModel$networks[[i]]$description,
                               nodes = nodes_list[[i]],
                               links = links_list[[i]])
  }
  
  settings_list <- list(parameterLearningLogging = FALSE,
                        discreteTails = FALSE,
                        sampleSizeRanked = 5,
                        convergence = 0.001,
                        simulationLogging = FALSE,
                        iterations = 50,
                        tolerance = 1)

  if(length(inputModel$networkLinks) == 0){
    networklinks_list <- list()
  } else {
    networklinks_list <- inputModel$networkLinks
  }
  
  # networklinks_list <- list(sourceNetwork = "placeholder",
  #                           sourceNode = "placeholder",
  #                           targetNetwork = "placeholder",
  #                           targetNode = "placeholder",
  #                           type = "placeholder",
  #                           passState = "placeholder")
  
  model_list <- list(settings = settings_list,
                     networks = networks_list,
                     links = networklinks_list)
  
  json_list <- list(model = model_list)
  json_object <- rjson::toJSON(json_list)
  #write(json_object,"r-bayesian-model2.json")

}



