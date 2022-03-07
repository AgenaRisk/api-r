
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
                      show = function(){
                        cat("Bayesian Network Node: \"",.self$name,"\"\nID:",.self$id,"\nDescription:",.self$description)
                        if(.self$simulated==TRUE){
                          cat("\nType:",.self$type,"(simulated)")
                        }else{
                          cat("\nType:",.self$type)
                        }
                        cat("\nPrior distribution type:",.self$distr_type)
                        
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
                      addParent = function(newParent){
                        'Adds a Node object as a new parent node to the current Node object.
                        Parents list of a Node object is a list of other Node objects.
                        Variable name is used as a reference to the Node to be added as a parent here.
                        A good practice is to use Node ids as their variable names.'
                        temp_par_list <- c()
                        for (pr in .self$parents){
                          temp_par_list <- append(temp_par_list,pr$id)
                        }
                        if (!(newParent$id %in% temp_par_list)){
                          parents <<- append(parents,newParent)
                        }
                        
                        #need to resize / reset size of probs/exprs table when parent added

                      },
                      addParent_byID = function(newParentID, varList){
                        'A method to add parent Nodes by their ids mainly for cmpx parser capabilities.
                        To add parents to Node objects, the use of $addParent(Node) is recommended.'
                        
                        for (i in 1:length(varList)){
                          if(newParentID == varList[[i]]$id){
                            .self$addParent(varList[[i]])
                          }
                        }
                        
                        ######need to resize / reset size of probs/exprs table when parent added
                      },
                      removeParent = function(oldParentID){
                        'A method to remove one of the existing parents of a Node object.
                        Current parent Nodes are referred to by their ids.'
                        for (i in 1:length(.self$parents)){
                          if(oldParentID == .self$parents[[i]]$id){
                            .self$parents <<- .self$parents[-i]
                          }
                        }
                        
                        ######need to resize / reset size of probs/exprs table when parent removed
                      },
                      setDistributionType = function(new_distr_type){
                        'A method to set the distribution type of a Node for the table configurations.'
                        
                        if(.self$simulated){
                          if(new_distr_type == "Partitioned" && length(.self$parents) > 0){
                              distr_type <<- "Partitioned" ######if successfully changed to Partitioned, we need to reset temp default expressions
                            } else {
                              distr_type <<- "Expression"
                            }
                        } else{
                          if(new_distr_type == "Manual" || new_distr_type == "Expression"){
                            distr_type <<- new_distr_type
                          } else if(new_distr_type == "Partitioned"){ ######if successfully changed to Partitioned, we need to reset temp default expressions
                              if(length(.self$parents > 0)){ 
                                distr_type <<- new_distr_type
                              } else { 
                                distr_type <<- "Expression" #if Node has no parents, do not allow Partitioned
                              }
                            } else {
                              distr_type <<- "Manual" #if incorrect input, set it to default Manual
                            }
                        }
                        
                        #####need to re-adjust probabilities / expressions etc. if the distr_type is changed
                      },
                      setProbabilities = function(new_probs){
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
                           cat("Bayesian Network: \"",.self$name,"\"\nID:",.self$id,"\nDescription:",.self$description)
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
                         addNode = function(newNode){
                           'add new Node to Network'
                         },
                         removeNode = function(oldNode){
                           'remove Node from Network'
                           ######need to make sure this is in sync with Node$parents, when a Node is removed from Network what happens to its parents/children?
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
                     fields = list(networks = "list",
                                   dataSets = "list",
                                   networkLinks = "list"
                                   )) #still needs the show() function

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
    links[[i]] <- cmpx_networks[[1]]$links
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




