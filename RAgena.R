
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
                                  parents = "character",
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
                      addParent = function(newParent){
                        if(!(newParent %in% parents)){
                          parents <<- append(parents,newParent)
                        }
                        
                      })
                    )


#Network object as an R reference class
Network <- setRefClass("Network",
                       fields = list(id = "character",
                                     name = "character",
                                     description = "character",
                                     nodes = "list",
                                     links = "list"),
                       methods = list(
                         show = function(){
                           cat("Bayesian Network: \"",.self$name,"\"\nID:",.self$id,"\nDescription:",.self$description)
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

#function to read input CMPX file to create BNModel and its BNNodes
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
  
  #filling in the list of Network objects with each network in the CMPX model, ID is required
  for (i in 1:length(cmpx_networks)){
    networks[[i]] <- Network$new(id = cmpx_networks[[i]]$id)
    
    #if name and description are given, they're parsed for corresponding Network object attributes. if not, name is same as ID and description is left blank
    
    #Network$name
    if(!is.null(cmpx_networks[[i]]$name)){
      networks[[i]]$name <- cmpx_networks[[i]]$name
    } else {
      networks[[i]]$name <- cmpx_networks[[i]]$id #if there's no network name, Network$name is same as Network$id
    }
    #Network$description
    if(!is.null(cmpx_networks[[i]]$description)){
      networks[[i]]$description <- cmpx_networks[[i]]$description
    }
    
    #filling in the list of Node objects with each node of each network, ID is required
    #keep in mind this list is two dimensional, each list element is a list of Nodes itself
    for (j in 1:length(cmpx_networks[[i]]$nodes)){
      nodes[[i]][[j]] <- Node$new(id = cmpx_networks[[i]]$nodes[[j]]$id,
                                  description = cmpx_networks[[i]]$nodes[[j]]$description)
      
      #if the following optional attributes are given, they're parsed for corresponding Node object attributes. if not, corresponding Node attribute is left blank
      
      #Node$name
      if(!is.null(cmpx_networks[[i]]$nodes[[j]]$name)){
        nodes[[i]][[j]]$name <- cmpx_networks[[i]]$nodes[[j]]$name
      }else{
        nodes[[i]][[j]]$name <- cmpx_networks[[i]]$nodes[[j]]$id #if there's no node name, Node$name is same as Node$id
      }
      
      #Node$description
      if(!is.null(cmpx_networks[[i]]$nodes[[j]]$description)){
        nodes[[i]][[j]]$description <- cmpx_networks[[i]]$nodes[[j]]$description
      }
      
      #Node$type
      if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$type)){
        nodes[[i]][[j]]$type <- cmpx_networks[[i]]$nodes[[j]]$configuration$type
      }
      
      #Node$simulated
      if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$simulated) && cmpx_networks[[i]]$nodes[[j]]$configuration$simulated==TRUE){
        nodes[[i]][[j]]$simulated <- TRUE
      }else{
        nodes[[i]][[j]]$simulated <- FALSE
      }
      
      #Node$distr_type
      if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$type)){
        nodes[[i]][[j]]$distr_type <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$type
      }
      
      #Node$states, Node$probabilities, Node$expressions, and Node$partitions depend on Node$simulated and Node$distr_type
      
      #if Node is NOT SIMULATED... 
      if(nodes[[i]][[j]]$simulated == FALSE){
        #...and distribution type is MANUAL
        if(nodes[[i]][[j]]$distr_type=="Manual"){
          #Node$states and Node$probabilities are filled in
          #Node$expressions and Node$partitions are NULL
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$states)){
            nodes[[i]][[j]]$states <- cmpx_networks[[i]]$nodes[[j]]$configuration$states
            nodes[[i]][[j]]$probabilities <- vector(mode = "list", length = length(nodes[[i]][[j]]$states))
          }
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities)){
            for (k in 1:length(nodes[[i]][[j]]$states)) {
              nodes[[i]][[j]]$probabilities[[k]] <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities[[k]]
            }
            
            # if(length(cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities[[1]])==1){
            #   nodes[[i]][[j]]$probabilities <- list(cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities)
            # }else{
            #   nodes[[i]][[j]]$probabilities <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities
            # }
          }
        }
        #...and distribution type is EXPRESSION
        if(nodes[[i]][[j]]$distr_type=="Expression"){
          #Node$states, Node$probabilities, and Node$expressions (one element) are filled in
          #Node$partitions is NULL
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$states)){
            nodes[[i]][[j]]$states <- cmpx_networks[[i]]$nodes[[j]]$configuration$states
            nodes[[i]][[j]]$probabilities <- vector(mode = "list", length = length(nodes[[i]][[j]]$states))
          }
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities)){
            for (k in 1:length(nodes[[i]][[j]]$states)) {
              nodes[[i]][[j]]$probabilities[[k]] <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities[[k]]
            }
            # if(length(cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities[[1]])==1){
            #   nodes[[i]][[j]]$probabilities <- list(cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities)
            # }else{
            #   nodes[[i]][[j]]$probabilities <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities
            # }
          }
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions)){
            nodes[[i]][[j]]$expressions <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions
          }
        }
        #... and distribution type is PARTITIONED
        if(nodes[[i]][[j]]$distr_type=="Partitioned"){
          #Node$states, Node$probabilities, Node$expressions (many elements), and Node$partitions are filled in
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$states)){
            nodes[[i]][[j]]$states <- cmpx_networks[[i]]$nodes[[j]]$configuration$states
            nodes[[i]][[j]]$probabilities <- vector(mode = "list", length = length(nodes[[i]][[j]]$states))
          }
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities)){
            for (k in 1:length(nodes[[i]][[j]]$states)) {
              nodes[[i]][[j]]$probabilities[[k]] <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities[[k]]
            }
            # if(length(cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities[[1]])==1){
            #   nodes[[i]][[j]]$probabilities <- list(cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities)
            # }else{
            #   nodes[[i]][[j]]$probabilities <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$probabilities
            # }
          }
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions)){
            nodes[[i]][[j]]$expressions <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions
          }
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$partitions)){
            nodes[[i]][[j]]$partitions <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$partitions
          }
        }
      }
      
      #if Node is SIMULATED... 
      if(nodes[[i]][[j]]$simulated == TRUE){
        #...and distribution type is EXPRESSION
        if(nodes[[i]][[j]]$distr_type=="Expression"){
          #Node$expression (one element) is filled in
          #Node$states, Node$probabilities, and Node$partitions are NULL
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions)){
            nodes[[i]][[j]]$expressions <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions
          }
        }
        #... and distribution type is PARTITIONED
        if(nodes[[i]][[j]]$distr_type=="Partitioned"){
          #Node$expression (many elements) and Node$partitions are filled in
          #Node$states and Node$probabilities are NULL
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions)){
            nodes[[i]][[j]]$expressions <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$expressions
          }
          if(!is.null(cmpx_networks[[i]]$nodes[[j]]$configuration$table$partitions)){
            nodes[[i]][[j]]$partitions <- cmpx_networks[[i]]$nodes[[j]]$configuration$table$partitions
          }
        }
      }
    }
    
    # assigning all the Nodes to the nodes attribute of the correct Network objects
    networks[[i]]$nodes <- nodes[[i]]
    
    links[[i]] <- cmpx_networks[[1]]$links
  }
  
  for (i in 1:length(networks)) {
    for (j in 1:length(networks[[i]]$nodes)){
      for (k in 1:length(links[[i]])){
        if (links[[i]][[k]]$child == networks[[i]]$nodes[[j]]$id){
          networks[[i]]$nodes[[j]]$addParent(links[[i]][[k]]$parent)
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


for (ntw in networks){
  cat("Nodes in ",ntw$id,":\n")
  for (nd in ntw$nodes){
    cat(nd$id,"\n")
  }
}

