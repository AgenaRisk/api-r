
#BNNode object as R reference class
BNNode <- setRefClass("BNNode",
                          fields = list(id = "character",
                                        parents = "list",
                                        states = "character",
                                        probabilities = "list",
                                        expression = "character",
                                        expr_parameters = "list",
                                        partitions = "list"),
                          methods = list(
                            initialize = function(){},
                            show = function(){}
                            )
                          )

#BNModel object as R reference class
BNModel <- setRefClass("BNModel",
                       fields = list(id = "character",
                                     nodes = "list",
                                     links = "list"),
                       methods = list(
                         initialize = function(){},
                         show = function(){}
                         )
                       )


#function to read input CMPX file to create BNModel and its BNNodes
from_cmpx <- function(modelPath){
  
}