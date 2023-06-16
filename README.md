# Table of Contents

* [Description](#1-description)
* [Prerequisites](#2-prerequisites)
* [Structure of R-Agena Classes](#3-structure-of-r-agena-classes)
* [Class Methods](#4-class-methods)
* [Importing a Model from .cmpx](#5-importing-a-model-from-cmpx)
* [Creating and Modifying a Model in R](#6-creating-and-modifying-a-model-in-r)
* [Creating Batch Cases for a Model in R](#7-creating-batch-cases-for-a-model-in-r)
* [Agena.ai Cloud with R-Agena](#8-agenaai-cloud-with-r-agena)
* [Local agena.ai API with R-Agena](#9-local-agenaai-api-with-r-agena)
* [R-Agena Use Case Examples](#10-r-agena-use-case-examples)

# 1. Description

agena.ai is an R environment for creating, modifying, and parsing Bayesian network models, and sending the models to agena.ai Cloud to execute calculation requests. The environment allows users to read and modify Bayesian networks from .cmpx model files, create new Bayesian networks in R and export to .cmpx and .json files locally, as well as authenticating with agena.ai Cloud for individual or batch model calculations. In the rest of this document, the R environment for agena.ai is referred to as R-Agena.

# 2. Prerequisites and Installation

To install R-Agena from CRAN:

```r
install.packages("agena.ai")
```

R-Agena requires `rjson`, `httr`, `Rgraphviz`, and `openxlsx` packages installed.

To install `rjson`, `httr`, and `openxlsx` from CRAN:

```r
install.packages('rjson')
install.packages('httr')
install.packages('openxlsx')
```

To install `Rgraphviz` from Bioconductor:

```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("Rgraphviz")
```


# 3. Structure of R-Agena Classes

The Bayesian networks (BNs) in the R environment are represented with several objects: `Node`, `Network`, `DataSet`, and `Model`. These R objects generally follow their equivalents defined in agena.ai models.

## 3.1 `Node` objects

These represent the nodes in a BN. The fields that define a `Node` object are as follows:

### 3.1.1 `id`

Mandatory field to create a new `Node` object. This is the unique identifier of agena.ai model nodes.

### 3.1.2 `name`

Name of the node, optional. If not defined, `id` of the node will be passed onto the `name` field too.

### 3.1.3 `description`

Description of the node, optional. If not defined, "New Node" will be assigned to the `description` field.

### 3.1.4 `type`

Node type, it can be:

* Boolean
* Labelled
* Ranked
* DiscreteReal
* ContinuousInterval
* IntegerInterval

If it's not specified when creating a new node, the new node is "Boolean" by default if it's not a simulation node; and it is "ContinuousInterval" by default if it's a simulation node.

### 3.1.5 `parents`

Other `Node` objects can be pointed as parents of a `Node` object. It is not recommended to modify this field manually, to add parents to a node, see the function `addParent()`.

Something to keep in mind: the parent-child relationship information is stored at `Node` level in R environment thanks to this field, as opposed to the separate `links` field of a .cmpx/.json file for the agena.ai models. When importing or exporting .cmpx files you do not need to think about this difference as the cmpx parser and writer functions handle the correct formats. This difference allows adding and removing `Node` objects as parents 

### 3.1.6 `simulated`

A boolean field to indicate whether the node is a simulation node or not.

### 3.1.7 `distr_type`

The table type of the node, it can be:

* Manual
* Expression
* Partitioned

### 3.1.8 `states`

States of the node (if not simulated). If states are not specified, depending on the `type`, sensible default states are assigned. Default states for different node types are:

* "Boolean" or "Labelled" node: "False", "True"
* "Ranked" node: "Low", "Medium", "High"
* "DiscreteReal" node: "0", "1"

And for a node with the table type (`distr_type`) "Expression", the default expression is: "Normal(0,1000000)"

### 3.1.9 `probabilities`

If the table type (`distr_type`) of the node is "Manual", the node will have state probabilities, values in its NPT. This field is a list of these values. The length of the list depends on the node states and the number of its parents. To see how to set probability values for a node, see `setProbabilities()` function. 

### 3.1.10 `expressions`

If the table type (`distr_type`) of the node is "Expression" or "Partitioned", the node will have expression(s) instead of the manually defined NPT values.

* If the node's table type is "Expression", the `expressions` field will have a single expression (a single character string).
* If the node's table type is "Partitioned", the `expressions` field will have a list of as many expressions as the number of parent node states on which the expression is partitioned.

To see how to set the expressions for a node, see `set_expressions()` function.

### 3.1.11 `partitions`

If the table type (`distr_type`) of the node is "Partitioned", in addition to the expressions, the node will have the `partitions` field. This field is a list of strings, which are `id`s of the parent nodes on which the node expression is partitioned.

### 3.1.12 `variables`

The node variables are called constants on agena.ai Modeller. This field, if specified, sets the constant value for the node observations.

## 3.2 `Network` objects 

These represent each network in a BN. Networks consist of nodes and in a BN model there might be more than one network. These networks can also be linked to each other with the use of input and output nodes. For such links, see `Model$networkLinks` field later in this document.

The fields that define a `Network` object are as follows:

### 3.2.1 `id`

Id of the `Network`. Mandatory field to create a new network.

### 3.2.2 `name`

Name of the network, optional. If not specified, `id` of the network is passed onto `name` field as well.

### 3.2.3 `description`

Description, optional. If not specified, the string "New Network" is assigned to `description` field by default.

### 3.2.4 `nodes`

A list of `Node` objects which are in the network. These `Node` objects have their own fields which define them as explained above in this document.

Note that `Network` objects do not have a `links` field unlike the agena.ai models. As explained in `Node$parents` section above, this information is stored in `Node` objects in the R environment. When importing a .cmpx model, the information in `links` field is used to populate `Node$parents` fields for each node. Similarly, when exporting to a .cmpx/.json file, the parent-child information in `Node$parents` field is used to create the `links` field of the `Network` field of the .cmpx/.json.

## 3.3 `DataSet` objects 

These represent the set of observations in a BN. A `Model` can have multiple `DataSet` objects in its `dataSets` field.  When a new `Model` is created, it always comes with a default `DataSet` object with the `id` "Scenario 1" and with blank observations. It is possible to add more datasets (scenarios) with their `id`s. Each `DataSet` object under a `Model` can be called a new "scenario".

### 3.3.1 `id`

Id of the dataset (scenario).

### 3.3.2 `observations`

Under each dataset (scenario), observations for all the observed nodes in all the networks of the model (in terms of their states or values) are listed. If it's hard evidence, observation for a node will have a single value with the weight of 1. If a node in the model has a value in its `variable` field, this value will be passed onto the dataset (scenario) with the weight of 1.

### 3.3.3 `results`

This field is defined only for when a .cmpx model with calculations is imported. When creating a new BN in the R environment, this field is not created or filled in. The `results` field stores the posterior probability and inference results upon model calculation on agena.ai Cloud.

## 3.4 `Model` objects

These represent the overall BN. A single .cmpx file corresponds to a singe `Model`. A BN model can have multiple networks with their own nodes, links between these networks, and datasets. 

## 3.4.1 `id`

Id of the Model, optional. If not specified, the `id` of the first `Network` in the model's `networks` field is used to create a `Model$id`. 

## 3.4.2 `networks`

A list of all the `Network` objects that make up the model. This field is mandatory for creating a new `Model` object. 

## 3.4.3 `dataSets`

Optional field for `DataSet` objects. When creating a new `Model`, it is possible to use predefined scenarios as long as their `DataSet$observations` field has matching `id`s with the nodes in the model. If none is specified, by default a new `Model` object will come with an empty dataset called "Scenario 1".

## 3.4.4 `networkLinks`

If the `Model` has multiple networks, it is possible to have links between these networks, following the agena.ai model networkLinks format.

To see how to create these links, see `add_network_link()` function later in this document.

## 3.4.5 `settings`

`Model` settings for calculations. It includes the following fields (the values in parantheses are the defaults if settings are not specified for a model):

* parameterLearningLogging (FALSE)
* discreteTails (FALSE)
* sampleSizeRanked (5)
* convergence (0.001)
* simulationLogging (FALSE)
* iterations (50)
* tolerance (1)

Model settings can be provided when creating a new model, if not provided the model will come with the default settings. Default settings can be changed later on (with the method `$change_settings()`), or model settings can be reset back to default values (with the method `$default_settings()`). See the correct input parameter format for these functions in the following section.

# 4. Class Methods

The `Node`, `Network`, and `Model` objects have their own respective methods to help their definition and manipulate their fields. The R class methods are used with the `$` sign following an instance of the class. For example,

```r
example_node$add_parent(exampleParentNode)
```

or

```r
example_network$remove_node(exampleNode)
```

or

```r
example_model$create_dataSet(exampleScenario)
```

## 4.1 `Node` methods

Some `Node` fields can be modified with a direct access to the field. For example, to update the name or a description information of a `Node`, simply use:

```r
example_node$name <- "new node name"
```

or

```r
example_node$description <- "new node description"
```

Because changing the name or description of a `Node` does not cause any compatibility issues. However, some fields such as table type or parents will have implications for other fields. Changing the node parents will change the size of its NPT, changing the node's table type from "Manual" to "Expression" will mean the state probabilities are now defined in a different way. Therefore, to modify such fields of a `Node`, use the corresponding method described below. These methods will ensure all the sensible adjustments are made when a field of a `Node` has been changed.

These are the methods `Node` objects can call for various purposes with their input parameters shown in parantheses:

### 4.1.1 `add_parent(newParent)`

The method to add a new parent to a node. Equivalent of adding an arc between two nodes on agena.ai Modeller. The input parameter `newParent` is another `Node` object. If `newParent` is already a parent for the node, the function does not update the `parents` field of the node.

When a new parent is added to a node, its NPT values and expressions are reset/resized accordingly. 

There is also a method called `addParent_byID(newParentID, varList)`, however, this is only used in the cmpx parser. To add a new parent to a `Node`, it is recommended to use `add_parent()` function with a `Node` object as the input.

### 4.1.2 `remove_parent(oldParent)` 

The method to remove one of the existing parents of a node. Equivalent of removing the arc between two nodes on agena.ai Modeller. The input parameter `oldParent` is a `Node` object which has already been added to the `parents` field of the node.

When an existing parent is removed from a node, its NPT values and expressions are reset/resized accordingly.

### 4.1.3 `get_parents()`

A method to list all the existing parent nodes of a `Node`.

### 4.1.4 `set_distribution_type(new_distr_type)`

A method to set the table type (`distr_type`) of a node. If a `Node` is `simulated`, its table type can be "Expression" or "Partitioned" - the latter is only if the node has parent nodes. If a `Node` is `not simulated`, its table type can be "Manual", "Expression", or "Partitioned Expression (if the node has parent nodes)".

### 4.1.5 `set_probabilities(new_probs, by_rows = TRUE)`

The method to set the probability values if the table type (`distr_type`) of a `Node` is "Manual". `new_probs` is a list of numerical values, and the length of the input list depends on the number of the states of the node and of its parents.

You can format the input list in two different orders. If the parameter `by_rows` is set to true, the method will read the input list to fill in the NPT row by row; if set to false, the method will read the input list to fill in the NPT column by columnn. This behaviour is illustrated with use case examples later in this document.

### 4.1.6 `set_expressions(new_expr, partition_parents = NULL)`
The method to set the probability values if the table type (`distr_type`) of a `Node` is "Expression" or "Partitioned". If the table type is "Expression", `new_expr` is a single string and `partition_parents` is left NULL. If the table type is "Partitioned", `new_expr` is a list of expressions for each parent state, and `partition_parents` is a list of strings for each partitioned parent node's `id`.

### 4.1.7 `set_variable(variable_name, variable_value)`

A method to set variables (constants) for a node. Takes the `variable_name` and `variable_value` inputs which define a new variable (constant) for the node.

### 4.1.8 `remove_variable(variable_name)`

A method to remove one of the existing variables (constants) from a node, using the `variable_name`.

## 4.2 `Network` methods

As described above, `Node` objects can be created and manipulated outside a network in the R environment. Once they are defined, they can be added to a `Network` object. Alternatively, a `Network` object can be created first and then its nodes can be specified. The R environment gives the user freedom, which is different from agena.ai Modeller where it is not possible to have a node completely outside any network. Once a `Network` object is created, with or without nodes, the following methods can be used to modify and manipulate the object.

### 4.2.1 `add_node(newNode)`

A method to add a new `Node` object to the `nodes` field of a `Network` object. The input `newNode` is a `Node` object and it is added to the network if it's not already in it.

Note that adding a new `Node` to the network does not automatically add its parents to the network. If the node has parents already defined, you need to add all the parent `Node`s separately to the network, too.

### 4.2.2 `remove_node(oldNode)`

A method to remove an existing `Node` object from the network. Note that removing a Node from a network doesn't automatically remove it from its previous parent-child relationships in the network. You need to adjust such relationships separately on `Node` level.

### 4.2.3 `get_nodes()`

A method to see `id`s of all the nodes in a network.

### 4.2.4 `plot()`

A method to plot the graphical structure of a BN network.

## 4.3 `Model` methods

A `Model` object consists of networks, network links, datasets, and settings. A new `Model` object can be created with a network (or multiple networks). By default, it is created with a single empty dataset (scenario) called "Scenario 1". Following methods can be used to modify `Model` objects: 

### 4.3.1 `add_network(newNetwork)`

A method to add a new `Network` object to the `networks` field of a `Model` object. The input `newNetwork` is a `Network` object and it is added to the model if it's not already in it.

### 4.3.2 `remove_network(oldNetwork)`

A method to remove an existing `Network` object from the model. Note that removing a Node from a network doesn't automatically remove its possible network links to other networks in the model. `networkLinks` field of a `Model` should be adjusted accordingly if needed.

### 4.3.3 `get_networks()`

A method to see `id`s of all the networks in a model.

### 4.3.4 `add_network_link(source_network, source_node, target_network, target_node, link_type, pass_state = NULL)`

This is the method to add links to a model between its networks. These links start from a "source node" in a network and go to a "target node" in another network. To create the link, the source and target nodes in the networks need to be specified together with the network they belong to (by the `Node` and `Network` `id`s). The input parameters are as follows:

* `source_network` = `Network$id` of the network the source node belongs to
* `source_node` = `Node$id` of the source node
* `target_network` = `Network$id` of the network the target node belongs to
* `target_node` = `Node$id` of the target node
* `link_type` = a string of the link type name. It can be one of the following:
    * Marginals
    * Mean
    * Median
    * Variance
    * StandardDeviation
    * LowerPercentile
    * UpperPercentile
    * State
* `pass_state` = one of the `Node$states` of the source node. It has to be specified only if the `link_type` of the link is `"State"`, otherwise is left blank.

Note that links between networks are allowed only when the source and target nodes fit certain criteria. Network links are allowed if:

* Both nodes are the same type and either of them is simulated
* Both nodes are the same type and neither is simulated and both have the same number of states
* Source node is not numeric interval or discrete real and target node is simulated

### 4.3.5 `remove_network_link(source_network, source_node,target_network, target_node)`

A method to remove network links, given the `id`s of the source and target nodes (and the networks they belong to).

### 4.3.6 `remove_all_network_links()`

A method to remove all existing network links in a model.

### 4.3.7 `create_dataSet(id)`

It is possible to add multiple scenarios to a model. These scenarios are new `DataSet` objects added to the `dataSets` field of a model. Initially these scenarios have no observations and are only defined by their `id`s. The scenarios are populated with the `enter_observation()` function.

### 4.3.8 `remove_dataSet(olddataSet)`

A method to remove an existing scenario from the model. Input parameter `olddataSet` is the string which is the `id` of a dataset (scenario).

### 4.3.9 `get_dataSets()`

A method to list the `id`s of all existing scenarios in a model.

### 4.3.10 `enter_observation(dataSet = NULL, node, network, value, variable_input = FALSE, soft_evidence = FALSE)`

A method to enter observation to a model. To enter the observation to a specific dataset (scenario), the dataset id must be given as the input parameter `dateSet`. If `dataSet` is left NULL, the entered observation will by default go to "Scenario 1". This means that if there is no extra datasets created for a model (which by default comes with "Scenario 1"), any observation entered will be set for this dataset (mimicking the behaviour of entering observation to agena.ai Modeller).

The observation is defined with the mandatory input parameters:
* `node` = `Node$id` of the observed node
* `network` = `Network$id` of the network the observed node belongs to
* `value` = this parameter can be:
    * the value or state of the observation for the observed node (if variable_input and soft_evidence are FALSE)
    * the id of a variable (constant) defined for the node (if variable_input is TRUE)
    * the array of multiple values and their weights (if soft_evidence is TRUE)
* `variable_input` = a boolean parameter, set to TRUE if the entered observation is a variable (constant) id for the node instead of an observed value
* `soft_evidence` = a boolean parameter, set to TRUE if the entered observation is not hard evidence. Then the `value` parameter should follow `c(value_one, value_one_weight, value_two, value_two_weight, ..., value_n, value_n_weight)`

### 4.3.11 `remove_observation(dataSet = NULL, node, network)`

A method to remove a specific observation from the model. It requires the id of the node which has the observation to be removed and the id of the network the node belongs to.

### 4.3.12 `clear_dataSet_observations(dataSet)`

A method to clear all observations in a specific dataset (scenario) in the model.

### 4.3.13 `clear_all_observations()`

A method to clear all observations defined in a model. This function removes all observations from all datasets (scenarios).

### 4.3.14 `import_results(results_file)`

A method to import results of a calculated dataSet from a json file. This correct format for the results json file for this method is the file generated with the local agena.ai developer API calculation (see [Section 9](#9-local-agenaai-api-with-r-agena)).

Note that when you use local API calculation, the results are imported to the model automatically.

### 4.3.15 `change_settings(settings)`

A method to change model settings. The input parameter `settings` must be a list with the correctly named elements, for example:

```r
new_settings <- list(parameterLearningLogging = TRUE, 
                    discreteTails = FALSE, 
                    sampleSizeRanked = 10, 
                    convergence = 0.05, 
                    simulationLogging = TRUE, 
                    iterations = 100, 
                    tolerance = 1)

example_model$change_settings(new_settings)
```

### 4.3.16 `default_settings()`

A method to reset model settings back to default values. The default values for model settings are:

* parameterLearningLogging = FALSE
* discreteTails = FALSE
* sampleSizeRanked = 5
* convergence = 0.001
* simulationLogging = FALSE
* iterations = 50
* tolerance = 1

### 4.3.17 `to_cmpx(filename = NULL)`

A method to export the `Model` to a .cmpx file. This method passes on all the information about the model, its datasets, its networks, their nodes, and model settings to a .cmpx file in the correct format readable by agena.ai.

If the input parameter `filename` is not specified, it will use the `Model$id` for the filename.

### 4.3.18 `to_json(filename = NULL)`

A method to export the `Model` to a .json file instead of .cmpx. See `to_cmpx()` description above for all the details.

### 4.3.19 `get_results()`

A method to generate a .csv file based on the calculation results a `Model` contains. See [Section 8](#8-agena-ai-cloud-with-r-agena) for details.

## 4.4 Other R-Agena Functions

R-Agena environment provides certain other functions outside the class methods.

### 4.4.1 `from_cmpx(modelPath = "/path/to/model/file.cmpx")`

This is the cmpx parser function to import a .cmpx file and create R objects based on the model in the file. To see its use, see [Section 5](#5-importing-a-model-from-cmpx) and [Section 9](#9-r-agena-use-case-examples).

### 4.4.2 `create_batch_cases(inputModel, inputData)`

This function takes an R `Model` object (`inputModel`) and an input CSV file (`inputData`) with observations defined in the correct format and creates a batch of datasets (scenarios) for each row in the input data and generates a .json file. To see its use and the correct format of the CSV file for a model's data, see [Section 7](#7-creating-batch-cases-for-a-model-in-r).

### 4.4.3 `create_csv_template(inputModel)`

This function creates an empty CSV file with the correct format so that it can be filled in and used for `create_batch_bases()`.

### 4.4.4 `create_sensitivity_config(...)`

A function to create a sensitivity configuration object if a sensitivity analysis request will be sent to agena.ai Cloud servers. Its parameters are:

* `target` = target node ID for the analysis
* `sensitivity_nodes` = a list of sensitivity node IDs
* (optional) `network` = ID of the network to perform analysis on. If missing, the first network in the model is used
* (optional) `dataset` = ID of the dataSet (scenario) to use for analysis
* (optional) `report_settings` = settings for the sensitivity analysis report. A named list with the following fields:
    * `summaryStats` (a list with the following fields)
        * mean
        * median
        * variance
        * standardDeviation
        * upperPercentile
        * lowerPercentile
    * `sumsLowerPercentileValue` (set the reported lower percentile value.
Default is 25)
    * `sumsUpperPercentileValue` (set the reported upper percentile value.
Default is 75)
    * `sensLowerPercentileValue` (lower percentile value to limit sensitivity node data by. Default is 0)
    * `sensUpperPercentileValue` (upper percentile value to limit sensitivity node data by. Default is 100)

For the use of the function, see [Section 8](#8-agenaai-cloud-with-r-agena).

## 4.5 agena.ai Cloud Related Functions

R-Agena environment allows users to send their models to agena.ai Cloud servers for calculation. The functions around the server capabilities (including authentication) are described in [Section 8](#8-agenaai-cloud-with-r-agena).

## 4.6 agena.ai Local API Related Function

R-Agena environment allows users to connect to the local agena.ai developer API for calculation. The functions about the local developer API communication are descibed in [Section 9](#9-local-agenaai-api-with-r-agena).

# 5. Importing a Model from .cmpx

To import an existing agena.ai model (from a .cmpx file), use the `from_cmpx()` function:

```r
library(agena.ai)

new_model <- from_cmpx("/path/to/model/file.cmpx")
```

This creates an R `Model` object with all the information taken from the .cmpx file. All fields and sub-fields of the `Model` object (as per [Section 3](#3-structure)) are accessible now. For example, you can see the networks in this model with:

```r
new_model$networks
```

Each network in the model is a `Network` object, therefore you can access its fields with the same logic, for example to see the id of the first network and all the nodes in the first network in the BN, use respectively:

```r
new_model$networks[[1]]$id
```

```r
new_model$networks[[1]]$nodes
```

Similarly, each node in a network itself is a `Node` object. You can display all the fields of a node. Example uses for the second node in the first network of a model:

```r
new_model$networks[[1]]$nodes[[1]]$id
```

```r
new_model$networks[[1]]$nodes[[1]]$id
```

Once the R model is created from the imported .cmpx file, the `Model` object as well as all of its `Network`, `DataSet`, and `Node` objects can be manipulated using R methods.

# 6. Creating and Modifying a Model in R

It is possible to create an agena.ai model entirely in R, without a .cmpx file to begin with. Once all the networks and nodes of a model are created and defined in R, you can export the model to a .cmpx or .json file to be used with agena.ai calculations and inference, locally or on agena.ai Cloud. In this section, creating a model is shown step by step, starting with nodes.

Import the installed agena.ai R code with

```r
library(agena.ai)
```

## 6.1 Creating Nodes

In the R environment, `Node` objects represent the nodes in BNs, and you can create `Node` objects before creating and defining any network. To create a new node, only its id (unique identifier) is mandatory, you can define some other optional fields upon creation if desired. A new node creation function takes the following parameters where id is the only mandatory one and all others are optional:

```r
Node$new(id, name, description, type, simulated, states)

# id parameter is mandatory
# the rest is optional
```

If the optional fields are not specified, the nodes will be created with the defaults. The default values for the fields, if they are not specified, are:

* name = node id
* description = "New Node"
* simulated = FALSE
* type = 
    * if simulated: "ContinuousInterval"
    * if not simulated: "Boolean"
* states =
    * if Boolean or Labelled: ("False", "True")
    * if Ranked: ("Low", "Medium", "High")
    * if DiscreteReal: ("0.0", "1.0")

Once a new node is created, depending on the type and number of states, other fields are given sensible default values too. These fields are distr_type (table type), probabilities or expressions. To specify values in these fields, you need to use the relevant set functions (explained in [Section](#4-class-methods) and shown later in this section). The default values for these fields are:

* distr_type = 
    * if simulated: "Expression"
    * if not simulated: "Manual"
* probabilities = 
    * if distr_type is Manual: discrete uniform distribution, each state has the probability of (1/number of states)
* expressions = 
    * if distr_type is Expression: "Normal(0,1000000)"

Look at the following new node creation examples:

```r
node_one <- Node$new(id = "node_one")
```

```r
node_two <- Node$new(id = "node_two", name = "Second Node")
```

```r
node_three <- Node$new(id = "node_three", type = "Ranked")
```

```r
node_four <- Node$new(id = "node_four", type = "Ranked", states = c("Very low", "Low", "Medium", "High", "Very high"))
```

Looking up some example values in the fields that define these nodes:

* node_one$id = "node_one"
* node_one$name = "node_one"
* node_one$description = "New Node"
* node_one$type = "Boolean"
* node_one$states = ("False", "True")
* node_two$id = "node_two"
* node_two$name = "Second Node"
* node_three$type = "Ranked"
* node_three$states =  ("Low", "Medium", "High")
* node_four$states =  ("Very low", "Low", "Medium", "High", "Very high")
* node_one$distr_type = "Manual"
* node_one$probabilities = (0.5, 0.5)
* node_three$probabilities = (0.3333, 0.3333, 0.3333)
* node_four$probabilities = (0.2, 0.2, 0.2, 0.2, 0.2)

## 6.2 Modifying Nodes

To update node information, some fields can be simply overwritten with direct access to the field if it does not affect other fields. These fields are node name, description, or state names (without changing the number of states). For example: 

```r
node_one$states <- c("Negative","Positive")
```

```r
node_one$description <- "first node we have created"
```

Other fields can be specified with the relevant set functions. To set probability values for a node with a manual table (distr_type), you can use `set_probabilities()` function:

```r
node_one$set_probabilities(list(0.2,0.8))
```

Note that the `set_probabilities()` function takes a `list` as input, even when the node has no parents and its NPT has only one row of probabilities. If the node has parents, the NPT will have multiple rows which should be in the input list.

Assume that `node_one` and `node_two` are the parents of `node_three` (how to add parent nodes is illustrated later in this section). Now assume that you want `node_three` to have the following NPT:

<table>
<tbody>
  <tr>
    <td><strong>node_one</strong></td>
    <td colspan="2"><strong>Negative</strong></td>
    <td colspan="2"><strong>Positive</strong></td>
  </tr>
  <tr>
    <td><strong>node_two</strong></td>
    <td><strong>False</strong></td>
    <td><strong>True</strong></td>
    <td><strong>False</strong></td>
    <td><strong>True</strong></td>
  </tr>
  <tr>
    <td>Low</td>
    <td>0.1</td>
    <td>0.2</td>
    <td>0.3</td>
    <td>0.4</td>
  </tr>
  <tr>
    <td>Medium</td>
    <td>0.4</td>
    <td>0.45</td>
    <td>0.6</td>
    <td>0.55</td>
  </tr>
  <tr>
    <td>High</td>
    <td>0.5</td>
    <td>0.35</td>
    <td>0.1</td>
    <td>0.05</td>
  </tr>
</tbody>
</table>

There are two ways to order the values in this table for the `set_probabilities()` function, using the boolean `by_rows` parameter. If you want to enter the values following the rows in agena.ai Modeller NPT rather than ordering them by the combination of parent states (columns), you can use `by_rows = TRUE` where each element of the list is a row of the agena.ai Modeller NPT:

```r
node_three$set_probabilities(list(c(0.1, 0.2, 0.3, 0.4), c(0.4, 0.45, 0.6, 0.55), c(0.5, 0.35, 0.1, 0.05)), by_rows = TRUE)
```

If, instead, you want to define the NPT with the probabilities that add up to 1 (conditioned on the each possible combination of parent states), you can set `by_rows = FALSE` as the following example:

```r
node_three$set_probabilities(list(c(0.1, 0.4, 0.5), c(0.2, 0.45, 0.35), c(0.3, 0.6, 0.1), c(0.4, 0.55, 0.05)), by_rows = FALSE)
```

Similarly, you can use `set_expressions()` function to define and update expressions for the nodes without Manual NPT tables. If the node has no parents, you can add a single expression:

```r
example_node$set_expressions("TNormal(4,1,-10,10)")
```

Or if the node has parents and the expression is partitioned on the parents:

```r
example_node$set_expressions(c("Normal(90,10)", "Normal(110,15)", "Normal(120,30)"), partition_parents = "parent_node")
```

Here you can see the expression is an array with three elements and the second parameter (`partition_parameters`) contains the ids of the parent nodes. Expression input has three elements based on the number of states of the parent node(s) on which the expression is partitioned.

## 6.3 Adding and Removing Parent Nodes

To add parents to a node, you can use `addParent()` function. For example:

```r
node_three$addParent(node_one)
```

This adds `node_one` to the parents list of `node_three`, and resizes the NPT of `node_three` (and resets the values to a discrete uniform distribution).

To remove an already existing parent, you can use:

```r
node_three$removeParent(node_one)
```

This removes `node_one` from the parents list of `node_three`, and resizes the NPT of `node_three` (and resets the values to a discrete uniform distribution).

Below we follow the steps from creation of node_three to the parent modifications and see how the NPT of node_three changes after each step.

* Creating node_tree with only type specified:

```r
node_three <- Node$new(id = "node_three", type = "Ranked")
```
* node_three$getParents()

```r
NULL
```

* node_three$probabilities

```r
[[1]]
[1] 0.3333333

[[2]]
[1] 0.3333333

[[3]]
[1] 0.3333333

#discrete uniform with three states (default of Ranked node)
```

* Changing the probabilities:

```r
node_three$setProbabilities(list(0.7, 0.2, 0.1))
```

* node_three$probabilities

```r
[[1]]
[1] 0.7

[[2]]
[1] 0.2

[[3]]
[1] 0.1
```

* Adding a parent:

```r
node_three$addParent(node_one)
```

* node_three$getParents()

```r
[1] "node_one"

# node_one has been added to the parents list of node_three
```

* node_three$probabilities

```r
[[1]]
[1] 0.3333333 0.3333333

[[2]]
[1] 0.3333333 0.3333333

[[3]]
[1] 0.3333333 0.3333333

#  NPT of node_three has been resized based on the number of parent node_one states
# NPT values for node_three are reset to discrete uniform
```

* Adding another parent:

```r
node_three$addParent(node_two)
```

* node_three$getParents()

```r
[1] "node_one" "node_two"

# node_two has been added to the parents list of node_three
```

* node_three$probabilities

```r
[[1]]
[1] 0.3333333 0.3333333 0.3333333 0.3333333

[[2]]
[1] 0.3333333 0.3333333 0.3333333 0.3333333

[[3]]
[1] 0.3333333 0.3333333 0.3333333 0.3333333

#  NPT of node_three has been resized based on the number of parent node_one and node_two states
# NPT values for node_three are reset to discrete uniform
```

## 6.4 Creating and Modifying Networks

BN Models contain networks, at least one or optionally multiple. If there are multiple networks in a model, they can be linked to each other with the use of input and output nodes. A `Network` object in R represents a network in a BN model. To create a new `Network` object, you need to specify its id (mandatory parameter), and you can also fill in the optional parameters:

```r
Network$new(id, name, description, nodes)

# id parameter is mandatory
# the rest is optional
```

Here clearly `nodes` field is the most important information for a network but you do not need to specify these on creation. You can choose to create an empty network and fill it in with the nodes afterwards with the use of `add_node()` function. Alternatively, if all (or some) of the nodes you will have in the network are already defined, you can pass them to the new `Network` object on creation.

Below is an example of network creation with the nodes added later:

```r
network_one <- Network$new(id = "network_one")

network_one$add_node(node_three)
network_one$add_node(node_one)
network_one$add_node(node_two)
```

Notice that when node_three is added to the network, its parents are not automatically included. So if a node has parents, you need to separately add them to the network, so that later on your model will not have discrepancies.

The order in which nodes are added to a network is not important as long as all parent-child nodes are eventually in the network.

Alternatively, you can create a new network with its nodes:

```r
network_two <- Network$new(id = "network_two", nodes = c(node_one, node_two, node_three))
```

Or you can create the network with some nodes and add more nodes later on:

```r
network_three <- Network$new(id = "network_three", nodes = c(node_one, node_three))

network_three$add_node(node_two)
```

To remove a node from a network, you can use `remove_node()` function. Again keep in mind that removing a node does not automatically remove all of its parents from the network. For example,

```r
network_three$remove_node(node_three)
```

To plot a network and see its graphical structure, you can use

```r
network_one$plot()
```

## 6.5 Creating and Modifying the Model

BN models consist of networks, the links between networks, and datasets (scenarios). Only the networks information is mandatory to create a new `Model` object in R. The other fields can be filled in afterwards. The new model creation function is:

```r
Model$new(id, networks, dataSets, networkLinks)

# networks parameter is mandatory
# the rest is optional
```

For example, you can create a model with the networks defined above:

```r
example_model <- Model$new(networks = list(network_one))
```

Note that even when there is only one network in the model, the input has to be a list. Networks in a model can be modified with `add_network()` and `remove_network()` functions:

```r
example_model$add_network(network_two)
```

```r
example_model$remove_network(network_two)
```

Network links between networks of the model can be added with the `add_network_link()` function. For example:

```r
example_model$add_network_link(source_network = network_one, source_node = node_three, target_network = network_two, target_node = node_three, link_type = "Marginals")
```

For link_type options and allowed network link rules, see [`add_network_link()` section](#434-add_network_linksource_network-source_node-target_network-target_node-link_type-pass_state--null).

When a new model is created, it comes with a single dataset (scenario) by default. See next section to see how to add observations to this dataset (scenario) or add new datasets (scenarios).

## 6.6 Creating Datasets (Scenarios) and Entering Observation

To enter observations to a Model (which by default has one single scenario), use the `enter_observation()` function. You need to specify the node (and the network it belongs to) and give the value (one of the states if it's a discrete node, a sensible numerical value if it's a continuous node):

```r
example_model$enter_observation(node = node_three, network = network_one, value = "High")
```

Note that this function did not specify any dataset (scenario). If this is the case, observation is always entered to the first (default) scenario.

You may choose to add more datasets (scenarios) to the model with the `create_dataSet()` function:

```r
example_model$create_dataSet("Scenario 2")
```

Once added, you can enter observation to the new dataset (scenario) if you specify the `dataSet` parameter in the `enter_observation()` function:

```r
example_model$enter_observation(dataSet = "Scenario 2", node = node_three, network = network_one, value = "Medium")
```

## 6.7. Exporting a Model to .cmpx or .json

Once an R model is defined fully and it is ready, you can export it to a .cpmx or a .json file. The function to create these files convert the information to the correct format for agena.ai to understand. You can use either of the functions:

```r
example_model$to_json()
```

or 

```r
example_model$to_cmpx()
```

If left blank, these functions will create a file named after the `Model$id` with the correct extension. You may choose to name the file at the creation:

```r
example_model$to_json("custom_file_name")
```

# 7. Creating Batch Cases for a Model in R

R-Agena environment allows creation of batch cases based on a single model and multiple observation sets. Observations should be provided in a CSV file with the correct format for the model. In this CSV file, each row of the data is a single case (dataset) with a set of observed values for nodes in the model. First column of the CSV file is the dataset (scenario) ids which will be used to create a new risk scenario for each data row. All other columns are possible evidence variables whose headers follow the "node_id.network_id" format. Thus, each column represents a node in the BN and is defined by the node id and the id of the network to which it belongs.

An example CSV format is as below:

<table>
<thead>
  <tr>
    <th>Case</th>
    <th>node_one.network_one</th>
    <th>node_two.network_one</th>
    <th>cont_node.network_one</th>
    <th>node_one.network_two</th>
    <th>node_two.network_two</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>1</td>
    <td>Negative</td>
    <td>True</td>
    <td>20</td>
    <td>Negative</td>
    <td>False<br></td>
  </tr>
  <tr>
    <td>2</td>
    <td>Positive<br></td>
    <td>True</td>
    <td></td>
    <td>Negative</td>
    <td>True</td>
  </tr>
  <tr>
    <td>3</td>
    <td>Positive</td>
    <td>False</td>
    <td>18</td>
    <td>Positive</td>
    <td></td>
  </tr>
</tbody>
</table>

Once the model is defined in R-Agena and the CSV file with the observations is prepared, you can use the `create_batch_cases()` function to generate scenarios for the BN:

```r
create_batch_cases(inputModel, inputData)
```

where `inputModel` is a `Model` object and `inputData` is the path to the CSV file with the correct format. For example,

```r
create_batch_cases(example_model, "example_dataset.csv")
```

This will create new datasets (scenarios) for each row of the dataset in the model, fill these datasets (scenarios) in with the observations using the values given in the dataset, create a new .json file for the model with all the datasets (scenarios). If there are NA values in the dataset, it will not fill in any observation for that specific node in that specific dataset (scenario).

Important note: Once the function has generated the .json file with all the new datasets (scenarios), it will remove the new datasets (scenarios) from the model. This function does not permanently update the model with the datasets (scenarios), it generates a .json model output with the observed datasets (scenarios) for the BN. It also does not alter already existing datasets (scenarios) in the `Model` object if there are any.

Assume that you use a model in R with two already existing datasets: an empty default "Scenario 1" which was created with the model, and a dataset (scenario) you have added "Test patient" with some observations. And you have a CSV file with 10 rows of data, whose Case column reads: "Patient 1, Patient 2, ..., Patient 10", with the set of observations for 10 patients. Once `create_batch_cases()` is used, it's going to generate a .json file for this model with all 12 datasets (scenarios), but after the use of the function, the model will still have only "Scenario 1" and "Test patient" datasets (scenarios) in its `$dataSets` field.

# 8. agena.ai Cloud with R-Agena

You can use R-Agena environment to authenticate with agena.ai Cloud (using your existing account) and send your model files to Cloud for calculations. The connection between your local R-Agena environment and agena.ai Cloud servers is based on the `httr` package in R.

## 8.1 Authentication

`login()` function is used to authenticate the user. To create an account, visit https://portal.agena.ai. Once created, you can use your credentials in R-Agena to access the servers.

```r
example_login <- login(username, password)
```

This will send a POST request to authentication server, and will return the login object (including access and refresh tokens) which will be used to authenticate further operations.

## 8.2 Model Calculation

`calculate()` function is used to send an R model object to agena.ai Cloud servers for calculation. The function takes the following parameters:

* `input_model` is the R Model object
* `login` is the login object created with the credentials
* (optional) `dataSet` is the name of the dataset that contains the set of observations (`$id` of one of the `dataSets` objects) if any. If the model has only one dataset (scenario) with observations, scenario needs not be specified (it is also possible to send a model without any observations).

Currently servers accept a single set of observations for each calculation, if the R model has multiple datasets (scenarios), you need to specify which dataset is to be used.

For example,

```r
calculate(example_model, example_login)
```

or

```r
calculate(example_model, example_login, dataSet_id)

```

If calculation is successful, this function will update the R model (the relevant `dataSets$results` field in the model) with results of the calculation.

If you would like to see the calculation results in a .csv format, you can use the Model method `get_results()` to generate the output file.

`get_results()` is a method for the R `Model` objects, and it creates a .csv output with all calculated marginal posterior probabilities in the model. To use the function,

```r
example_model$get_results()
```

or with a custom file name:

```r
example_model$get_results("example_output_file")
```

This will generate a .csv file with the following format:

<table>
<thead>
  <tr>
    <th>Scenario</th>
    <th>Network</th>
    <th>Node</th>
    <th>State</th>
    <th>Probability</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>Scenario 1</td>
    <td>Network 1</td>
    <td>Node 1</td>
    <td>State 1</td>
    <td>0.2</td>
  </tr>
  <tr>
    <td>Scenario 1</td>
    <td>Network 1</td>
    <td>Node 1</td>
    <td>State 2</td>
    <td>0.3</td>
  </tr>
  <tr>
    <td>Scenario 1</td>
    <td>Network 1</td>
    <td>Node 1</td>
    <td>State 3</td>
    <td>0.5</td>
  </tr>
  <tr>
    <td>Scenario 1</td>
    <td>Network 1</td>
    <td>Node 2</td>
    <td>State 1</td>
    <td>0.3</td>
  </tr>
  <tr>
    <td>Scenario 1</td>
    <td>Network 1</td>
    <td>Node 2</td>
    <td>State 2</td>
    <td>0.7</td>
  </tr>
  <tr>
    <td>Scenario 1</td>
    <td>Network 1</td>
    <td>Node 3</td>
    <td>State 1</td>
    <td>0.1</td>
  </tr>
  <tr>
    <td>Scenario 1</td>
    <td>Network 1</td>
    <td>Node 3</td>
    <td>State 2</td>
    <td>0.8</td>
  </tr>
  <tr>
    <td>Scenario 1</td>
    <td>Network 1</td>
    <td>Node 3</td>
    <td>State 3</td>
    <td>0.1</td>
  </tr>
</tbody>
</table>

## 8.3 Sensitivity Analysis

For the sensitivity analysis, first you need to crate a sensivity configuration object, using the `create_sensitivity_config(...)` function. For example,

```r
example_sens_config <- create_sensitivity_config(
                      target = "node_one",
                      sensitivity_nodes = c("node_two","node_three"),
                      report_settings = list(summaryStats = c("mean", "variance")),
                      dataset = "dataSet_id",
                      network = "network_one")
```

Using this config object, now you can use the `sensitivity_analysis()` function to send the request to the server. For example,

```r
sensitivity_analysis(example_model, test_login, example_sens_config)
```

This will return a spreadsheet of tables and a json file for the results. The spreadsheet contains sensitivity analysis results and probability values for each sensitivity node defined in the configuration. The results json file contains raw results data for all analysis report options defined, such as tables, tornado graphs, and curve graphs.

Note that the spreadsheet of tables is not created if there is an .xlsx file with the same name in the directory, then only results json is created.

# 9. Local agena.ai API with R-Agena

Agena.ai has a [Java based API](https://github.com/AgenaRisk/api) to be used with agena.ai developer license. If you have the developer license, you can use the local API for calculations in addition to agena.ai modeller. The local API has Java and maven dependencies, which you can see on its github page in full detail. R-Agena has communication with the local agena developer API.

To manually set up the local agena developer API, follow the instructions on the github page for the API: https://github.com/AgenaRisk/api.

For the API setup, in the R environment you can use

```r
local_api_clone()
```

to clone the git repository of the API in your working directory.

Once the API is cloned, you can compile maven environment with:

```r
local_api_compile()
```

and if needed, activate your agena.ai developer license with

```r
local_api_activate_license("1234-ABCD-5678-EFGH")
```

passing on your developer license key as the parameter.

**!! Note that when there is a new version of the agena developer API, you need to re-run `local_api_compile()` function to update the local repository.**

Once the local API is compiled and developer license is activated, you can use the local API directly with your models defined in R. To use the local API for calculations of a model created in R:

```r
local_api_calculate(model, dataSet, output)
```

where the parameter `model` is an R Model object, `dataSet` is the id of one of the dataSets existing in the Model object, and `output` is the desired name of the output file to be generated with the result values. Note that `output` is just the file name and not the absolute path. For example,

```r
local_api_calculate(model = example_model,
                    dataSet = example_dataset_id,
                    output = "exampe_results.json")
```
This function will create the .cmpx file for the model and the separate .json file required for the dataSet, and send them to the local API (cloned and compiled within the working directory), obtain the calculation result values and create the output file in the working directory, and remove the model and dataSet files used for calculation from the directory. The function also updates the R Model object with the calculation results (in addition to creating the separate results.json file in the directory).

If you'd like to run multiple dataSets in the same model in batch, you can use `local_api_batch_calculate()` instead. This function takes an R Model object as input and runs the calculation for each dataSet in it, and fills in all the relevant result fields under each dataSet. You can use this function as

```r
local_api_batch_calculate(model = example_model)
```

where `example_model` is an R Model object with multiple dataSets.


You can also run a sensitivity analysis in the local API, using

```r
local_api_sensitivity(model, sens_config, output)
```

Here the sens_config is created by the use of `create_sensitivity_config(...)`. For example: 

```r
local_api_sensitivity(model = example_model,
                      sens_config = example_sensitivity_config,
                      output = "example_sa_results.json")
```

This function will create the .cmpx file for the model and the separate .json files required for the dataSet and sensitivity analysis configuration file, and send them to the local API (cloned and compiled within the working directory), obtain the sensitivity analysis result values and create the output file in the working directory, and remove the model, dataSet and config files used for sensitivity analysis from the directory. `local_api_sensitivity()` looks at the `dataSet` field of `sens_config` to determine which dataSet to use, if the field doesn't exist, the default behaviour is to create a new dataSet without any observations for the sensitivity analysis.

# 10. R-Agena Use Case Examples

In this section, some use case examples of R-Agena environment are shown. 

## 10.1 Diet Experiment Model

This is a BN which uses experiment observations to estimate the parameters of a distribution. In the model structure, there are nodes for the parameters which are the underlying parameters for all the experiments and the observed values inform us about the values for these parameters. The model in agena.ai Modeller is given below:

![Diet Experiment Image](https://resources.agena.ai/materials/repos/r_diet_image.png)

In this section we will create this model entirely in RAgena environment. We can start with creating first four nodes. 

Mean and variance nodes:

```r
library(agena.ai)

#First we create the "mean" and "variance" nodes

mean <- Node$new(id = "mean", simulated = TRUE)
mean$set_expressions("Normal(0.0,100000.0)")

variance <- Node$new(id = "variance", simulated = TRUE)
variance$set_expressions("Uniform(0.0,50.0)")
```

Common variance and tau nodes:

```r
#Now we create the "common variance" and its "tau" parameter nodes

tau <- Node$new(id = "tau", simulated = TRUE)
tau$set_expressions("Gamma(0.001,1000.0)")

common_var <- Node$new(id = "common_var", name = "common variance", simulated = TRUE)
common_var$add_parent(tau)
common_var$set_expressions("Arithmetic(1.0/tau)")
```

Now we can create the four mean nodes, using a for loop and list of Nodes:

```r
#Creating a list of four mean nodes, "mean A", "mean B", "mean C", and "mean D"

mean_names <- c("A", "B", "C", "D")
means_list <- vector(mode = "list", length = length(mean_names))

for (i in seq_along(mean_names)) {
  node_id <- paste0("mean",mean_names[i])
  node_name <- paste("mean",mean_names[[i]])
  means_list[[i]] <- Node$new(id = node_id, name = node_name, simulated = TRUE)
  means_list[[i]]$add_parent(mean)
  means_list[[i]]$add_parent(variance)
  means_list[[i]]$set_expressions("Normal(mean,variance)")
}
```

Now we can create the experiment nodes, based on the number of observations which will be entered:

```r
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
    obs_nodes_list[[i]][[j]]$add_parent(common_var)
    obs_nodes_list[[i]][[j]]$add_parent(means_list[[i]])
    this_expression <- paste0("Normal(",this_mean_id,",common_var)")
    obs_nodes_list[[i]][[j]]$set_expressions(this_expression)
  }
}
```

We can create a network for all the nodes:

```r
#Creating the network for all the nodes

diet_network <- Network$new(id = "Hierarchical_Normal_Model_1",
                            name = "Hierarchical Normal Model")
```

And add all the nodes to this network. First eight nodes:

```r
# Adding first eight nodes to the network

for (nd in c(mean, variance, tau, common_var, means_list)) {
  diet_network$add_node(nd)
}
```

Then adding all the experiment nodes:

```r
# Adding all the experiment nodes to the network

for (nds in obs_nodes_list) {
  for (nd in nds) {
    diet_network$add_node(nd)
  }
}
```

Now we can create a model with this network:

```r
# Creating a model with the network

diet_model <- Model$new(networks = list(diet_network),
                        id = "Diet_Experiment_Model")
```

We enter all the observation values to the nodes:

```r
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
```

Now the model is ready with all the information, we can export it to either a .json or a .cmpx file for agena.ai calculations, either locally or on Cloud:

```r
# Creating json or cmpx file for the model
diet_model$to_json()
diet_model$to_cmpx()
```