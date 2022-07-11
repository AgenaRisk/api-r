# Table of Contents

* [Description](#1-description)
* [Prerequisites](#2-prerequisites)
* [Structure of RAgena Classes](#3-structure-of-ragena-classes)
* [Class Methods](#4-class-methods)
* [Importing a Model from .cmpx](#5-importing-a-model-from-cmpx)
* [Creating a Model in R](#6-creating-a-model-in-r)
* [Modifying a Model in R](#7-modifying-a-model-in-r)
* [Exporting a Model to .cmpx or .json](#8-exporting-a-model-to-cmpx-or-json)
* [Creating Batch Cases for a Model in R](#9-creating-batch-cases-for-a-model-in-r)
* [RAgena Use Case Examples](#10-ragena-use-case-examples)

# 1. Description

RAgena is an R environment for AgenaRisk. The environment allows users to read and modify Bayesian networks from .cmpx model files, create new Bayesian networks in R and export to .cmpx and .json files.

# 2. Prerequisites

RAgena requires `rjson` package installed.

```r
install.packages('rjson')
```

# 3. Structure of RAgena Classes

The Bayesian networks (BNs) in the R environment are represented with several objects: `Node`, `Network`, `DataSet`, and `Model`. These R objects generally follow their definition in AgenaRisk.

## 3.1 `Node` objects

These represent the nodes in a BN. The fields that define a `Node` object are as follows:

### 3.1.1 `id`

Mandatory field to create a new `Node` object. This is the unique identifier of AgenaRisk nodes.

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

Something to keep in mind: the parent-child relationship information is stored at `Node` level in R environment thanks to this field, as opposed to the separate `links` field of a .cmpx/.json file for the AgenaRisk models. When importing or exporting .cmpx files you do not need to think about this difference as the cmpx parser and writer functions handle the correct formats. This difference allows adding and removing `Node` objects as parents 

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

To see how to set the expressions for a node, see `setExpressions()` function.

### 3.1.11 `partitions`

If the table type (`distr_type`) of the node is "Partitioned", in addition to the expressions, the node will have the `partitions` field. This field is a list of strings, which are `id`s of the parent nodes on which the node expression is partitioned.

### 3.1.12 `variables`

The node variables are called constants on AgenaRisk desktop. This field, if specified, sets the constant value for the node observations.

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

Note that `Network` objects do not have a `links` field unlike the AgenaRisk models. As explained in `Node$parents` section above, this information is stored in `Node` objects in the R environment. When importing a .cmpx model, the information in `links` field is used to populate `Node$parents` fields for each node. Similarly, when exporting to a .cmpx/.json file, the parent-child information in `Node$parents` field is used to create the `links` field of the `Network` field of the .cmpx/.json.

## 3.3 `DataSet` objects 

These represent the set of observations in a BN. A `Model` can have multiple `DataSet` objects in its `dataSets` field.  When a new `Model` is created, it always comes with a default `DataSet` object with the `id` "Scenario 1" and with blank observations. It is possible to add more scenarios with their `id`s. Each `DataSet` object under a `Model` can be called a new "scenario".

### 3.3.1 `id`

Id of the scenario.

### 3.3.2 `observations`

Under each scenario, observations for all the observed nodes in all the networks of the model (in terms of their states or values) are listed. If it's hard evidence, observation for a node will have a single value with the weight of 1. If a node in the model has a value in its `variable` field, this value will be passed onto the scenario with the weight of 1.

### 3.3.3 `results`

This field is defined only for when a .cmpx model with calculations is imported. When creating a new BN in the R environment, this field is not created or filled in. The `results` field stores the posterior probability and inference results upon model calculation on AgenaRisk servers.

## 3.4 `Model` objects

These represent the overall BN. A single .cmpx file corresponds to a singe `Model`. A BN model can have multiple networks with their own nodes, links between these networks, and datasets (observations/scenarios). 

## 3.4.1 `id`

Id of the Model, optional. If not specified, the `id` of the first `Network` in the model's `networks` field is used to create a `Model$id`. 

## 3.4.2 `networks`

A list of all the `Network` objects that make up the model. This field is mandatory for creating a new `Model` object. 

## 3.4.3 `dataSets`

Optional field for `DataSet` objects (scenarios). When creating a new `Model`, it is possible to use predefined scenarios as long as the `DataSet$observations` field of these scenarios have matching `id`s with the nodes in the model. If none is specified, by default a new `Model` object will come with an empty scenario called "Scenario 1".

## 3.4.4 `networkLinks`

If the `Model` has multiple networks, it is possible to have links between these networks, following the AgenaRisk networkLinks format.

To see how to create these links, see `addNetworkLink()` function later in this document.

# 4. Class Methods

The `Node`, `Network`, and `Model` objects have their own respective methods to help their definition and manipulate their fields. The R class methods are used with the `$` sign following an instance of the class. For example,

```r
example_node$addParent(exampleParentNode)
```

or

```r
example_network$removeNode(exampleNode)
```

or

```r
example_model$create_scenario(exampleScenario)
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

### 4.1.1 `addParent(newParent)`

The method to add a new parent to a node. Equivalent of adding an arc between two nodes on AgenaRisk Desktop. The input parameter `newParent` is another `Node` object. If `newParent` is already a parent for the node, the function does not update the `parents` field of the node.

When a new parent is added to a node, its NPT values and expressions are reset/resized accordingly. 

There is also a method called `addParent_byID(newParentID, varList)`, however, this is only used in the cmpx parser. To add a new parent to a `Node`, it is recommended to use `addParent()` function with a `Node` object as the input.

### 4.1.2 `removeParent(oldParent)` 

The method to remove one of the existing parents of a node. Equivalent of removing the arc between two nodes on AgenaRisk Desktop. The input parameter `oldParent` is a `Node` object which has already been added to the `parents` field of the node.

When an existing parent is removed from a node, its NPT values and expressions are reset/resized accordingly.

### 4.1.3 `getParents()`

A method to list all the existing parent nodes of a `Node`.

### 4.1.4 `setDistributionType(new_distr_type)`

A method to set the table type (`distr_type`) of a node. If a `Node` is `simulated`, its table type can be "Expression" or "Partitioned" - the latter is only if the node has parent nodes. If a `Node` is `not simulated`, its table type can be "Manual", "Expression", or "Partitioned Expression (if the node has parent nodes)".

### 4.1.5 `setProbabilities(new_probs, by_rows = TRUE)`

The method to set the probability values if the table type (`distr_type`) of a `Node` is "Manual". `new_probs` is a list of numerical values, and the length of the input list depends on the number of the states of the node and of its parents.

You can format the input list in two different orders. If the parameter `by_rows` is set to true, the method will read the input list to fill in the NPT row by row; if set to false, the method will read the input list to fill in the NPT column by columnn. This behaviour is illustrated with use case examples later in this document.

### 4.1.6 `setExpressions(new_expr, partition_parents = NULL)`
The method to set the probability values if the table type (`distr_type`) of a `Node` is "Expression" or "Partitioned". If the table type is "Expression", `new_expr` is a single string and `partition_parents` is left NULL. If the table type is "Partitioned", `new_expr` is a list of expressions for each parent state, and `partition_parents` is a list of strings for each partitioned parent node's `id`.

### 4.1.7 `setVariables(variables_list)`

A method to set variables (constants) for a node. Takes the `variables_list` input which is a list whose items are lists with fields named in the AgenaRisk variable (constant) definition format: `name = node_id, value = constant_value`. 

## 4.2 `Network` methods

As described above, `Node` objects can be created and manipulated outside a network in the R environment. Once they are defined, they can be added to a `Network` object. Alternatively, a `Network` object can be created first and then its nodes can be specified. The R environment gives the user freedom, which is different from AgenaRisk Desktop where it is not possible to have a node completely outside any network. Once a `Network` object is created, with or without nodes, the following methods can be used to modify and manipulate the object.

### 4.2.1 `addNode(newNode)`

A method to add a new `Node` object to the `nodes` field of a `Network` object. The input `newNode` is a `Node` object and it is added to the network if it's not already in it.

Note that adding a new `Node` to the network does not automatically add its parents to the network. If the node has parents already defined, you need to add all the parent `Node`s separately to the network, too.

### 4.2.2 `removeNode(oldNode)`

A method to remove an existing `Node` object from the network. Note that removing a Node from a network doesn't automatically remove it from its previous parent-child relationships in the network. You need to adjust such relationships separately on `Node` level.

### 4.2.3 `getNodes()`

A method to see `id`s of all the nodes in a network.

## 4.3 `Model` methods

A `Model` object consists of networks, network links, and datasets (and default settings). A new `Model` object can be created with a network (or multiple networks). By default, it is created with a single empty scenario called "Scenario 1". Following methods can be used to modify `Model` objects: 

### 4.3.1 `addNetworkLink(outNetwork, outNode, inNetwork, inNode, linkType)`

This is the method to add links to a model between its networks. These links start from an "output node" in a network and go to an "input node" in another network. To create the link, the output and input nodes in the networks need to be specified together with the network they belong to (by the `Node` and `Network` `id`s). The input parameters are as follows:

* `outNetwork` = `Network$id` of the network the output node belongs to
* `outNode` = `Node$id` of the output node
* `inNetwork` = `Network$id` of the network the input node belongs to
* `inNode` = `Node$id` of the input node
* `linkType` = a string of the link type name, one of the defined link types in AgenaRisk

### 4.3.2 `create_scenario(id)`

It is possible to add multiple scenarios to a model. These scenarios are new `DataSet` objects added to the `dataSets` field of a model. Initially these scenarios have no observations and are only defined by their `id`s. The scenarios are populated with the `enter_observation()` function.

### 4.3.3 `enter_observation(scenario = NULL, node, network, value)`

A method to enter observation to a model. To enter the observation to a specific scenario, the scenario id must be given as the input parameter `scenario`. If `scenario` is left NULL, the entered observation will by default go to "Scenario 1". This means that if there is no extra scenarios created for a model (which by default comes with "Scenario 1"), any observation entered will be set for this scenario (mimicking the behaviour of entering observation in AgenaRisk Desktop).

The observation is defined with the mandatory input parameters:
* `node` = `Node$id` of the observed node
* `network` = `Network$id` of the network the observed node belongs to
* `value` = the value or state of the observation for the observed node

### 4.3.4 `clear_all_observations()`

A method to clear all observations defined in a model. This function removes all observations from all scenarios.

### 4.3.5 `to_cmpx(filename = NULL)`

A method to export the `Model` to a .cmpx file. This method passes on all the information about the model, its datasets, its networks, and their nodes (and default settings) to a .cmpx file in the correct format readable by AgenaRisk. If the input parameter `filename` is not specified, it will use the `Model$id` for the filename.

### 4.3.6 `to_json(filename = NULL)`

A method to export the `Model` to a .json file. This method passes on all the information about the model, its datasets, its networks, and their nodes (and default settings) to a .json file in the correct format readable by AgenaRisk. If the input parameter `filename` is not specified, it will use the `Model$id` for the filename.

## 4.4 Other RAgena Functions

RAgena environment provides certain other functions outside the class methods.

### 4.4.1 `from_cmpx(modelPath = "/path/to/model/file.cmpx")`

This is the cmpx parser function to import a .cmpx file and create R objects based on the model in the file. To see its use, see [Section 5](#5-importing-a-model-from-cmpx) and [Section 10](#10-ragena-use-case-examples).

### 4.4.2 `create_batch_cases(inputModel, inputData)`

This function takes an R `Model` object (`inputModel`) and an input CSV file (`inputData`) with observations defined in the correct format and creates a batch of .json files for each row in the input dataset. To see its use and the correct format of the CSV file for a model's data, see [Section 9](#9-creating-batch-cases-for-a-model-in-r).

# 5. Importing a Model from .cmpx

To import an existing AgenaRisk model (from a .cmpx file), use the `from_cmpx()` function:

```r
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

# 6. Creating a Model in R

It is possible to create an AgenaRisk model entirely in R, without a .cmpx file to begin with. Once all the networks and nodes of a model are created and defined in R, you can export the model to a .cmpx or .json file to be used on AgenaRisk servers for calculations and inference. In this section, creating a model is shown step by step, starting with nodes.

## 6.1 Creating Nodes

...

## 6.2 Creating Networks

...

## 6.3 Creating the Model

...

## 6.4 Creating Scenarios and Entering Observation

...

# 7. Modifying a Model in R

Once an R model is created, either by parsing a .cmpx file or defining it in R using RAgena objects, it is possible to modify and update parts of this model using the RAgena methods. 

# 8. Exporting a Model to .cmpx or .json

Once an R model is defined fully and it is ready

# 9. Creating Batch Cases for a Model in R

create_batch_cases

# 10. RAgena Use Case Examples

examples of model creation from scratch with code pieces here, for different models