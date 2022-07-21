# Table of Contents

* [Description](#1-description)
* [Prerequisites](#2-prerequisites)
* [Structure of RAgena Classes](#3-structure-of-ragena-classes)
* [Class Methods](#4-class-methods)
* [Importing a Model from .cmpx](#5-importing-a-model-from-cmpx)
* [Creating and Modifying a Model in R](#6-creating-and-modifying-a-model-in-r)
* [Creating Batch Cases for a Model in R](#7-creating-batch-cases-for-a-model-in-r)
* [RAgena Use Case Examples](#8-ragena-use-case-examples)

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

### 4.3.1 `addNetwork(newNetwork)`

A method to add a new `Network` object to the `networks` field of a `Model` object. The input `newNetwork` is a `Network` object and it is added to the model if it's not already in it.

### 4.3.2 `removeNetwork(oldNetwork)`

A method to remove an existing `Network` object from the model. Note that removing a Node from a network doesn't automatically remove its possible network links to other networks in the model. `networkLinks` field of a `Model` should be adjusted accordingly if needed.

### 4.3.3 `getNetworks()`

A method to see `id`s of all the networks in a model.

### 4.3.4 `addNetworkLink(outNetwork, outNode, inNetwork, inNode, linkType)`

This is the method to add links to a model between its networks. These links start from an "output node" in a network and go to an "input node" in another network. To create the link, the output and input nodes in the networks need to be specified together with the network they belong to (by the `Node` and `Network` `id`s). The input parameters are as follows:

* `outNetwork` = `Network$id` of the network the output node belongs to
* `outNode` = `Node$id` of the output node
* `inNetwork` = `Network$id` of the network the input node belongs to
* `inNode` = `Node$id` of the input node
* `linkType` = a string of the link type name, one of the defined link types in AgenaRisk

### 4.3.5 `create_scenario(id)`

It is possible to add multiple scenarios to a model. These scenarios are new `DataSet` objects added to the `dataSets` field of a model. Initially these scenarios have no observations and are only defined by their `id`s. The scenarios are populated with the `enter_observation()` function.

### 4.3.6 `remove_scenario(oldScenario)`

A method to remove an existing scenario from the model. Input parameter `oldScenario` is the string which is the `id` of a scenario (`dataSet` object).

### 4.3.7 `get_scenarios()`

A method to list the `id`s of all existing scenarios in a model.

### 4.3.8 `enter_observation(scenario = NULL, node, network, value)`

A method to enter observation to a model. To enter the observation to a specific scenario, the scenario id must be given as the input parameter `scenario`. If `scenario` is left NULL, the entered observation will by default go to "Scenario 1". This means that if there is no extra scenarios created for a model (which by default comes with "Scenario 1"), any observation entered will be set for this scenario (mimicking the behaviour of entering observation in AgenaRisk Desktop).

The observation is defined with the mandatory input parameters:
* `node` = `Node$id` of the observed node
* `network` = `Network$id` of the network the observed node belongs to
* `value` = the value or state of the observation for the observed node

### 4.3.9 `clear_all_observations()`

A method to clear all observations defined in a model. This function removes all observations from all scenarios.

### 4.3.10 `to_cmpx(filename = NULL)`

A method to export the `Model` to a .cmpx file. This method passes on all the information about the model, its datasets, its networks, and their nodes (and default settings) to a .cmpx file in the correct format readable by AgenaRisk. If the input parameter `filename` is not specified, it will use the `Model$id` for the filename.

### 4.3.11 `to_json(filename = NULL)`

A method to export the `Model` to a .json file. This method passes on all the information about the model, its datasets, its networks, and their nodes (and default settings) to a .json file in the correct format readable by AgenaRisk. If the input parameter `filename` is not specified, it will use the `Model$id` for the filename.

## 4.4 Other RAgena Functions

RAgena environment provides certain other functions outside the class methods.

### 4.4.1 `from_cmpx(modelPath = "/path/to/model/file.cmpx")`

This is the cmpx parser function to import a .cmpx file and create R objects based on the model in the file. To see its use, see [Section 5](#5-importing-a-model-from-cmpx) and [Section 10](#10-ragena-use-case-examples).

### 4.4.2 `create_batch_cases(inputModel, inputData)`

This function takes an R `Model` object (`inputModel`) and an input CSV file (`inputData`) with observations defined in the correct format and creates a batch of scenarios for each row in the input dataset. and generates a .json file. To see its use and the correct format of the CSV file for a model's data, see [Section 7](#7-creating-batch-cases-for-a-model-in-r).

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

# 6. Creating and Modifying a Model in R

It is possible to create an AgenaRisk model entirely in R, without a .cmpx file to begin with. Once all the networks and nodes of a model are created and defined in R, you can export the model to a .cmpx or .json file to be used with AgenaRisk calculations and inference. In this section, creating a model is shown step by step, starting with nodes.

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

Other fields can be specified with the relevant set functions. To set probability values for a node with a manual table (distr_type), you can use `setProbabilities()` function:

```r
node_one$setProbabilities(list(0.2,0.8))
```

Note that the `setProbabilities()` function takes a `list` as input, even when the node has no parents and its NPT has only one row of probabilities. If the node has parents, the NPT will have multiple rows which should be in the input list.

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

There are two ways to order the values in this table for the `setProbabilities()` function, using the boolean `by_rows` parameter. If you want to enter the values following the rows in AgenaRisk NPT rather than ordering them by the combination of parent states (columns), you can use `by_rows = TRUE` where each element of the list is a row of the AgenaRisk NPT:

```r
node_three$setProbabilities(list(c(0.1, 0.2, 0.3, 0.4), c(0.4, 0.45, 0.6, 0.55), c(0.5, 0.35, 0.1, 0.05)), by_rows = TRUE)
```

If, instead, you want to define the NPT with the probabilities that add up to 1 (conditioned on the each possible combination of parent states), you can set `by_rows = FALSE` as the following example:

```r
node_three$setProbabilities(list(c(0.1, 0.4, 0.5), c(0.2, 0.45, 0.35), c(0.3, 0.6, 0.1), c(0.4, 0.55, 0.05)), by_rows = FALSE)
```

Similarly, you can use `setExpressions()` function to define and update expressions for the nodes without Manual NPT tables. If the node has no parents, you can add a single expression:

```r
example_node$setExpressions("TNormal(4,1,-10,10)")
```

Or if the node has parents and the expression is partitioned on the parents:

```r
example_node$setExpressions(c("Normal(90,10)", "Normal(110,15)", "Normal(120,30)"), partition_parents = "parent_node")
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

Here clearly `nodes` field is the most important information for a network but you do not need to specify these on creation. You can choose to create an empty network and fill it in with the nodes afterwards with the use of `addNode()` function. Alternatively, if all (or some) of the nodes you will have in the network are already defined, you can pass them to the new `Network` object on creation.

Below is an example of network creation with the nodes added later:

```r
network_one <- Network$new(id = "network_one")

network_one$addNode(node_three)
network_one$addNode(node_one)
network_one$addNode(node_two)
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

network_three$addNode(node_two)
```

To remove a node from a network, you can use `removeNode()` function. Again keep in mind that removing a node does not automatically remove all of its parents from the network. For example,

```r
network_three$removeNode(node_three)
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

Note that even when there is only one network in the model, the input has to be a list. Networks in a model can be modified with `addNetwork()` and `removeNetwork()` functions:

```r
example_model$addNetwork(network_two)
```

```r
example_model$removeNetwork(network_two)
```

Network links between networks of the model can be added with the `addNetworkLink()` function. For example:

```r
example_model$addNetworkLink(outNetwork = network_one, outNode = node_three, inNetwork = network_two, inNode = node_three, linkType = "Marginals")
```

When a new model is created, it comes with a single scenario (dataSet element) by default. See next section to see how to add observations to this scenario or add new scenarios.

## 6.6 Creating Scenarios and Entering Observation

To enter observations to a Model (which by default has one single scenario), use the `enter_observation()` function. You need to specify the node (and the network it belongs to) and give the value (one of the states if it's a discrete node, a sensible numerical value if it's a continuous node):

```r
example_model$enter_observation(node = node_three, network = network_one, value = "High")
```

Note that this function did not specify any scenario. If this is the case, observation is always entered to the first (default) scenario.

You may choose to add more scenarios to the model with the `create_scenario()` function:

```r
example_model$create_scenario("Scenario 2")
```

Once added, you can enter observation to the new scenario if you specify the `scenario` parameter in the `enter_observation()` function:

```r
example_model$enter_observation(scenario = "Scenario 2", node = node_three, network = network_one, value = "Medium")
```

## 6.7. Exporting a Model to .cmpx or .json

Once an R model is defined fully and it is ready, you can export it to a .cpmx or a .json file. The function to create these files convert the information to the correct format for AgenaRisk to understand. You can use either of the functions:

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

RAgena environment allows creation of batch cases based on a single model and multiple observation sets. 



# 8. RAgena Use Case Examples

examples of model creation from scratch with code pieces here, for different models

-diet exercise model