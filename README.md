# 1. Description

RAgena is an R environment for AgenaRisk. The environment allows users to read and modify Bayesian networks from .cmpx model files, create new Bayesian networks in R and export to .cmpx and .json files.

# 2. Prerequisites

RAgena requires `rjson` package installed.

```r
install.packages('rjson')
```

# 3. Structure
<a name="structure"></a>

The Bayesian networks (BNs) in the R environment are represented with several objects: `Node`, `Network`, `DataSet`, and `Model`. These R objects generally follow their definition in AgenaRisk.

## 3.1 `Node` objects

These represent the nodes in a BN. The fields that define a `Node` object are as follows:

### `id`

Mandatory field to create a new `Node` object. This is the unique identifier of AgenaRisk nodes.

### `name`

Name of the node, optional. If not defined, `id` of the node will be passed onto the `name` field too.

### `description`

Description of the node, optional. If not defined, "New Node" will be assigned to the `description` field.

### `type`

Node type, it can be:

* Boolean
* Labelled
* Ranked
* DiscreteReal
* ContinuousInterval
* IntegerInterval

If it's not specified when creating a new node, the new node is "Boolean" by default if it's not a simulation node; and it is "ContinuousInterval" by default if it's a simulation node.

### `parents`

Other `Node` objects can be pointed as parents of a `Node` object. It is not recommended to modify this field manually, to add parents to a node, see the function `addParent()`.

Something to keep in mind: the parent-child relationship information is stored at `Node` level in R environment thanks to this field, as opposed to the separate `links` field of a .cmpx/.json file for the AgenaRisk models. When importing or exporting .cmpx files you do not need to think about this difference as the cmpx parser and writer functions handle the correct formats. This difference allows adding and removing `Node` objects as parents 

### `simulated`

A boolean field to indicate whether the node is a simulation node or not.

### `distr_type`

The table type of the node, it can be:

* Manual
* Expression
* Partitioned



### `states`

States of the node (if not simulated). If states are not specified, depending on the `type`, sensible default states are assigned.

### `probabilities`

NPT values

### `expressions`

expressions

### `partitions`

parent nodes the partitioned expression is based on

### `variables`

variables/constants

## `Network` objects 

These represent each network in a BN.

### `id`

id

### `name`

name, if not given it's id

### `description`

description, if not given it's "New Network"

### `nodes`

Node objects as nodes in the network

Note that `Network` objects do not have a `links` field unlike the AgenaRisk models. As explained in `Node$parents` section above, this information is stored in `Node` objects in the R environment. When importing a .cmpx model, the information in `links` field is used to populate `Node$parents` fields for each node. Similarly, when exporting to a .cmpx/.json file, the parent-child information in `Node$parents` field is used to create the `links` field of the `Network` field of the .cmpx/.json.

## `DataSet` objects 

These represent the set of observations in a BN.

### `id`

id of the scenario

### `observations`

observations for nodes in networks (states or values)

### `results`

This field is defined only for when a .cmpx model is imported

## `Model` objects

These represent the overall BN.

## `id`

id 

## `networks`

all the network objects

## `dataSets`

by default there is Scenario 1

## `networkLinks`

to create these links, see `addNetworkLink()` function below

# 4. Class Methods

## `Node` methods

addParent = function(newParent)

(also see addParent_byID = function(newParentID, varList))

removeParent = function(oldParent)

getParents = function() 

setDistributionType = function(new_distr_type) 

setProbabilities = function(new_probs, by_rows=TRUE)

setExpressions = function(new_expr,partition_parents=NULL) 

## `Network` methods

addNode = function(newNode)

removeNode = function(oldNode) 

getNodes = function() 

## `Model` methods

addNetworkLink = function(outNetwork,outNode,inNetwork,inNode,linkType)

create_scenario = function(id)

enter_observation = function(scenario=NULL, node, network, value)

clear_all_observations = function()

to_cmpx = function(filename=NULL)

to_json = function(filename=NULL)

# 5. Importing a Model from .cmpx

To import an existing AgenaRisk model (from a .cmpx file), use the `from_cmpx()` function:

```r
new_model <- from_cmpx("/path/to/model/file.cmpx")
```

This creates an R `Model` object with all the information taken from the .cmpx file. All fields and sub-fields of the `Model` object (as per the [Structure](#structure) section) are accessible now. For example, you can see the networks in this model with:

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


# 6. Creating a Model in R

...

## Creating Nodes

...

## Creating Networks

...

## Creating the Model

...

## Exporting to .cmpx or .json

...

# 7. Use Case Examples

examples of model creation from scratch with code pieces here, for different models