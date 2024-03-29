% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RAgena.R
\docType{class}
\name{Node-class}
\alias{Node-class}
\alias{Node}
\title{BN Node object}
\description{
These represent the nodes in a BN.
}
\section{Fields}{

\describe{
\item{\code{id}}{node id}

\item{\code{name}}{node display name}

\item{\code{description}}{node description}

\item{\code{type}}{node type}

\item{\code{parents}}{node parent nodes}

\item{\code{simulated}}{whether node is simulated}

\item{\code{distr_type}}{node distribution type}

\item{\code{states}}{node states}

\item{\code{probabilities}}{node probabilities}

\item{\code{expressions}}{node expressions}

\item{\code{partitions}}{node expression partitions}

\item{\code{variables}}{node variables or constants}
}}

\section{Methods}{

\describe{
\item{\code{addParent_byID(newParentID, varList)}}{This is a method to add parent Nodes by their ids for cmpx parser capabilities.
To add parents to Node objects, please use $add_parent(Node) method.}

\item{\code{add_parent(newParent)}}{Adds a Node object as a new parent node to the current Node object and resets/resizes the NPT values and expressions of the Node as needed.
Parents list of a Node object is a list of other Node objects.
The input parameter of the function is a Node object variable. A good practice is to use Node ids as their variable names.}

\item{\code{get_parents()}}{A method to list all the existing parent nodes of a Node.}

\item{\code{initialize(
  id,
  name = NULL,
  description = NULL,
  type = NULL,
  simulated = FALSE,
  states = NULL
)}}{Creates a new Node object, a unique id is required, other fields are filled with defaults unless specified.
Node id, name, description, type, states, and whether it is a simulation or regular node do not depend on its edges and parents in a network,
a Node object can be defined with all this information outside a Network as well.
To add/remove/modify parents, distr_type, probabilities, expressions, and partitions; use the corresponding method.}

\item{\code{remove_parent(oldParent)}}{Removes a Node object from parents of the current Node object and resets/resizes the NPT values and expressions of the Node as needed.
The input parameter of the function is a Node object variable. A good practice is to use Node ids as their variable names.}

\item{\code{remove_variable(variable_name)}}{A method to remove one of the existing variables (constants) from a node, using the variable_name.}

\item{\code{set_distribution_type(new_distr_type)}}{A method to set the table type (distr_type) of a node. If a Node is simulated,
its table type can be "Expression" or "Partitioned" - the latter is only if the
node has parent nodes. If a Node is not simulated, its table type can be "Manual",
"Expression", or "Partitioned Expression (if the node has parent nodes)".}

\item{\code{set_expressions(new_expr, partition_parents = NULL)}}{The method to set the probability values if the table type (distr_type) of a Node is "Expression"
or "Partitioned". If the table type is "Expression", new_expr is a single string and partition_parents
is left NULL. If the table type is "Partitioned", new_expr is a list of expressions for each parent
state, and partition_parents is a list of strings for each partitioned parent node id.}

\item{\code{set_probabilities(new_probs, by_rows = TRUE)}}{The method to set the probability values if the table type (distr_type) of a Node is "Manual".
                        new_probs is a list of numerical values, and the length of the input list depends on the number
                        of the states of the node and of its parents.

                        You can format the input list in two different orders. If the parameter by_rows is set to true,
                        the method will read the input list to fill in the NPT row by row; if set to false, the method
                        will read the input list to fill in the NPT column by columnn. See README.md for examples.}

\item{\code{set_variable(variable_name, variable_value)}}{A method to set variables (constants) for a node. Takes the variable_name and
variable_value inputs which define a new variable (constant) for the node.}
}}

