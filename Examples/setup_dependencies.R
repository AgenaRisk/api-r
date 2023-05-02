# installing the dependency packages

install.packages('rjson')
install.packages('httr')
install.packages('openxlsx')
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("Rgraphviz")


