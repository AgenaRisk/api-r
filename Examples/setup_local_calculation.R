#setwd("/Users/user/repos/api-r")

library("RAgena.R")

# Creating the local agena.ai developer API environment
# Requires git, Java, and maven

local_api_clone() 
local_api_compile()
local_api_activate_license("1234-ABCD-5678-EFGH")