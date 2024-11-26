################################################################################
#                                                                             
# Title: Adevent of Code 2024
# Author: Sophia Crouch
# Start date: 26 November 2024
# Edit date: 1 December 2024
#
################################################################################


### Setup ######################################################################

# Working directory
source_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(source_path))

# Directories
input_dir = ".//inputs//"
output_dir = ".//outputs//"

# Libraries
library(tidyverse)
library(readxl)

# Define functions
`%notin%` = function(x,y) {!(x %in% y)}

### Parameters #################################################################