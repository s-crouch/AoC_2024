################################################################################
#                                                                             
# Title: Advent of Code 2024
# Author: Sophia Crouch
# Start date: 26 November 2024
# Edit date: 1 December 2024
#
################################################################################

# https://adventofcode.com/

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

### Day 1 ######################################################################

# PUZZLE 1
  # There's just one problem: by holding the two lists up side by side (your puzzle input), it quickly becomes clear that the lists aren't very similar. Maybe you can help The Historians reconcile their lists?
  
  lists <- read.delim(paste0(input_dir, "D1_P1.txt"),
                      sep = " ", #not parsing as tabs? 
                      header = FALSE) %>% 
           select(list1 = V1, list2 = V4)
  
  # Maybe the lists are only off by a small amount! 
  # To find out, pair up the numbers and measure how far apart they are. 
  # Pair up the smallest number in the left list with the smallest number in the right list, then the second-smallest left number with the second-smallest right number, and so on.
  
  reconcile_df <- lists %>% mutate(list1 = sort(list1), 
                                   list2 = sort(list2))

  
  #Within each pair, figure out how far apart the two numbers are; you'll need to add up all of those distances. 
  #For example, if you pair up a 3 from the left list with a 7 from the right list, the distance apart is 4; if you pair up a 9 with a 3, the distance apart is 6.

  reconcile_df <- reconcile_df %>% mutate(distance = abs(list1 -list2))
  
  #To find the total distance between the left list and the right list, add up the distances between all of the pairs you found. 
  # In the example above, this is 2 + 1 + 0 + 1 + 2 + 5, a total distance of 11!
  
  total_distance <- sum(reconcile_df$distance)
  
  total_distance
  
# PUZZLE 2
  
  #A lot of location IDs appear in both lists! Maybe the other numbers aren't location IDs at all but rather misinterpreted handwriting.
  # This time, you'll need to figure out exactly how often each number from the left list appears in the right list. 
  # Calculate a total similarity score by adding up each number in the left list after multiplying it by the number of times that number appears in the right list.
  
  # list1 <- c(3, 4, 2, 1, 3, 3)
  # list2 <- c(4, 3, 5, 3, 9, 3)
  # list1 <- list1 %>% sort()
  # list2 <- list2 %>% sort()
  # reconcile_df <- as.data.frame(cbind(list1, list2))
  # reconcile_df <- reconcile_df %>% mutate(distance = abs(list1 -list2))
  
  similarity_score <- 0
  
  for(i in reconcile_df$list1){
    
    #Check for occurrence of lefthand list item in righthand list
    common_check <- reconcile_df$list2 == i
    
    #count number of occurrences
    common_sum <- sum(common_check)
    
    #calculate running similarity score
    similarity_score <- similarity_score + (common_sum * i)

  }
  
  similarity_score
  
### Day 2 ######################################################################
