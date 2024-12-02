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

# PUZZLE 1 ---------------------------------------------------------------------
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

  
  # Within each pair, figure out how far apart the two numbers are; you'll need to add up all of those distances. 
  # For example, if you pair up a 3 from the left list with a 7 from the right list, the distance apart is 4; if you pair up a 9 with a 3, the distance apart is 6.

  reconcile_df <- reconcile_df %>% mutate(distance = abs(list1 -list2))
  
  # To find the total distance between the left list and the right list, add up the distances between all of the pairs you found. 
  # In the example above, this is 2 + 1 + 0 + 1 + 2 + 5, a total distance of 11!
  
  total_distance <- sum(reconcile_df$distance)
  
  total_distance
  
# PUZZLE 2 ---------------------------------------------------------------------
  
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
  
# PUZZLE 1 ---------------------------------------------------------------------
  
  # The unusual data (your puzzle input) consists of many reports, one report per line. 
  # Each report is a list of numbers called levels that are separated by spaces. For example:  
  
  # data <- c(7, 6, 4, 2, 1,
  #           1, 2, 7, 8, 9,
  #           9, 7, 6, 2, 1,
  #           1, 3, 2, 4, 5,
  #           8, 6, 4, 4, 1,
  #           1, 3, 6, 7, 9)
  #   
  #   
  # data_df <- data.frame(matrix(unlist(data), nrow=6, byrow=TRUE))
   
  data_df <- read.delim(paste0(input_dir, "D2_P1.txt"),
                      sep = " ", 
                      header = FALSE) 

  colnames(data_df) <- paste0("level_", seq(1:ncol(data_df)))
  
  # The engineers are trying to figure out which reports are safe. 
  # The Red-Nosed reactor safety systems can only tolerate levels that are either gradually increasing or gradually decreasing. 
  # So, a report only counts as safe if both of the following are true:
  #   - The levels are either all increasing or all decreasing.
  #   - Any two adjacent levels differ by at least one and at most three.
  
  # Create saveout dataframe
  data_df_mod <- data_df

  # Check each report (row)
  for(r in 1:nrow(data_df)){
    
    sel_row <- as.numeric(data_df[r,]) 
    
    #not all records have the same number of levels. Remove NAs. 
    sel_row <- sel_row[!is.na(sel_row)]
    
    #Calculate change between levels (left to right)
    lag_diff <- diff(sel_row)
    
    # Determine if monotonic (all increasing or all decreasing):
    all_decreasing <- prod(lag_diff < 0) == 1
    all_increasing <- prod(lag_diff > 0) == 1
    monotonic <- sum(all_decreasing + all_increasing) > 0
    
    # Check that adjacent levels differ by at least one and at most 3
    abs_lag_diff <- abs(lag_diff)
    acceptable_diff <- (max(abs_lag_diff) <= 3) & (min(abs_lag_diff) >= 1)
    
    safe <- monotonic & acceptable_diff
    
    data_df_mod$monotinic[r] <- monotonic
    data_df_mod$acceptable_diff[r] <- acceptable_diff
    
    data_df_mod$safe[r] <- safe
    
  }
  
  data_df_mod
  n_safe_reports <- sum(data_df_mod$safe)
  n_safe_reports
  
# PUZZLE 2 ---------------------------------------------------------------------
  
  # The Problem Dampener is a reactor-mounted module that lets the reactor safety systems tolerate a single bad level in what would otherwise be a safe report. It's like the bad level never happened!
  # Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, the report instead counts as safe.  
   
  data <- c(7, 6, 4, 2, 1,
            1, 2, 7, 8, 9,
            9, 7, 6, 2, 1,
            1, 3, 2, 4, 5,
            8, 6, 4, 4, 1,
            1, 3, 6, 7, 9)


  data_df <- data.frame(matrix(unlist(data), nrow=6, byrow=TRUE))

  colnames(data_df) <- paste0("level_", seq(1:ncol(data_df)))
  
  # Create saveout dataframe
  data_df_mod <- data_df
  
  # Check each report (row)
  for(r in 1:nrow(data_df)){
    
    #reset fixable checker
    fixable = NA
    
    sel_row <- as.numeric(data_df[r,]) 
    
    #not all records have the same number of levels. Remove NAs. 
    sel_row <- sel_row[!is.na(sel_row)]
    
    #Calculate change between levels (left to right)
    lag_diff <- diff(sel_row)
    
    # Determine if monotonic (all increasing or all decreasing):
    all_decreasing <- prod(lag_diff < 0) == 1
    all_increasing <- prod(lag_diff > 0) == 1
    monotonic <- sum(all_decreasing + all_increasing) > 0

      #if not monotonic, identify levels that creates the issue: 
      if(monotonic == FALSE){
        
        n_decreasing <- sum(lag_diff < 0)
        n_increasing <- sum(lag_diff > 0)
        
        if(n_decreasing > 2 & n_increasing > 2){
          fixable = FALSE
          next
        }else{
          remove_dec <- n_decreasing < n_increasing 
          
          #If TRUE, identify decrease (negatives) for removal. Else, identify increase (positives)
          if(remove_dec == TRUE) {
            i_drop <- which(lag_diff <= 0) + 1 #index in original row is 1 greater than lag_diff
            fix_row <- sel_row[-i_drop]
          }else{
            i_drop <- which(lag_diff >= 0) + 1 #index in original row is 1 greater than lag_diff
            fix_row <- sel_row[-i_drop]
          }
          #Check if fixed row passes acceptable difference check
          
          lag_diff_fix <-  diff(fix_row)
          abs_lag_diff_fix <- abs(lag_diff_fix)
          acceptable_diff_fix <- (max(abs_lag_diff_fix) <= 3) & (min(abs_lag_diff_fix) >= 1)
          
          if(acceptable_diff_fix == TRUE){
            fixable = TRUE
          }
        }
      } 
      

    # Check that adjacent levels differ by at least one and at most 3
    abs_lag_diff <- abs(lag_diff)
    acceptable_diff <- (max(abs_lag_diff) <= 3) & (min(abs_lag_diff) >= 1)
    
    # If monotonic, but difference is outside acceptable range, check if only one or more than one bad level creates the issue
    if(acceptable_diff == FALSE){
      
      too_big <- sum(abs_lag_diff > 3)
      too_small <- sum(abs_lag_diff < 1)
      
      if(too_big+too_small > 1){
        fixable = FALSE
      }else{
        remove_big <- too_big > too_small
        
        #If TRUE, identify changes of >3 for removal. Else, identify changes < 1 for removal.
        if(remove_big == TRUE){
          i_drop <- which(abs_lag_diff > 3)
          fix_row <- sel_row[-i_drop]
        }else{
          i_drop <- which(abs_lag_diff < 1)
          fix_row <- sel_row[-i_drop]
        }
        
        #Check if fixed row passes acceptable difference check
        lag_diff_fix <-  diff(fix_row)
        abs_lag_diff_fix <- abs(lag_diff_fix)
        acceptable_diff_fix <- (max(abs_lag_diff_fix) <= 3) & (min(abs_lag_diff_fix) >= 1)
        
        if(acceptable_diff_fix == TRUE){
          fixable = TRUE
          
        }
      } 
    } 
    
    
      # if(acceptable_diff == FALSE){
      #   #Check if both highest and lowest are issues. If so, cannot be fixed. 
      #   if(sum((max(abs_lag_diff) > 3), (min(abs_lag_diff) < 1))> 1) {
      #     fixable_2 <- "no"
      #   }else{
      #     if(max(abs_lag_diff) > 3){}
      #   }
      #   
      #   #
      #   
      # }
      # 
    safe <- monotonic & acceptable_diff
    
    data_df_mod$monotonic[r] <- monotonic
    data_df_mod$acceptable_diff[r] <- acceptable_diff
    
    data_df_mod$safe[r] <- safe
    
    data_df_mod$fixable[r] <- fixable

  }
  
  data_df_mod$pass <- data_df_mod$safe|data_df_mod$fixable
  data_df_mod
  n_pass_reports <- sum(data_df_mod$pass, na.rm = TRUE)
  n_pass_reports
  
  
  