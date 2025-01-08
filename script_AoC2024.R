################################################################################
#                                                                             
# Title: Advent of Code 2024
# Author: Sophia Crouch
# Start date: 26 November 2024
# Edit date: 4 December 2024
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
  
# PUZZLE 2 - INCOMPLETE --------------------------------------------------------
  
  # The Problem Dampener is a reactor-mounted module that lets the reactor safety systems tolerate a single bad level in what would otherwise be a safe report. It's like the bad level never happened!
  # Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, the report instead counts as safe.  
   
  # data <- c(7, 6, 4, 2, 1,
  #           1, 2, 7, 8, 9,
  #           9, 7, 6, 2, 1,
  #           1, 3, 2, 4, 5,
  #           8, 6, 4, 4, 1,
  #           1, 3, 6, 7, 9)
  # 
  # 
  # data_df <- data.frame(matrix(unlist(data), nrow=6, byrow=TRUE))

  colnames(data_df) <- paste0("level_", seq(1:ncol(data_df)))
  
  # Create saveout dataframe
  data_df_mod <- data_df

  # Check each report (row)
  for(r in 1:nrow(data_df)){
    
    #reset fixable checker
    fixable = NA
    use_row = NA
    use_ag_diff = NA
    use_abs_lag_diff = NA
    i_gap = NA
    i_drop = NA
    
    sel_row <- as.numeric(data_df[r,]) 
    
    #not all records have the same number of levels. Remove NAs. 
    sel_row <- sel_row[!is.na(sel_row)]
    sel_row #TEMP
    
    #Calculate change between levels (left to right)
    lag_diff <- diff(sel_row)
    lag_diff #TEMP
    
    # Determine if monotonic (all increasing or all decreasing):
    all_decreasing <- prod(lag_diff < 0) == 1
    all_increasing <- prod(lag_diff > 0) == 1
    monotonic <- sum(all_decreasing + all_increasing) > 0
    
    # Determine if adjacent levels differ by at least one and at most 3
    abs_lag_diff <- abs(lag_diff)
    acceptable_diff <- (max(abs_lag_diff) <= 3) & (min(abs_lag_diff) >= 1)
    
    safe <- monotonic & acceptable_diff
    safe #TEMP
    
    #If not safe based on original levels, investigate removal of 1 level
    if(safe == FALSE){
      #Address gap size 
      n_wrong_gap <- sum(abs_lag_diff > 3, abs_lag_diff < 1)
      if(n_wrong_gap > 1) { #Cannot fix if more than one gap of the wrong size
        fixable = FALSE
      }else{
        i_gap <- c(which(abs_lag_diff > 3), which(abs_lag_diff < 1)) #Identify which difference indicates an issue
        
        if(length(i_gap) > 0){
          if(i_gap > 1){
            i_drop <- i_gap + 1
          }else if (i_gap == 1){ #when i_gap == 1, remove first item
            if(sel_row[1] >= sel_row[2]){i_drop = i_gap}
            if(sel_row[2] > sel_row[1]){i_drop = i_gap + 1}
          }
        }

        if(length(i_drop) == 0){
          use_row <- sel_row
          use_lag_diff <- lag_diff
        }else{
          use_row <- sel_row[-i_drop]
          use_lag_diff <- diff(use_row)
        }
        
        #Recheck that modified row meets gap requirements
        use_abs_lag_diff <- abs(use_lag_diff)
        n_wrong_gap_chk <- sum(use_abs_lag_diff > 3, use_abs_lag_diff < 1)
        
        if(n_wrong_gap_chk > 0){
          fixable = FALSE
        }else{
          
          #Check if updated row is monotonic
          all_decreasing_chk <- prod(use_lag_diff < 0) == 1
          all_increasing_chk <- prod(use_lag_diff > 0) == 1
          monotonic_chk <- sum(all_decreasing_chk + all_increasing_chk) > 0
  
          if(monotonic_chk == TRUE) {
            fixable = TRUE
            }else{
              if(length(i_drop) > 0) {#if not monotonic, but length i_drop > 0, out of options. 
                fixable = FALSE
              }else{# If not monotonic, but length i_drop == 0, remove one and re-test
                 
                n_decreasing <- sum(use_lag_diff < 0)
                n_increasing <- sum(use_lag_diff > 0)
                n_flat <- sum(use_lag_diff == 0)
                
                if((max(n_decreasing, n_increasing, n_flat) < length(use_row) - 2 )| n_flat > 1){
                  fixable = FALSE #Cannot fix if prevailing trend contains less than 1 shy of total (2 intervals)
                  }else{
                    #Identify which type of value predominates (thus which needs to be removed)
                    trend <- which.max(c(n_decreasing, n_increasing, n_flat))
                    
                    if(trend == 1) { #If trend == 1 (decreasing), identify and remove increase or flat
                      i_drop <- which(lag_diff >= 0) + 1 #index in original row is 1 greater than lag_diff
                      use_row <- sel_row[-i_drop]
                      use_lag_diff <- diff(use_row)
                      
                    }else if(trend == 2){ #If trend == 2 (increasing), identify and remove decrease or flat
                      i_drop <- which(lag_diff <= 0) + 1 #index in original row is 1 greater than lag_diff
                      use_row <- sel_row[-i_drop]
                      use_lag_diff <- diff(use_row)
                      
                    } else {
                      warning(paste0("Check row ", r))
                    }
                    
                    all_decreasing_fixed <- prod(use_lag_diff < 0) == 1
                    all_increasing_fixed <- prod(use_lag_diff > 0) == 1
                    monotonic_fixed <- sum(all_decreasing_fixed + all_increasing_fixed) > 0
                    
                    #Recheck that modified row meets gap requirements
                    use_abs_lag_diff <- abs(use_lag_diff)
                    n_wrong_gap_chk <- sum(use_abs_lag_diff > 3, use_abs_lag_diff < 1)
                    
                    if(monotonic_fixed & n_wrong_gap_chk == 0){fixable = TRUE}else{fixable = FALSE}
                }
              }
            }
          }
        }
      }
    
    data_df_mod$monotonic[r] <- monotonic
    data_df_mod$acceptable_diff[r] <- acceptable_diff
    
    data_df_mod$safe[r] <- safe
    
    data_df_mod$fixable[r] <- fixable
    
    }
  
  data_df_mod$pass <- data_df_mod$safe|data_df_mod$fixable
  data_df_mod
  n_pass_reports <- sum(data_df_mod$pass, na.rm = TRUE)
  n_pass_reports
    
  #   
  #   
  #   
  # 
  #     #if not monotonic, identify levels that creates the issue: 
  #     if(monotonic == FALSE){
  #       
  #       n_decreasing <- sum(lag_diff < 0)
  #       n_increasing <- sum(lag_diff > 0)
  #       n_flat <- sum(lag_diff == 0)
  #       
  #       if((max(n_decreasing, n_increasing, n_flat) < length(sel_row) - 2 )| n_flat > 1){
  #         fixable = FALSE #Cannot fix if prevailing trend contains less than 1 shy of total (2 intervals) 
  #       }else{
  #         #Identify which type of value predominates (thus which needs to be removed)
  #         trend <- which.max(c(n_decreasing, n_increasing, n_flat))
  # 
  #         if(trend == 1) { #If trend == 1, identify and remove increase or flat
  #           i_drop <- which(lag_diff >= 0) + 1 #index in original row is 1 greater than lag_diff
  #           fix_row <- sel_row[-i_drop]
  #         }else if(trend == 2){ #If trend == 2, identify and remove decrease or flat
  #           i_drop <- which(lag_diff <= 0) + 1 #index in original row is 1 greater than lag_diff
  #           fix_row <- sel_row[-i_drop]
  #         } else {
  #           warning(paste0("Check row ", r))
  #         }
  #         
  #         #Check if fixed row passes acceptable difference check
  #         lag_diff_fix <-  diff(fix_row)
  #         abs_lag_diff_fix <- abs(lag_diff_fix)
  #         print(paste0("Row ", r, " has max abs lag diff of", max(abs_lag_diff_fix)))
  #         acceptable_diff_fix <- (max(abs_lag_diff_fix) <= 3) & (min(abs_lag_diff_fix) >= 1)
  #         
  #         if(acceptable_diff_fix == TRUE & length(i_drop) == 1){
  #           fixable = TRUE
  #         } else {
  #           fixable = FALSE
  #         }
  #       }
  #     } 
  #     
  # 
  #   # Check that adjacent levels differ by at least one and at most 3
  #   abs_lag_diff <- abs(lag_diff)
  #   acceptable_diff <- (max(abs_lag_diff) <= 3) & (min(abs_lag_diff) >= 1)
  # 
  #   # If monotonic, but difference is outside acceptable range, check if only one or more than one bad level creates the issue
  #   if((monotonic == TRUE&(is.na(fixable)|fixable != FALSE)) & acceptable_diff == FALSE){
  # 
  #     too_big <- sum(abs_lag_diff > 3)
  #     too_small <- sum(abs_lag_diff < 1)
  # 
  #     if(too_big+too_small > 1){
  #       fixable = FALSE
  #     }else{
  #       remove_big <- too_big > too_small
  # 
  #       #If TRUE, identify changes of >3 for removal. Else, identify changes < 1 for removal.
  #       if(remove_big == TRUE){
  #         i_drop <- which(abs_lag_diff > 3) + 1
  #         fix_row <- sel_row[-i_drop]
  #       }else{
  #         i_drop <- which(abs_lag_diff < 1) + 1
  #         fix_row <- sel_row[-i_drop]
  #       }
  # 
  #       #Check if fixed row passes acceptable difference check
  #       lag_diff_fix <-  diff(fix_row)
  #       abs_lag_diff_fix <- abs(lag_diff_fix)
  #       acceptable_diff_fix <- (max(abs_lag_diff_fix) <= 3) & (min(abs_lag_diff_fix) >= 1)
  # 
  #       if(acceptable_diff_fix == TRUE){
  #         fixable = TRUE
  #       }
  #     }
  #   }
  # 
  #   data_df_mod$monotonic[r] <- monotonic
  #   data_df_mod$acceptable_diff[r] <- acceptable_diff
  #   
  #   safe <- monotonic & acceptable_diff
  #   data_df_mod$safe[r] <- safe
  #   
  #   data_df_mod$fixable[r] <- fixable
  # 
  # }
  # 
  # data_df_mod$pass <- data_df_mod$safe|data_df_mod$fixable
  # data_df_mod
  # n_pass_reports <- sum(data_df_mod$pass, na.rm = TRUE)
  # n_pass_reports
  # 
  
### Day 3 ######################################################################

# PUZZLE 1 ---------------------------------------------------------------------
  
  # It seems like the goal of the program is just to multiply some numbers. 
  # It does that with instructions like mul(X,Y), where X and Y are each 1-3 digit numbers. 
  # For instance, mul(44,46) multiplies 44 by 46 to get a result of 2024. 
  # Similarly, mul(123,4) would multiply 123 by 4.  
  
  # However, because the program's memory has been corrupted, there are also many invalid characters that should be ignored, 
  # even if they look like part of a mul instruction. Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.
  
  # For example, consider the following section of corrupted memory:
  # xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
  # Only the four highlighted sections are real mul instructions. Adding up the result of each instruction produces 161 (2*4 + 5*5 + 11*8 + 8*5).
  # Scan the corrupted memory for uncorrupted mul instructions. What do you get if you add up all of the results of the multiplications?
  
  # EXAMPLE 
  # corrupt_data <- "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  
  
  filepath <- paste0(input_dir, "D3_P1.txt")
  corrupt_data <- readChar(filepath, file.info(filepath)$size)
  
  mul_statements <- unlist(str_extract_all(corrupt_data, regex('mul\\(\\d{1,3},\\d{1,3}\\)'))) #https://stackoverflow.com/questions/4271553/how-do-i-write-a-regular-expression-to-match-any-three-digit-number-value
  
  mul_statements
  
  num <- mul_statements %>% 
         str_split(pattern = c("mul\\(")) %>% 
         unlist() %>%
         str_split(pattern = c("\\)")) %>% 
         unlist() %>% 
         sort(decreasing = TRUE) %>% 
         str_extract_all("\\d+") %>% 
         unlist()
    
  num_pairs_df <- as.data.frame(matrix(as.numeric(num), ncol = 2, byrow = TRUE))
  
  colnames(num_pairs_df) <- c("N1", "N2")
          
  num_pairs_df <- num_pairs_df %>% mutate(prod = N1 * N2)
  
  prod_sum <- sum(num_pairs_df$prod) 
  prod_sum
  
  
# PUZZLE 2 ---------------------------------------------------------------------
  # As you scan through the corrupted memory, you notice that some of the conditional statements are also still intact. 
  # If you handle some of the uncorrupted conditional statements in the program, you might be able to get an even more accurate result.
  # There are two new instructions you'll need to handle:
    # The do() instruction enables future mul instructions.
    # The don't() instruction disables future mul instructions.
  # Only the most recent do() or don't() instruction applies. At the beginning of the program, mul instructions are enabled.
    
  # For example:
  # xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
  # This corrupted memory is similar to the example from before, 
  # but this time the mul(5,5) and mul(11,8) instructions are disabled because there is a don't() instruction before them. 
  # The other mul instructions function normally, including the one at the end that gets re-enabled by a do() instruction.  
    
  #EXAMPLE  
  # corrupt_data <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  
  filepath <- paste0(input_dir, "D3_P1.txt")
  corrupt_data <- readChar(filepath, file.info(filepath)$size)
  
  # do_statements <- unlist(str_extract_all(corrupt_data, regex('do\\(\\)')))
  # dont_statements <- unlist(str_extract_all(corrupt_data, regex("don't\\(\\)")))
  key_statements <- unlist(str_extract_all(corrupt_data, regex("mul\\(\\d{1,3},\\d{1,3}\\)|do\\(\\)|don't\\(\\)"))) #https://stackoverflow.com/questions/4271553/how-do-i-write-a-regular-expression-to-match-any-three-digit-number-value
  mul_statements <- unlist(str_extract_all(corrupt_data, regex('mul\\(\\d{1,3},\\d{1,3}\\)'))) #https://stackoverflow.com/questions/4271553/how-do-i-write-a-regular-expression-to-match-any-three-digit-number-value
  
  #Find indices of various parts
  i_muls <- which(key_statements %in% mul_statements)
  i_dos <- which(key_statements == "do()")
  i_donts <- which(key_statements == "don't()")
  
  muls_df <- as.data.frame(i_muls)
  muls_df$mul_statements <- mul_statements
  muls_df$process_bool <- c()
  
  #Identify if 'mul()' statements are preceded by a "do" or a "dont"
  for(i in 1:length(i_muls)){
    index = i_muls[i]
    
    #find the nearest lower "do"
    nearest_do <- suppressWarnings(max(i_dos[i_dos < index]))
    
    #find the nearest lower "don't"
    nearest_dont <- suppressWarnings(max(i_donts[i_donts < index]))
  
    #compare to determine if "do" or "don't". If neither, "do". 
    if(abs(nearest_do) == Inf & abs(nearest_dont) == Inf){
      process <- TRUE
    }else if(nearest_do > nearest_dont){
      process <- TRUE
    }else{
      process <- FALSE
    }
    
    muls_df$process[i] <- process
  }
  
  muls_df
  
  process_muls_df <- muls_df %>% filter(process == TRUE)
  
  process_muls <- process_muls_df$mul_statements
  
  num <- process_muls %>% 
    str_split(pattern = c("mul\\(")) %>% 
    unlist() %>%
    str_split(pattern = c("\\)")) %>% 
    unlist() %>% 
    sort(decreasing = TRUE) %>% 
    str_extract_all("\\d+") %>% 
    unlist()
  
  num_pairs_df <- as.data.frame(matrix(as.numeric(num), ncol = 2, byrow = TRUE))
  
  colnames(num_pairs_df) <- c("N1", "N2")
  
  num_pairs_df <- num_pairs_df %>% mutate(prod = N1 * N2)
  
  prod_sum <- sum(num_pairs_df$prod) 
  prod_sum

  
### Day 4 ######################################################################
  
# PUZZLE 1 ---------------------------------------------------------------------
  
  # She'd like to know if you could help her with her word search (your puzzle input). She only has to find one word: XMAS.
  # This word search allows words to be horizontal, vertical, diagonal, written backwards, or even overlapping other words. 
  # It's a little unusual, though, as you don't merely need to find one instance of XMAS - you need to find all of them. 
  # Here are a few ways XMAS might appear, where irrelevant characters have been replaced with .:
  
    # ..X...
    # .SAMX.
    # .A..A.
    # XMAS.S
    # .X....
  
#   The actual word search will be full of letters instead. For example:

  #   MMMSXXMASM
  #   MSAMXMSMSA
  #   AMXSXMAAMM
  #   MSAMASMSMX
  #   XMASAMXAMM
  #   XXAMMXXAMA
  #   SMSMSASXSS
  #   SAXAMASAAA
  #   MAMMMXMMMM
  #   MXMXAXMASX
  
#   In this word search, XMAS occurs a total of 18 times; here's the same word search again, but where letters not involved in any XMAS have been replaced with .:
  
  # ....XXMAS.
  # .SAMXMS...
  # ...S..A...
  # ..A.A.MS.X
  # XMASAMX.MM
  # X.....XA.A
  # S.S.S.S.SS
  # .A.A.A.A.A
  # ..M.M.M.MM
  # .X.X.XMASX
  
  # input_str <- "MMMSXXMASM
  #              MSAMXMSMSA
  #              AMXSXMAAMM
  #              MSAMASMSMX
  #              XMASAMXAMM
  #              XXAMMXXAMA
  #              SMSMSASXSS
  #              SAXAMASAAA
  #              MAMMMXMMMM
  #              MXMXAXMASX"
  
  filepath <- paste0(input_dir, "D4_P1.txt")
  input_str <- readChar(filepath, file.info(filepath)$size)
  
  input_str_split <- strsplit(input_str, "\n") %>% 
                    unlist() %>% 
                    str_replace_all(" ", "") #drop whitespace
  
  #Create a matrix with 1 letter in each row/column index 
  search_matrix <- as.data.frame(matrix(nrow = 0, ncol = str_length(input_str_split[1])))
  
  for (i in 1:length(input_str_split)){
    row <- str_split_1(input_str_split[i],"")
    search_matrix <- rbind(search_matrix, row)
    # print(row)
  }
  
  colnames(search_matrix) <- NULL
  search_matrix 
  
  #Check for viable letter combinations
  directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  word_count = 0
  
    # Beginning at each X, search for "M", then "A", then "S".
  
    for(r in 1:nrow(search_matrix)){
      sel_row <- search_matrix[r,]
      add_word = 0
      
      for(c in 1:length(sel_row)){
        sel_letter <- as.character(sel_row[c])

        if(sel_letter != "X"){next} #if not an X, move on
        #print(paste0("Found an X!"))

        # If an X is found, check surrounding letters for an M: 
        #surrounding letters:
          # (r-1,c-1) (r-1,c) (r-1,c+1)
          # (r,c-1)   X(r,c)  (r,c+1)
          # (r+1,c-1) (r+1,c) (r+1,c+1)
        
        # let's give them IDs
          # 8 1 2 
          # 7 X 3
          # 6 5 4
        
        for (d in 1:length(directions)){
          direction = directions[d]

          #configure row-column steps based on direction
          r_step = case_when(direction == "NW" ~ -1,
                             direction == "N" ~ -1,
                             direction == "NE" ~ -1,
                             direction == "W" ~ 0,
                             direction == "E" ~ 0,
                             direction == "SW" ~ 1,
                             direction == "S" ~ 1,
                             direction == "SE" ~ 1,
          )
          
          c_step = case_when(direction == "NW" ~ -1,
                             direction == "N" ~ 0,
                             direction == "NE" ~ 1,
                             direction == "W" ~ -1,
                             direction == "E" ~ 1,
                             direction == "SW" ~ -1,
                             direction == "S" ~ 0,
                             direction == "SE" ~ 1,
          )
          
          #find the subsequent 3 neighbors 
          n1 <- as.character(search_matrix[r+r_step*1,c+c_step*1]) #M
          n2 <- as.character(search_matrix[r+r_step*2,c+c_step*2]) #A
          n3 <- as.character(search_matrix[r+r_step*3,c+c_step*3]) #S
          
          if(length(n1) == 0|length(n2) == 0|length(n3) == 0){next}
          if(is.na(n1)|is.na(n2)|is.na(n3)){next}
          check_word <- paste0(sel_letter, n1, n2, n3) #put it all together
          # print(check_word)
          if(check_word == "XMAS"){
              add_word = 1
              print(paste0("XMAS found at (", r, ",", c, ")"))
            }else{
              add_word = 0
            }
          word_count = word_count + add_word
        }
      }
    } 
        
word_count        


# PUZZLE 2 ---------------------------------------------------------------------

  # Looking for the instructions, you flip over the word search to find that this isn't actually an XMAS puzzle; 
  # it's an X-MAS puzzle in which you're supposed to find two MAS in the shape of an X. One way to achieve that is like this:
    # M.S
    # .A.
    # M.S
  # Irrelevant characters have again been replaced with . in the above diagram. 
  # Within the X, each MAS can be written forwards or backwards.

  #Find A's, which for neighboring M's and S's on opposite (N/S - E/W or NE/SW - SE - NW orientations)
  
  #Begin from Search Matrix - run lines 569:601
  cross_count = 0 
  directional_axes <- c("N - S & W - E", "NW - SE & SW - NE")

  
  for(r in 1:nrow(search_matrix)){
    print(paste0("Beginning row ", r))
    sel_row <- search_matrix[r,]
    add_cross = 0
    
    for(c in 1:length(sel_row)){
      sel_letter <- as.character(sel_row[c])
      
      if(sel_letter != "A"){next} #if not an A, move on
      # print(paste0("Found an A!"))
      
      # If an A is found, check surrounding letters for an M
      for (d in 1:length(directional_axes)){
        a1_good = NULL
        a2_good = NULL
        
        sel_axis = directional_axes[d]
        
        #Find neighbors on first axis in pair
        before_a1_r_step = case_when(sel_axis == "N - S & W - E" ~ -1, #N for d == 1
                                     sel_axis == "NW - SE & SW - NE" ~ -1) #NW for d == 2
        before_a1_c_step = case_when(sel_axis == "N - S & W - E" ~ 0, #N for d == 1
                                     sel_axis == "NW - SE & SW - NE" ~ -1) #NW for d == 2
        
        after_a1_r_step  = case_when(sel_axis == "N - S & W - E" ~ 1, #S for d == 1
                                     sel_axis == "NW - SE & SW - NE" ~ 1) #SE for d == 2
        after_a1_c_step = case_when(sel_axis == "N - S & W - E" ~ 0, #S for d == 1
                                    sel_axis == "NW - SE & SW - NE" ~ 1) #SE for d == 2
        
        #Find neighbors on second axis in pair
        before_a2_r_step = case_when(sel_axis == "N - S & W - E" ~ 0, #W for d == 1
                                     sel_axis == "NW - SE & SW - NE" ~ 1) #SW for d == 2
        before_a2_c_step = case_when(sel_axis == "N - S & W - E" ~ -1, #W for d == 1
                                     sel_axis == "NW - SE & SW - NE" ~ -1) #SW for d == 2
        
        after_a2_r_step = case_when(sel_axis == "N - S & W - E" ~ 0, #E for d == 1
                                    sel_axis == "NW - SE & SW - NE" ~ -1) #NE for d == 2
        after_a2_c_step = case_when(sel_axis == "N - S & W - E" ~ 1, #E for d == 1
                                    sel_axis == "NW - SE & SW - NE" ~ 1) #NE for d == 2
        

        #find the subsequent 3 neighbors 
        before_a1 <- as.character(search_matrix[r+before_a1_r_step,c+before_a1_c_step]) #M
        after_a1 <- as.character(search_matrix[r+after_a1_r_step,c+after_a1_c_step]) #S
        a1_word <- paste0(before_a1, sel_letter, after_a1)
        
        before_a2 <- as.character(search_matrix[r+before_a2_r_step,c+before_a2_c_step]) #M
        after_a2 <- as.character(search_matrix[r+after_a2_r_step,c+after_a2_c_step]) #S
        a2_word <- paste0(before_a2, sel_letter, after_a2)
        
        # print(a1_word)
        # print(a2_word)

        a1_good = if(a1_word %in% c("MAS", "SAM")){TRUE}else{FALSE}
        a2_good = if(a2_word %in% c("MAS", "SAM")){TRUE}else{FALSE}
        
        add_cross = a1_good * a2_good
        # if(add_cross == 1){print(paste0("Found one! Cross count: ", cross_count+1))}
      }
      cross_count = cross_count + add_cross
    }
  }
cross_count #should be 9 for example
    