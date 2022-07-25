# call libraries
library(tidyverse)
library(here)
library(rio)

# clean qualtrics data
clean_qualtrics <- function(df_q) {
  df_q[3:nrow(df_q),1:ncol(df_q)] %>% 
    janitor::clean_names() %>% 
    filter(distribution_channel != "preview") %>% 
    select(id, unique_code, finished, consent1) %>% 
    mutate(unique_code = as.numeric(unique_code),
           con = if_else(consent1 == "I give my consent.", 1, 0),
           done = if_else(finished == T, if_else(con == 1, 1, 0), 0))
}

# clean mturk data (and, if applicable, specify name of column w/ "batch" labels)
clean_mturk <- function(df_m, label_m = NULL) {
  df_m %>% 
    janitor::clean_names() %>% 
    rename(., id = amazon_identifier) %>% 
    rename(., entered_code = actual_completion_code) %>% 
    mutate(entered_code = as.numeric(entered_code),
           code_yn = if_else(is.na(entered_code) == T, 0, 1),
           submitted = if_else(approval_status == "Not Submitted", 0, 1)) %>% 
    select(id, submitted, code_yn, entered_code, approval_status, submitted, label_m)
}

# check for duplicates in each of the dfs you are hoping to match
check_duplicates <- function(df_m, df_q, kill_switch = T) {
  duplicates1 <- if_else(nrow(unique(df_m)) != nrow(df_m), 1, 0)
  duplicates2 <- if_else(nrow(unique(df_q)) != nrow(df_q), 1, 0)
  
  duplicates <- if_else(duplicates1 == 1, if_else(duplicates2 == 0, 1, 3), if_else(duplicates2 == 1, 2, 0))
  
  if(duplicates == 0) {
    message("No duplicates detected.")
  }
  else(
    if(kill_switch == F) {
      if(duplicates == 3) {
        warning("Check df1 and df2 for duplicates.")
      }
      else(
        if(duplicates == 1) {
          warning("Check df1 for duplicates.")
        }
        else(
          warning("Check df2 for duplicates.")
        )
      )
    }
    else(
      if(duplicates == 3) {
        stop("Check df1 and df2 for duplicates.")
      }
      else(
        if(duplicates == 1) {
          stop("Check df1 for duplicates.")
        }
        else(
          stop("Check df2 for duplicates.")
        )
      )
    )
  )
  
}

# merge dfs w/ "id" as the key
match_dfs <- function(df_m, df_q, label_m = NULL) {
  left_join(df_m, df_q, by = "id") %>%
    select(id, submitted, con, unique_code, entered_code, code_yn, label_m, everything())
}

# clean joined df and add info about matches and MTurk approval status
clean_matchdf <- function(df_j, label_m = NULL) {
  df_j %>% 
    mutate(match = if_else(code_yn == 1, if_else(
      as.numeric(entered_code) == as.numeric(unique_code), 1, 0
    ), 2),
    pending = if_else(approval_status == "Pending", 1, 0)) %>% 
    select(id, match, con, submitted, pending, contains("code"), approval_status, label_m)
}

# put all of the functions together
mtchr <- function(df_m, df_q, label_m = NULL, kill_switch = T) {
  
  df_m <- df_m %>% 
    clean_mturk(., label_m)
  
  df_q <- df_q %>% 
    clean_qualtrics()
  
  check_duplicates(df_m, df_q, kill_switch)
  
  match_dfs(df_m, df_q, label_m) %>% 
    clean_matchdf(., label_m)
}