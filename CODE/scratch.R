temp2 <- q_r_super_sub_cols %>%
  mutate(
    label = case_when(
      (str_detect(item, "Access") | str_detect(item, "Quality_of")) & value == 5 ~ "Excellent",
      (str_detect(item, "Access") | str_detect(item, "Quality_of")) & value == 4 ~ "Above average",
      (str_detect(item, "Access") | str_detect(item, "Quality_of")) & value == 3 ~ "Average",
      (str_detect(item, "Access") | str_detect(item, "Quality_of")) & value == 2 ~ "Below average",
      (str_detect(item, "Access") | str_detect(item, "Quality_of")) & value == 1 ~ "Poor",
      str_detect(item, "Objectives") & value == 5 ~ "Strongly agree",
      str_detect(item, "Objectives") & value == 4 ~ "Agree",
      str_detect(item, "Objectives") & value == 3 ~ "Neutral",
      str_detect(item, "Objectives") & value == 2 ~ "Disagree",
      str_detect(item, "Objectives") & value == 1 ~ "Strongly disagree",
      str_detect(item, "Self-Evaluation") & value == 5 ~ "A great deal",
      str_detect(item, "Self-Evaluation") & value == 4 ~ "More than average",
      str_detect(item, "Self-Evaluation") & value == 3 ~ "Average",
      str_detect(item, "Self-Evaluation") & value == 2 ~ "Less than average",
      str_detect(item, "Self-Evaluation") & value == 1 ~ "Very little",
      str_detect(item, "Usefulness_of") & value == 5 ~ "Extremely useful",
      str_detect(item, "Usefulness_of") & value == 4 ~ "A good deal useful",
      str_detect(item, "Usefulness_of") & value == 3 ~ "Somewhat useful",
      str_detect(item, "Usefulness_of") & value == 2 ~ "A little useful",
      str_detect(item, "Usefulness_of") & value == 1 ~ "Not useful",
      TRUE ~ NA_character_
    )
  )


# create report for RAs that gives freq counts of responses by question
freq_table_super_sub_cols <- output %>% 
  # filter(`What is your field of work?` == "School Psychology") %>% 
  select(all_of(names(named_super_sub_r_cols))) %>%  
  # all_of(token_addl_cols_for_freq_counts)) %>% 
  pivot_longer(everything(), names_to = 'item', values_to = 'value') %>% 
  count(item, value) %>% 
  group_by(item) %>% 
  complete(value = 1:5) %>% 
  arrange(desc(value), .by_group = TRUE) %>% 
  replace_na(list(n = 0)) %>% 
  mutate(total = sum(n),
         total_pct = round(100*(n/total), 1),
         valid_pct = round(100*(n/total), 1),
         csum = cumsum(n),
         valid_cum_pct = round(100*(csum/total), 1),
  ) %>% 
  separate(
    item,# source col for string to split
    c("super_q", "sub_q"), # destination cols for split parts of input string
    ":_", #regex for chars to split on and drop from destination cols
    remove = TRUE # drop input string
  ) %>% 
  # create label var with mutate(case_when()). Using str_detect(), which
  # evaluates to a logical, we can construct predicates that capture multiple
  # different row values that share a common substring (e.g., "Access")
  mutate(
    label = case_when(
      (str_detect(super_q, "Access") | str_detect(super_q, "Quality_of")) & value == 5 ~ "Excellent",
      (str_detect(super_q, "Access") | str_detect(super_q, "Quality_of")) & value == 4 ~ "Above average",
      (str_detect(super_q, "Access") | str_detect(super_q, "Quality_of")) & value == 3 ~ "Average",
      (str_detect(super_q, "Access") | str_detect(super_q, "Quality_of")) & value == 2 ~ "Below average",
      (str_detect(super_q, "Access") | str_detect(super_q, "Quality_of")) & value == 1 ~ "Poor",
      str_detect(super_q, "Objectives") & value == 5 ~ "Strongly agree",
      str_detect(super_q, "Objectives") & value == 4 ~ "Agree",
      str_detect(super_q, "Objectives") & value == 3 ~ "Neutral",
      str_detect(super_q, "Objectives") & value == 2 ~ "Disagree",
      str_detect(super_q, "Objectives") & value == 1 ~ "Strongly disagree",
      str_detect(super_q, "Self-Evaluation") & value == 5 ~ "A great deal",
      str_detect(super_q, "Self-Evaluation") & value == 4 ~ "More than average",
      str_detect(super_q, "Self-Evaluation") & value == 3 ~ "Average",
      str_detect(super_q, "Self-Evaluation") & value == 2 ~ "Less than average",
      str_detect(super_q, "Self-Evaluation") & value == 1 ~ "Very little",
      str_detect(super_q, "Usefulness_of") & value == 5 ~ "Extremely useful",
      str_detect(super_q, "Usefulness_of") & value == 4 ~ "A good deal useful",
      str_detect(super_q, "Usefulness_of") & value == 3 ~ "Somewhat useful",
      str_detect(super_q, "Usefulness_of") & value == 2 ~ "A little useful",
      str_detect(super_q, "Usefulness_of") & value == 1 ~ "Not useful",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(
    super_q = case_when(
      lag(super_q) == super_q  ~ NA_character_,
      TRUE ~ super_q
    ),
    sub_q = case_when(
      lag(sub_q) == sub_q  ~ NA_character_,
      TRUE ~ sub_q
    ),
    across(c(total_pct, valid_pct, valid_cum_pct), ~ format(., digits = 1, nsmall = 1)) # format ensures pct will print with 2 digits right of decimal
  ) %>%
  select(super_q, sub_q, value, label, n, total_pct, valid_pct, valid_cum_pct, total) %>% 
  rename(freq = n) %>% 
  mutate(across(everything(), ~ replace_na(., "")))

# create report for RAs that gives freq counts of responses by question
freq_table_super_sub_cols <- output %>% 
  # filter(`What is your field of work?` == "School Psychology") %>% 
  select(all_of(names(named_super_sub_r_cols))) %>%  
  # all_of(token_addl_cols_for_freq_counts)) %>% 
  pivot_longer(everything(), names_to = 'item', values_to = 'value') %>% 
  count(item, value) %>% 
  group_by(item) %>% 
  complete(value = 1:5) %>% 
  arrange(desc(value), .by_group = TRUE) %>% 
  replace_na(list(n = 0)) %>% 
  mutate(total = sum(n),
         total_pct = round(100*(n/total), 1),
         valid_pct = round(100*(n/total), 1),
         csum = cumsum(n),
         valid_cum_pct = round(100*(csum/total), 1),
  ) %>% 
  separate(
    item,# source col for string to split
    c("super_q", "sub_q"), # destination cols for split parts of input string
    ":_", #regex for chars to split on and drop from destination cols
    remove = TRUE # drop input string
  ) %>% mutate(
    super_q = case_when(
      lag(super_q) == super_q  ~ NA_character_,
      TRUE ~ super_q
    ),
    sub_q = case_when(
      lag(sub_q) == sub_q  ~ NA_character_,
      TRUE ~ sub_q
    ),
    label = case_when(
      value == 5 ~ "excellent",
      value == 4 ~ "above average",
      value == 3 ~ "average",
      value == 2 ~ "below average",
      value == 1 ~ "poor",
      TRUE ~ NA_character_
    ),
    across(c(total_pct, valid_pct, valid_cum_pct), ~ format(., digits = 1, nsmall = 1)) # format ensures pct will print with 2 digits right of decimal
  ) %>%
  select(super_q, sub_q, value, label, n, total_pct, valid_pct, valid_cum_pct, total) %>% 
  rename(freq = n) %>% 
  mutate(across(everything(), ~ replace_na(., "")))


