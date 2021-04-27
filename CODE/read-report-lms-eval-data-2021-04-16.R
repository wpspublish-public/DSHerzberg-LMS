suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(writexl))

input <-
  suppressMessages(read_csv(here(
    "INPUT-FILES/input-survey-ados2-workshop-2021-04-16.csv"
  )))

names_input <- names(input)
token_super_sub_first_col <- "Access/Setting/Overall Experience"
token_super_sub_last_col <- "Usefulness of Content"
token_addl_cols_for_freq_counts <- c("Would you recommend this CE program to others?", 
                                     "In general, what format do you prefer for webinars?", 
                                     "In general, what time of day do you prefer to begin a live webinar?", 
                                     "How did you learn about this CE program?", 
                                     "What is your highest academic degree?",
                                     "What is your field of work?", 
                                     "I certify that I am the person who attended the live webinar and completed this evaluation.")
token_lhs_intact_cols <- c("First Name", "Last Name", "Email", "Credits")
token_rhs_text_cols <- c("How will you use the knowledge gained from this course within your practice?",
                         "If you selected Other, please specify:",
                         str_c("Please share any other feedback you have, including suggestions ",
                         "for future continuing education you’d like to see available through WPS, ", 
                         "such as specific topics for live workshops, webinars, or independent study opportunities:"),
                         "If you selected Other, please specify:_1")
token_split_destination_cols <- c("q1", "r1", "q2", "r2", "q3", "r3", "q4", "r4", "q5", "r5")
# DOCUMENT USE OF SQUARE BRACKETS TO MATCH VERTICAL PIPE IN REGEX
token_split_regex <- ":|[|]"

# segregate super-sub cols
df_super_sub_cols <- input %>% 
  select(all_of(token_super_sub_first_col):all_of(token_super_sub_last_col))

# segregate and process date-time col
date_col <- input %>% 
  select(Completed) %>% 
  transmute(Completed = lubridate::mdy_hm(Completed))

# segregate LHS and RHS side cols for final output
lhs_cols <- input %>% 
  select(all_of(token_lhs_intact_cols)) %>% 
  mutate(across(where(is.logical), as.character))

rhs_num_cols <- input %>% 
  select(all_of(token_addl_cols_for_freq_counts))

rhs_text_cols <- input %>% 
  select(all_of(token_rhs_text_cols))

# Set up df to hold item-value-label mapping for rhs_num_cols

recommend_CE <- tibble(
  item = rep("Would you recommend this CE program to others?", 2),
  value = 1:2,
  label = c("Yes", "No")
)

webinar_format <- tibble(
  item = rep(
    str_c(
      "In general, what format do you prefer ",
      "for webinars? Choose all that apply:"
    ),
    6
  ),
  value = 1:6,
  label = c(
    "Single half-day (3–4 hours/day)",
    "Single full-day (5–6 hours/day)",
    "Multiday (half-days)",
    "Multiday (full-days)",
    "Pre-recorded at own pace (no live instruction)",
    "Blended instruction (live webinar combined with independent study)"
  )
)

webinar_time <- tibble(
  item = rep("In general, what time of day do you prefer to begin a live webinar?",
             3),
  value = 1:3,
  label = c("Morning",
            "Afternoon",
            "Evening")
)

learn_CE <- tibble(
  item = rep("How did you learn about this CE program?",
             7),
  value = 1:7,
  label = c(
    "WPS website",
    "WPS catalog/print advertisement",
    "Direct email from WPS",
    "Social media (e.g., Facebook)",
    "Supervisor",
    "Colleague",
    "Other (answer in next question)"
  )
)

highest_degree <- tibble(
  item = rep("What is your highest academic degree?",
             5),
  value = 1:5,
  label = c(
    "Doctorate",
    "Master’s (MSW, MS, MA)",
    "Bachelor’s (BS, BA)",
    "Associates",
    "No college degree"
  )
)

field_work <- tibble(
  item = rep("What is your field of work?",
             14),
  value = 1:14,
  label = c(
    "Applied Behavior Analysis (ABA; BABCP)",
    "Clinical Psychology",
    "Counseling",
    "Educational Diagnostician/Psychometrist",
    "Medicine",
    "Neuropsychology",
    "Occupational Therapy",
    "Physical Therapy",
    "Psychiatry",
    "School Psychology",
    "Social Work",
    "Special Education",
    "Speech–Language Pathology/Audiology",
    "Other (answer in next question)"
  )
)

certify_attend <- tibble(
  item =
    str_c(
      "I certify that I am the person who attended ",
      "the live webinar and completed this evaluation."
    ),
  value = 1,
  label = "Yes"
)

item_value_label_map <- bind_rows(
  recommend_CE,
  webinar_format,
  webinar_time,
  learn_CE,
  highest_degree,
  field_work,
  certify_attend
)

# CREATE FREQ TABLE FOR super_sub_cols

# extract super-ordinate question name for output by replacing white space with
# underscore, adding : as a separator for the subordinate question name, col
# labels on output table are long labels containing both super- and sub- questions.
q_name <- str_c(str_replace_all(names(df_super_sub_cols), " ", "_"), ":")

# use `tidyr::separate` to split a single column containing a long string into
# several cols. 1st arg names col to be split; 2nd arg is vec of column names to
# hold the parts of the split string; 3rd arg is regex specifying the
# characters to split on; 4th arg drops input col in output df. In this input
# file, there are X super-sub questions to process, so we map() over the names
# of those X questions. In addition, those X questions have differing
# numbers of sub-questions. The way to handle this is to create enough output cols
# (e.g., q1-r1  pairs) to handle question with the MOST sub-questions. The ones
# with fewer sub-questions will have all NA in those extra columns, and the empty
# cols can be dropped with janitor::remove_empty(). Here map returns a list of
# X dfs, each containing the separated sub-questions and responses
# corresponding to one super-ordinate question.
list_super_sub_cols <- map(
  names(df_super_sub_cols),
  ~ df_super_sub_cols %>%
    select(!!sym(.x)) %>%
    separate(
      !!sym(.x),
      all_of(token_split_destination_cols),
      token_split_regex,
      remove = TRUE
    # ) %>% 
    # janitor::remove_empty("cols")
))

# START HERE: COLS ARE SPLIT PROPERLY, BUT NEED TO GET RID OF CERTAIN TEXT ELEMENTS ACROSS COLS.

# The cells of the final output need to be the numerical response values to the
# questions. Next snippet selects() the cols holding these response values,
# strips out white space, and formats values as integers. Here we are mapping
# over a list containing dfs, and feeding one data frame per iteration with .x
list_r_cols <- map(list_super_sub_cols,
                   ~ .x %>%
                     select(contains("r")) %>%
                     mutate(across(
                       everything(),
                       ~ str_replace(., " ", "") %>%
                         as.integer()
                     )))

# In the list of dfs, the sub-question names exist as duplicate cell values in
# cols, but we need them to be in a vec so they can be used as part of col names
# on the final output table. We select the sub-question cols, filter only the
# first row to capture just the names we need without any dups, apply
# as.character to transform the df row into a vec, strip out whitespace and
# other garbage text. Note how str_replace_all() can take a vector of strings
# matched to replacements. Thus it can handle searching for multiple different
# sttings within a single call. Here we are mapping over a list containing dfs,
# and feeding one data frame per iteration with .x. Map returns a list of vecs
# containing the sub-question names associated with each df.
list_sub_q_names <- map(
  list_super_sub_cols,
  ~ .x %>%
    select(contains("q")) %>%
    filter(row_number() == 1) %>%
    as.character() %>%
    str_replace_all(" ", "_") %>% 
    str_replace_all(
      c(
        "_Excellent,_" = "",
        "_Above average,_" = "",
        "_Average,_" = "",
        "_Below average,_" = "",
        "_Poor,_" = ""
      )
    ))

# now we create the vector of col names for output by concatenating the
# super-ordinate q_name (which gets recycled), with the sub-ordinate q_names. We
# use map2() to map over a list of char vecs (.x) and a vec (.y). As we iterate
# and concatenate the col names, the current .y element recycles and gets joined
# to each of the current .x elements.
list_col_names <- map2(list_sub_q_names,
                       q_name,
                       ~
                         str_c(.y, .x, sep = "_") %>%
                         str_replace(., "__", "_"))

# we name the cols of numerical responses with the vec of names we just created.
# As previously, we map2() over the list of col names, and the list of dfs
# containing the sub question responses. This returns a list of dfs with the
# cols named as required. We use bind_cols() to bind the separate dfs into a
# single df.
named_super_sub_r_cols <- map2(list_r_cols,  list_col_names,
                                    ~ .x %>%
                                      set_names(.y)) %>% 
  bind_cols()

# configure final output

output <- bind_cols(
  lhs_cols,
  date_col,
  named_super_sub_r_cols,
  rhs_num_cols,
  rhs_text_cols
)

# write output to .csv
write_csv(output,
          here("OUTPUT-FILES/lms-output.csv"),
          na = "")

# create report for RAs that gives freq counts of responses by question, for
# only school psychologists
freq_table_super_sub_cols <- output %>% 
  filter(`What is your field of work?` == "School Psychology") %>% 
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

# CREATE FREQ TABLE FOR rhs_num_cols

freq_table_rhs_num_cols <- rhs_num_cols %>% 
  pivot_longer(everything(), names_to = 'item', values_to = 'value') %>% 
  count(item,value) %>% 
  rename(label = value) %>% 
  right_join(item_value_label_map, by = c("item", "label")) %>%
  relocate(value, .after = item) %>% 
  group_by(item) %>% 
  arrange(desc(value), .by_group = TRUE) %>% 
  # next line will disappear for future versions of this template. It exists
  # here only to get rid of garbage rows that account for cells where more than
  # one response was entered. The LMS output no longer permits multiple
  # responses to a single question.
  filter(!is.na(value)) %>% 
  replace_na(list(n = 0)) %>% 
  mutate(total = sum(n),
         total_pct = round(100*(n/total), 1),
         valid_pct = round(100*(n/total), 1),
         csum = cumsum(n),
         valid_cum_pct = round(100*(csum/total), 1),
  ) %>% 
  # need to ungroup() before calling mutate(across()), grouped input messes with across()
  ungroup() %>% 
  mutate(
    item = case_when(
      lag(item) == item  ~ NA_character_,
      TRUE ~ item
    ), across(c(total_pct, valid_pct, valid_cum_pct), ~ format(., digits = 1, nsmall = 1)) 
    # format ensures pct will print with 1 digit right of decimal
  ) %>%
  select(item, value, label, n, total_pct, valid_pct, valid_cum_pct, total) %>% 
  rename(freq = n) %>% 
  mutate(across(item, ~ replace_na(., "")))



# write_xlsx(
#   list(school_psych = school_psych), 
#   here("OUTPUT-FILES/school-psych-report.xlsx")
# )


