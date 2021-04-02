suppressMessages(library(here))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(xlsx))

input <-
  suppressMessages(read_csv(here(
    "INPUT-FILES/summary-evaluation-data.csv"
  )))

names_input <- names(input)
token_super_sub_first_col <- "General Live Webinar Experience"
token_super_sub_last_col <- "Usefulness of Content"
token_lhs_intact_cols <- c("First Name", "Last Name", "Email", "Credits")
token_rhs_intact_first_col <- str_c("How will you use the knowledge ", 
                                    "gained from this course within your practice?")
token_rhs_intact_last_col <- str_c("I certify that I am the person who ", 
                                   "attended the live webinar and completed this evaluation.")
token_split_destination_cols <- c("q1", "r1", "q2", "r2", "q3", "r3", "q4", "r4", "q5", "r5")
token_split_regex <- ":|(?<=[[:digit:]]),"

# segregate super-sub cols
df_super_sub_cols <- input %>% 
  select(all_of(token_super_sub_first_col):all_of(token_super_sub_last_col))

# segregate and process date-time col
date_col <- input %>% 
  select(Completed) %>% 
  transmute(Completed = lubridate::mdy_hm(Completed))

# segregate LHS and RHS side cols to leave intact for final output
lhs_cols <- input %>% 
  select(all_of(token_lhs_intact_cols)) %>% 
  mutate(across(where(is.logical), as.character))

rhs_cols <- input %>% 
  select(all_of(token_rhs_intact_first_col):all_of(token_rhs_intact_last_col))

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
    ) %>% 
    janitor::remove_empty("cols")
)

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
# as.character to transform the df row into a vec, and strip out whitespace.
# Here we are mapping over a list containing dfs, and feeding one data frame per
# iteration with .x. Map returns a list of vecs containing the sub-question names
# associated with each df.
list_sub_q_names <- map(
  list_super_sub_cols,
  ~ .x %>%
    select(contains("q")) %>%
    filter(row_number() == 1) %>%
    as.character() %>%
    str_replace_all(" ", "_")
)

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
# cols named as required. We used bind_cols() to bind the separate dfs into a
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
  rhs_cols
)

# write output to .csv
write_csv(output,
          here("OUTPUT-FILES/lms-output.csv"),
          na = "")

# create report for RAs that gives freq counts of responses by question, for
# only school psychologists
school_psych <- output %>% 
  filter(`What is your field of work?` == "School Psychology") %>% 
  select(all_of(names(named_super_sub_r_cols))) %>% 
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
  as.data.frame() %>% # write.xlsx() needs to see a data frame, not a tibble
  mutate(across(everything(), ~ replace_na(., ""))) %>% 
  write.xlsx(
    here("OUTPUT-FILES/school-psych-report.xlsx"),
    sheetName = "school_psych",
    row.names = FALSE,
    append = FALSE
  )


