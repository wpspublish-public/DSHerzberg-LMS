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
token_split_regex <- ":|(?<=[[:digit:]]),"

df_super_sub_cols <- input %>% 
  select(all_of(token_super_sub_first_col):all_of(token_super_sub_last_col)) %>% 
  mutate(across(everything(),
                ~ str_replace_all(
                  .,
                  c(
                    " [|] Excellent" = "",
                    " [|] Above average" = "",
                    " [|] Average" = "",
                    " [|] Below average" = "",
                    " [|] Poor" = "",
                    " [|] Strongly agree" = "",
                    " [|] Agree" = "",
                    " [|] Neutral" = "",
                    " [|] Disagree" = "",
                    " [|] Strongly disagree" = "",
                    " [|] Extremely useful" = "",
                    " [|] A good deal useful" = "",
                    " [|] Somewhat useful" = "",
                    " [|] A little useful" = "",
                    " [|] Not useful" = "",
                    " [|] A great deal" = "",
                    " [|] More than average" = "",
                    " [|] Average" = "",
                    " [|] Less than average" = "",
                    " [|] Very little" = ""
                  )
                )))

date_col <- input %>% 
  select(Completed) %>% 
  transmute(Completed = lubridate::mdy_hm(Completed))

lhs_cols <- input %>% 
  select(all_of(token_lhs_intact_cols)) %>% 
  mutate(across(where(is.logical), as.character))

rhs_num_cols <- input %>% 
  select(all_of(token_addl_cols_for_freq_counts))

rhs_text_cols <- input %>% 
  select(all_of(token_rhs_text_cols))

recommend_CE <- tibble(
  item = rep("Would you recommend this CE program to others?", 2),
  value = 1:2,
  label = c("Yes", "No")
)

webinar_format <- tibble(
  item = rep(
    str_c(
      "In general, what format do you prefer ",
      "for webinars?"
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

q_name <- str_c(str_replace_all(names(df_super_sub_cols), " ", "_"), ":")

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

list_r_cols <- map(list_super_sub_cols,
                   ~ .x %>%
                     select(contains("r")) %>%
                     mutate(across(
                       everything(),
                       ~ str_replace(., " ", "") %>%
                         as.integer()
                     )))

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

list_col_names <- map2(list_sub_q_names,
                       q_name,
                       ~
                         str_c(.y, .x, sep = "_") %>%
                         str_replace(., "__", "_"))

named_super_sub_r_cols <- map2(list_r_cols,  list_col_names,
                               ~ .x %>%
                                 set_names(.y)) %>%
  bind_cols()

output <- bind_cols(
  lhs_cols,
  date_col,
  named_super_sub_r_cols,
  rhs_num_cols,
  rhs_text_cols
)

write_csv(output,
          here("OUTPUT-FILES/lms-output-survey-ados2-workshop-2021-04-16.csv"),
          na = "")

freq_table_super_sub_cols <- output %>% 
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
    item,
    c("super_q", "sub_q"), 
    ":_", 
    remove = TRUE 
  ) %>% 
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
    across(c(total_pct, valid_pct, valid_cum_pct), ~ format(., nsmall = 1)) 
  ) %>%
  select(super_q, sub_q, value, label, n, total_pct, valid_pct, valid_cum_pct, total) %>% 
  rename(freq = n) %>% 
  mutate(across(everything(), ~ replace_na(., "")))

freq_table_rhs_num_cols <- rhs_num_cols %>% 
  pivot_longer(everything(), names_to = 'item', values_to = 'value') %>% 
  count(item,value) %>% 
  rename(label = value) %>% 
  right_join(item_value_label_map, by = c("item", "label")) %>%
  relocate(value, .after = item) %>% 
  group_by(item) %>% 
  arrange(desc(value), .by_group = TRUE) %>% 
  replace_na(list(n = 0)) %>% 
  mutate(total = sum(n),
         total_pct = round(100*(n/total), 1),
         valid_pct = round(100*(n/total), 1),
         csum = cumsum(n),
         valid_cum_pct = round(100*(csum/total), 1),
  ) %>% 
  ungroup() %>% 
  mutate(
    item = case_when(
      lag(item) == item  ~ NA_character_,
      TRUE ~ item
    ), 
    across(c(total_pct, valid_pct, valid_cum_pct), ~ format(., nsmall = 1)) 
  ) %>%
  select(item, value, label, n, total_pct, valid_pct, valid_cum_pct, total) %>% 
  rename(super_q = item, freq = n) %>% 
  mutate(across(super_q, ~ replace_na(., "")),
         sub_q = NA_character_,
         across(c(value, freq, total), ~ as.character(.))
  ) %>% 
  relocate(sub_q, .after = "super_q")

freq_table_all <- bind_rows(
  freq_table_super_sub_cols,
  freq_table_rhs_num_cols
)

text_cols_all <- bind_cols(
  lhs_cols,
  date_col,
  rhs_text_cols
)

write_xlsx(
  list(`text responses` = text_cols_all, `freq tables` = freq_table_all),
  here("OUTPUT-FILES/lms-report-survey-ados2-workshop-2021-04-16.xlsx")
)


