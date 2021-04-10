temp2 <- rhs_num_cols %>%
  mutate(
    across(
      `Would you recommend this CE program to others?`,
      ~ case_when(.x == "Yes" ~ 1,
                  .x == "No" ~ 2,
                  TRUE ~ NA_real_)
    ),
    across(
      `In general, what format do you prefer for webinars? Choose all that apply:`,
      ~ case_when(
        .x == "Single half-day (3–4 hours/day)" ~ 1,
        .x == "Single full-day (5–6 hours/day)" ~ 2,
        .x == "Multiday (half-days)" ~ 3,
        .x == "Multiday (full-days)" ~ 4,
        .x == "Pre-recorded at own pace (no live instruction)" ~ 5,
        .x == "Blended instruction (live webinar combined with independent study)" ~ 6,
        TRUE ~ NA_real_
      )
    ),
    across(
      `In general, what time of day do you prefer to begin a live webinar?`,
      ~ case_when(
        .x == "Morning" ~ 1,
        .x == "Afternoon" ~ 2,
        .x == "Evening" ~ 3,
        TRUE ~ NA_real_
      )
    ),
    across(
      `How did you learn about this CE program?`,
      ~ case_when(
        .x == "WPS website" ~ 1,
        .x == "WPS catalog/print advertisement" ~ 2,
        # need to verify exact wording
        .x == "Direct email from WPS" ~ 3,
        .x == "Social media (e.g., Facebook)" ~ 4,
        .x == "Supervisor" ~ 5,
        .x == "Colleague" ~ 6,
        .x == "Other (answer in next question)" ~ 7,
        TRUE ~ NA_real_
      )
    ),
    across(
      `What is your highest academic degree?`,
      ~ case_when(
        .x == "Doctorate" ~ 1,
        .x == "Master’s (MSW, MS, MA)" ~ 2,
        .x == "Bachelor’s (BS, BA)" ~ 3,
        .x == "Associates" ~ 4,
        .x == "No college degree" ~ 5,
        TRUE ~ NA_real_
      )
    ),
    across(
      `What is your field of work?`,
      ~ case_when(
        .x == "Applied Behavior Analysis (ABA; BABCP)" ~ 1,
        .x == "Clinical Psychology" ~ 2,
        .x == "Counseling" ~ 3,
        .x == "Educational Diagnostician/Psychometrist" ~ 4,
        .x == "Medicine" ~ 5,
        .x == "Neuropsychology" ~ 6,
        .x == "Occupational Therapy" ~ 7,
        .x == "Physical Therapy" ~ 8,
        .x == "Psychiatry" ~ 9,
        .x == "School Psychology" ~ 10,
        .x == "Social Work" ~ 11,
        .x == "Special Education" ~ 12,
        .x == "Speech–Language Pathology/Audiology" ~ 13,
        .x == "Other (answer in next question)" ~ 14,
        TRUE ~ NA_real_
      )
    ),
    across(
      `I certify that I am the person who attended the live webinar and completed this evaluation.`,
      ~ case_when(.x == "Yes" ~ 1,
                  TRUE ~ NA_real_)
    )
  ) %>% 
  pivot_longer(everything(), names_to = 'item', values_to = 'value') %>% 
  count(item, value) %>%   
  group_by(item)


temp3 <- map(names(rhs_num_cols), 
             ~
               temp2 %>% 
               filter(item == .x)
) %>% set_names(names(rhs_num_cols))


temp4 <- tibble(input = temp3,
                upper = c(2, 6, 3, 7, 5, 14, 1))


temp5 <- temp4 %>% 
  mutate(output =
           map2(
             input,
             upper,
             ~
               .x %>% 
               complete(value = 1:.y)
           )
  ) %>% 
  select(output) %>% 
  unnest(cols = c(output)) %>% 
  group_by(item) %>% 
  arrange(desc(value), .by_group = TRUE) %>% 
  replace_na(list(n = 0)) %>% 
  mutate(total = sum(n),
         total_pct = round(100*(n/total), 1),
         valid_pct = round(100*(n/total), 1),
         csum = cumsum(n),
         valid_cum_pct = round(100*(csum/total), 1),
  )
  
# Set up item-value-label mapping in a df

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

# build freq table

freq_table <- rhs_num_cols %>% 
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
  # responses to a single quesiton.
  filter(!is.na(value)) %>% 
  replace_na(list(n = 0)) %>% 
  mutate(total = sum(n),
         total_pct = round(100*(n/total), 1),
         valid_pct = round(100*(n/total), 1),
         csum = cumsum(n),
         valid_cum_pct = round(100*(csum/total), 1),
  ) %>% mutate(
    item = case_when(
      lag(item) == item  ~ NA_character_,
      TRUE ~ item
    ), across(c(total_pct, valid_pct, valid_cum_pct), ~ format(., digits = 1, nsmall = 1)) 
    # format ensures pct will print with 1 digit right of decimal
  ) %>%
  select(item, value, label, n, total_pct, valid_pct, valid_cum_pct, total) %>% 
  rename(freq = n) %>% 
  mutate(across(item, ~ replace_na(., "")))
  

