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
  left_join(item_value_label_map, by = c("item", "label")) %>% 
  relocate(value, .after = item) %>% 
  group_by(item) %>% 
  arrange(desc(value), .by_group = TRUE)
  
  
