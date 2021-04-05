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

# NEXT: TO USE complete(), ON ITEMS WITH DIFFERENT NUMBERS OF RESPONSE OPTIONS,
# MAY NEED TO BREAK INTO SEPARATE DFS BY ITEM, APPLY complete(), AND THEN
# REASSEMBLE LONG TABLE.

# Testing GitHub

       
