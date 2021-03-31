suppressMessages(library(summarytools))
suppressMessages(library(huxtable))


# freq(output$`General_Live_Webinar_Experience:_Overall_educational_experience`, order = "freq")
# 
# 
# freq(output$`General_Live_Webinar_Experience:_Overall_educational_experience`, plain.ascii = FALSE, style = "rmarkdown")
# 
# 
# temp2 <- freq(named_super_q_sub_q_r_cols)
# 
# 
# # write table to .html
# view(temp2, 
#      Data.frame = NULL,
#      display.type = FALSE,
#      # collapse = TRUE,
#      file = here("temp2.html"))


# get separate freq counts for school psych vs non-school psych.
school_psych <- output %>% 
  filter(`What is your field of work?` == "School Psychology") %>% 
  select(all_of(names(named_super_q_sub_q_r_cols))) %>% 
  freq(style = "rmarkdown") %>% 
  print( 
     Data.frame = NULL,
     display.type = FALSE,
     report.title = "School Psychologists",
     file = here("OUTPUT-FILES/school-psych.html"))



not_school_psych <- output %>% 
  filter(`What is your field of work?` != "School Psychology")



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
         pct = round(100*(n/total), 1),
         csum = cumsum(n),
         cum_pct = round(100*(csum/total), 1),
  ) %>% 
  separate(
    item,# source col for string to split
    c("super_q", "sub_q"), # destination cols for split parts of input string
    "(:_)", #regex for chars to split on and drop from destination cols
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
  )
  ) %>% 
  select(super_q, sub_q, value, label, n, total, pct, cum_pct)
  
write_csv(school_psych, here("OUTPUT-FILES/school-psych-report.csv"),
          na = ""
          )
  

school_psych <- output %>% 
  filter(`What is your field of work?` == "School Psychology") %>% 
  select(all_of(names(named_super_q_sub_q_r_cols))) %>% 
  pivot_longer(everything(), names_to = 'item', values_to = 'value') %>% 
  count(item, value) %>% 
  group_by(item) %>% 
  mutate(total = sum(n))
  


new.data = old.data %>%
  group_by(country, year) %>%
  mutate(sum.amount = sum(amount)) %>%
  filter(donor == "united states") %>%
  summarize(sum.amount = max(sum.amount), 
            us.amount = sum(amount))



  pmap(
    ~ ..1 %>%
      select(!!..2) %>%
      pivot_longer(everything(), names_to = 'item', values_to = 'value') %>%
      count(item, value) %>%
      pivot_wider(names_from = value, values_from = n) %>%
      arrange(match(item, !!..2)) %>%
      mutate(data = case_when(rownames(.) == "1" ~ ..4,
                              T ~ NA_character_)) %>%
      select(data, item, !!..3)
  ) %>%
  set_names(str_c("freq_item_val_", data_name_suffix)) %>%
  iwalk(~ write_csv(.x, here(
    str_c("OUTPUT-FILES/TABLES/",
          str_replace_all(.y, "_", "-"),
          ".csv")
  ),
  na = "")) %>%
  list2env(envir = .GlobalEnv)
