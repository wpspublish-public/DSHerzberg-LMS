suppressMessages(library(summarytools))

summarytools::freq(output$`General_Live_Webinar_Experience:_Overall_educational_experience`, order = "freq")


freq(output$`General_Live_Webinar_Experience:_Overall_educational_experience`, plain.ascii = FALSE, style = "rmarkdown")


temp2 <- freq(named_super_sub_r_cols)


# write table to .html
view(temp2, 
     Data.frame = NULL,
     display.type = FALSE,
     # collapse = TRUE,
     file = here("temp2.html"))


# get separate freq counts for school psych vs non-school psych.
school_psych <- output %>% 
  filter(`What is your field of work?` == "School Psychology") %>% 
  select(all_of(names(named_super_sub_r_cols))) %>% 
  freq(style = "rmarkdown") %>% 
  print( 
     Data.frame = NULL,
     display.type = FALSE,
     report.title = "School Psychologists",
     file = here("OUTPUT-FILES/school-psych.html"))



not_school_psych <- output %>% 
  filter(`What is your field of work?` != "School Psychology")
