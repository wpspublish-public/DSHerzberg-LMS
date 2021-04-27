library(here)
library(tidyverse)

input <- read_csv(url(
  "https://raw.github.com/wpspublish/DSHerzberg-LMS/master/INPUT-FILES/reprex-input.csv"
))

input_clean <- input$q1 %>% 
  str_replace_all( " [|] Excellent", "")

input_clean2 <- input %>% 
  mutate(across(
    everything(),
    ~ str_replace(., " [|] Excellent", "")
  ))

input_clean3 <- input %>%
  mutate(across(everything(),
                ~ str_replace_all(
                  .,
                  c(
                    " [|] Excellent" = "",
                    " [|] Above average" = "",
                    " [|] Average" = "",
                    " [|] Below average" = "",
                    " [|] Poor" = ""
                  )
                )))

write_csv(input_clean3, here("test.csv"))
