vec <- input$`Access/Setting/Overall Experience`
# str_replace_all(vec, "[|]", "")
vec2 <- str_replace_all(vec, "Excellent, ", "")
vec3 <- str_replace_all(vec, c("Excellent, " = "", "Above average, " = "", 
                               "Average, " = "", "Below average, " = "", 
                               "Poor, " = ""))
vec3[1]




see <- function(rx) str_view_all("abc ABC 123\t.!|?\\(){}\n", rx)
see("[[:space:]]")

super_sub_rating_anchors <- c("Excellent, ", "Above average, ", 
                              "Average, ", "Below average, ", "Poor, ")

c("X" = "", "Y" = "-")

vec0 <- str_c(super_sub_rating_anchors, " = ''")

vec_matches <- c("Excellent, " = "", "Above average, " = "", 
                 "Average, " = "", "Below average, " = "", "Poor, " = "")

df1 <- str_replace_all(df_super_sub_cols, c("Excellent, " = "", "Above average, " = "", 
                               "Average, " = "", "Below average, " = "", 
                               "Poor, " = ""))

vec_clean <- list_sub_q_names[[1]] %>%
           str_replace_all(
             c(
               "_Excellent,_" = "",
               "_Above average,_" = "",
               "_Average,_" = "",
               "_Below average,_" = "",
               "_Poor,_" = ""
             )
           )

list_sub_q_names <- map(
  list_super_sub_cols,
  ~ .x %>%
    select(contains("q")) %>%
    filter(row_number() == 1) %>%
    as.character() %>%
    str_replace_all(" ", "_")
)

df_clean <- df_super_sub_cols$`Access/Setting/Overall Experience` %>% 
  str_replace_all(
    c(
      " | Excellent" = "",
      " | Above Average" = "",
      " | Average" = "",
      " | Below average" = "",
      " | Poor" = ""
    )  
  )

df_super_sub_cols$`Access/Setting/Overall Experience` <-  
  str_replace_all(df_super_sub_cols$`Access/Setting/Overall Experience`, 
    c(
      " | Excellent" = "",
      " | Above Average" = "",
      " | Average" = "",
      " | Below average" = "",
      " | Poor" = ""
    )  
  )

df_clean <- df_super_sub_cols$`Access/Setting/Overall Experience` %>% 
  str_replace_all( " | Excellent", "")
 
df_super_sub_cols$`Access/Setting/Overall Experience` <- 
  str_replace_all(df_super_sub_cols$`Access/Setting/Overall Experience`, ',', 'AA') %>% 
  unnest()

df_super_sub_cols$`Access/Setting/Overall Experience` <- 
  gsub(',', 'AA', df_super_sub_cols$`Access/Setting/Overall Experience`)

