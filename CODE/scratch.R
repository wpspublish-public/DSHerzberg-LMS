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
