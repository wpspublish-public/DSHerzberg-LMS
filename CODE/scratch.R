test <- rhs_num_cols %>% 
  pivot_longer(everything(), names_to = 'item', values_to = 'value') %>% 
  count(item,value) %>% 
  rename(label = value) %>% 
  right_join(item_value_label_map, by = c("item", "label")) 
