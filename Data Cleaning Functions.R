#create NAs from white list of terms
ReplwNull = function(x){
  match = c("","Unknown","unknwon","Not Applicable","NA","NULL")
  clean = mutate_all(x, list(~na_if_in(.,match)))
}

#trim and lower case all values for simplifying data
LowerTrim = function(x){
  x %>% mutate(across(where(is.character), str_trim))
  x %>% mutate(across(where(is.character), tolower))
}