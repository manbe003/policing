#create nulls
ReplwNull = function(x){
  match = c("","Unknown","unknwon","Not Applicable","NA","NULL")
  clean = mutate_all(x, list(~na_if_in(.,match)))
}