
joinRAW<- function(data, raw){
  
  data <- bind_rows(data, raw)
  
  return(data)
  
  
}