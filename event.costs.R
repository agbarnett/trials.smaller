# event.costs.R
# function to create random event costs
# August 2016

random.event.costs = function(){
  
# event (hospital) costs , 'inputs' columns Y to AG
  
## MI ##
event.costs = read.table(header=T, stringsAsFactors=F, sep=',', text='
Gender,Agey,Ageu,Cost_MI
Male,0,24,5505 
Male,25,34,7814 
Male,35,44,9343
Male,45,54,9755 
Male,55,64,10239 
Male,65,74,11137
Male,75,84,10801
Male,85,100,8364  
Female,0,24,8184
Female,25,34,7940 
Female,35,44,7793 
Female,45,54,7281 
Female,55,64,8133
Female,65,74,9386  
Female,75,84,9249 
Female,85,100,8101')
event.costs$Cost_MI = event.costs$Cost_MI*1.16 # inflate
event.costs$SE = event.costs$Cost_MI * 0.5 # assumption
event.costs$shape = (event.costs$Cost_MI/ event.costs$SE)^2
event.costs$rate = (event.costs$SE^2) / event.costs$Cost_MI
event.costs$Probabilistic_MI = NA
for (k in 1:nrow(event.costs)){
 event.costs$Probabilistic_MI[k] = rgamma(n=1, shape=event.costs$shape[k], scale=event.costs$rate[k]) # sample from gamma
}

e.costs = NULL
for (k in 1:nrow(event.costs)){
  this.e = event.costs[k,]
  frame = data.frame(Gender=this.e$Gender, Age=this.e$Agey:this.e$Ageu, EventCost=this.e$Probabilistic_MI)
  e.costs = rbind(e.costs, frame)
}
MI.costs = e.costs

## Stroke ##
event.costs = read.table(header=T, stringsAsFactors=F, sep=',', text='
Gender,Agey,Ageu, Cost_Stroke
Male,0,24,24682 
Male,25,34,26439 
Male,35,44,19733 
Male,45,54, 18018
Male,55,64, 16208 
Male,65,74, 15200 
Male,75,84, 14434 
Male,85,100, 14567 
Female,0,24, 25610
Female,25,34, 19629
Female,35,44, 20969 
Female,45,54, 19502
Female,55,64, 18372
Female,65,74, 16630 
Female,75,84, 14629 
Female,85,100,13941 ')
  
  event.costs$Cost_Stroke = event.costs$Cost_Stroke*1.16 # inflate
  event.costs$SE = event.costs$Cost_Stroke * 0.5 # assumption
  event.costs$shape = (event.costs$Cost_Stroke/ event.costs$SE)^2
  event.costs$rate = (event.costs$SE^2) / event.costs$Cost_Stroke
  event.costs$Probabilistic_Stroke = NA
  for (k in 1:nrow(event.costs)){
    event.costs$Probabilistic_Stroke[k] = rgamma(n=1, shape=event.costs$shape[k], scale=event.costs$rate[k])    # new gamma
  }
  
  e.costs = NULL
  
  for (k in 1:nrow(event.costs)){
    this.e = event.costs[k,]
    frame = data.frame(Gender=this.e$Gender, Age=this.e$Agey:this.e$Ageu, EventCost=this.e$Probabilistic_Stroke)
    e.costs = rbind(e.costs, frame)
  }
  Stroke.costs = e.costs
  
  names(MI.costs) = c("Gender","Age","MI_Cost")
  names(Stroke.costs) = c("Gender","Age","Stroke_Cost")
  
  e.costs =  data.frame(MI.costs,subset(Stroke.costs, select=Stroke_Cost))

  return(e.costs)
  }