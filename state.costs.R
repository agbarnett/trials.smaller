# state.costs.R
# function to create random state costs
# August 2016

random.state.costs = function(text.me.data, TM_MBS_PBS_complete){

# state.costs, 'inputs' 
text.me.data$age.group = as.numeric(text.me.data$AGE>55) + as.numeric(text.me.data$AGE>65) 

# get age group and gender
TM_MBS_PBS_complete$age.group = ifelse(TM_MBS_PBS_complete$age_gender=="<55, female" | TM_MBS_PBS_complete$age_gender=="<55, male", 0,
                                       ifelse(TM_MBS_PBS_complete$age_gender=="55-64, female" | TM_MBS_PBS_complete$age_gender=="55-64, male", 1,   
                                              ifelse(TM_MBS_PBS_complete$age_gender=="65+, female" | TM_MBS_PBS_complete$age_gender=="65+, male", 2,NA)))

TM_MBS_PBS_complete$GENDER = ifelse(TM_MBS_PBS_complete$age_gender=="<55, female" | TM_MBS_PBS_complete$age_gender=="55-64, female"|
                                      TM_MBS_PBS_complete$age_gender=="65+, female",1,
                                    ifelse(TM_MBS_PBS_complete$age_gender=="<55, male" | TM_MBS_PBS_complete$age_gender=="55-64, male"|
                                             TM_MBS_PBS_complete$age_gender=="65+, male",0, NA)) 

state.costs =  summaryBy(six_month_primary_care_cost_2014 ~ 
                           age.group + GENDER, data=subset(TM_MBS_PBS_complete, is.na(six_month_primary_care_cost_2014)==F), 
                         FUN=c(length,mean,sd))

state.costs = subset(state.costs, six_month_primary_care_cost_2014.length > 1) # need sample size of at least 2

# if group missing (due to small sample)
if(nrow(state.costs)<6){
  # calculate average
  mean.state.costs =  summaryBy(six_month_primary_care_cost_2014 ~ 1, data=subset(TM_MBS_PBS_complete, is.na(six_month_primary_care_cost_2014)==F), 
                           FUN=c(length,mean,sd))
  # find missing combs
  index = which(with(state.costs, table(age.group, GENDER))==0, arr.ind = T)
  for(k in 1:nrow(index)){
    frame = mean.state.costs
    frame$age.group = index[k,1] - 1
    frame$GENDER = index[k,2] - 1
    state.costs = rbind(state.costs, frame)
  }
}

# calculate parameters for gamma distribution
state.costs$six_month_primary_care_cost_2014.sem = state.costs$six_month_primary_care_cost_2014.sd / 
  sqrt(state.costs$six_month_primary_care_cost_2014.length)
state.costs$alpha = (state.costs$six_month_primary_care_cost_2014.mean
                     / state.costs$six_month_primary_care_cost_2014.sem)^2

state.costs$beta = (state.costs$six_month_primary_care_cost_2014.sem)^2 / state.costs$six_month_primary_care_cost_2014.mean
state.costs$random = NA
for (k in 1:6){
  state.costs$CHDCost[k] = rgamma(1, state.costs$alpha[k]+1, 1/(state.costs$beta[k]+1)) # sample from gamma distribution
}

state.costs$Gender = 'Male'
state.costs$Gender[state.costs$GENDER==1] = 'Female'
state.costs = subset(state.costs, select=c('Gender','age.group','CHDCost'))

return(state.costs)

}
