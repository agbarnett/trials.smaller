# Simulate.R
# Dec 2016
library(reshape2)
library(doBy)
#library(stringr)
library(plyr)
library(dplyr)
#library(broom)

# additional code
source('Life.Table.R')
source('text.me.sample.random.R') # to get random data from the TEXT ME trial (note this data is simulated as we don't have the rights to share the actual trial data)
source('state.costs.R') # 
source('event.costs.R') # 
source('risk.reductions.R') # 

#### function to run a cost-effectiveness model
# n.bigsim = number of random selections from TextMe data
# n.sim = nested stochastic simulations per sample size
run.one = function(sample.size = 400, n.bigsim = 1, n.sim = 500, threshold = 64000){
 
 cohort.size = 50000 # hypothetical cohort size
 discount = 0.03 # 3% discount for future costs and QALYs
 
 numbers.over.time = sim.frame = NULL
 for (s1 in 1:n.bigsim){ # big simulation (sample from text me)
  
 # get a random sample from the text me data
 text.me.all = text.me.sample(sample.size = sample.size)
 attach(text.me.all, warn.conflicts = F)
 
 for (s2 in 1:n.sim){ # nested simulation (stochastic samples)
   # randomly sample QALY (update jan 2017)
   Q.CVD = rbeta(1, Q.CVD.alpha, Q.CVD.beta) ## from data
   Q.MI = rbeta(1, 144.53, 71.19) ## from Excel
   Q.Stroke = rbeta(1, 159.02, 101.67)  # from Excel
   Q.Post.Stroke = rbeta(1, 115.63, 70.87)  # from Excel
   Q.CVD = Q.CVD / 2 # half year
   Q.MI = Q.MI / 2 # half year
   Q.Stroke = Q.Stroke / 2 # half year
   Q.Post.Stroke = Q.Post.Stroke / 2 # half year
   
   # randomly sample costs (Aug 2016)
   state.costs = random.state.costs(text.me.data, TM_MBS_PBS_complete=mbs.pbs.data)
   e.costs = random.event.costs()
   # randomly sample intervention costs
   intervention = 33.15 * runif(n=1, min=750, max=1250)/1000 # cost per person (resampled every simulation, July 2016)
   
   ## stochastic model
 time = seq(0, 60, 0.5) # half year jumps
 int.costs = cohort.size * intervention # intervention costs per 1000, add to women in TextMe group in next line
 frame = data.frame(cycle = 0, CHD = cohort.size, 
      Group = c('Usual care', 'Usual care', 'TextMe', 'TextMe'), 
      Gender = c('Male', 'Female', 'Male', 'Female'), 
      MI = 0, Stroke = 0, HistoryMI = 0, HistoryStroke = 0, Dead = 0, QALYs = 0, Costs = c(0, 0, 0, int.costs))
 frame = merge(frame, Proportions, by = 'Gender')
 ## randomly allocate gender
 # split by men:women, numbers need to be the same
 men = sum(rbinom(n = cohort.size, size = 1, prob = subset(frame, Gender == 'Male')$Expected[1])) # Says 'expected', but is actually stochastic
 women = cohort.size - men
 frame$CHD[frame$Gender == 'Female'] = women # this makes sure numbers are same by treatment group
 frame$CHD[frame$Gender == 'Male'] = men 
 
 # randomly allocate age (Aug 2016)
 age$starting.age = NA
 age$starting.age[1] = rnorm(1, age$AGE.mean[1], age$AGE.sd[1]) # 'risk appraisal' - men
 age$starting.age[2] = rnorm(1, age$AGE.mean[2], age$AGE.sd[2]) # 'risk appraisal' - women
 
 # random SBP treatment effect
 SBP$random = NA
 SBP$random[1] = rnorm(1, SBP$estimate[1], SBP$std.error[1])
 SBP$random[2] = rnorm(1, SBP$estimate[2], SBP$std.error[2])
 
 # random LDL treatment (use function with missing for small group?)
 LDL_CHOL$random = NA
 LDL_CHOL$random[1] = rnorm(1, LDL_CHOL$estimate[1], LDL_CHOL$std.error[1])
 LDL_CHOL$random[2] = rnorm(1, LDL_CHOL$estimate[2], LDL_CHOL$std.error[2])
 
 # random smoking treatment effect
 smoking$random = NA
 smoking$random[1] = rnorm(1, smoking$Expected[1], smoking$Std.error[1])
 smoking$random[2] = rnorm(1, smoking$Expected[2], smoking$Std.error[2])
 
 # random risk reductions, gets transitions over time
 transitions = risk.reduction(SBP, LDL_CHOL, smoking, age) # randomness in TextMe group

 # Move forward in time
 stochastic = NULL
 for (this.cycle in 1:100){ # loop through time
  frame = subset(frame, select = c('cycle', 'Group', 'Gender', 'CHD', 'MI', 'Stroke', 'HistoryMI', 'HistoryStroke', 'Dead', 'QALYs', 'Costs'))
  stochastic = rbind(stochastic, frame) # update with previous frame
  this = subset(transitions, Cycle == this.cycle)
 if( nrow(this)>0 ){ # only if age under 100, otherwise no data
  this = merge(subset(frame, cycle == this.cycle-1), this, by = c('Gender', 'Group'))
  this = merge(this, state.costs, by = c('age.group', 'Gender'))
  e.costs$Gender = str_trim(e.costs$Gender) # 
  this = merge(this, e.costs, by = c('Gender', 'Age'), all.x = T)
  this$MI_Cost = ifelse(this$Age>100, 0, this$MI_Cost)
  this$Stroke_Cost = ifelse(this$Age>100, 0, this$Stroke_Cost)
  frame = this # new frame
  frame$cycle = this.cycle
  frame$Q.CVD = Q.CVD
  frame$Q.MI = Q.MI
  frame$Q.Stroke = Q.Stroke
  frame$Q.Post.Stroke = Q.Post.Stroke
  
# using probabilistic moves instead of deterministic (July 2016)
  deterministic = T
  for (k in 1:nrow(this)){
    if(deterministic==T){
     chd.moves = this$CHD[k] * c(this$CHD2CHD[k], this$CHD2MI[k], this$CHD2Stroke[k], this$CHD2Dead[k]) # deterministic 
     MI.moves = this$MI[k] *c(this$MI2HistoryMI[k], this$MI2Dead[k]) # deterministic
     Stroke.moves = this$Stroke[k]*c(this$Stroke2HistoryStroke[k], this$Stroke2Dead[k]) # deterministic
     HistoryMI.moves = this$HistoryMI[k]*c(this$HistoryMI2HistoryMI[k], this$HistoryMI2Dead[k]) # deterministic
     HistoryStroke.moves = this$HistoryStroke[k]*c(this$HistoryStroke2HistoryStroke[k], this$HistoryStroke2Dead[k]) # deterministic
    }
    if(deterministic==F){ # not updated in Dec 2016
      chd.moves = rowSums(rmultinom(n = this$CHD[k], size = 1, 
          prob = c(this$CHD2CHD[k], 
            this$CHD2MI[k], this$CHD2Stroke[k], this$CHD2Dead[k])))
      MI.moves = rowSums(rmultinom(n = this$MI[k], size = 1, 
             prob = c(this$MI2HistoryMI[k], this$MI2Dead[k])))
      Stroke.moves = rowSums(rmultinom(n = this$Stroke[k], size = 1, 
                                       prob = c(this$Stroke2HistoryStroke[k], this$Stroke2Dead[k])))
      History.moves = rowSums(rmultinom(n = this$History[k], size = 1, 
                                        prob = c(this$History2History[k], this$History2Dead[k])))
    }
   # add up transition numbers
   frame$CHD[k] = chd.moves[1]  # remaining in CHD
   frame$MI[k] = chd.moves[2] # moving to MI from CHD
   frame$Stroke[k] = chd.moves[3]
   frame$HistoryMI[k] = MI.moves[1] + HistoryMI.moves[1] # pick-ups from MI and those who stay in history
   frame$HistoryStroke[k] = Stroke.moves[1] + HistoryStroke.moves[1] # pick-ups from stroke and those who stay in history
   frame$Dead[k] = this$Dead[k] + chd.moves[4] + MI.moves[2] + Stroke.moves[2] + HistoryMI.moves[2] + HistoryStroke.moves[2]
   # add up QALYs
   frame$QALYs[k] = ((frame$Q.CVD[k]*frame$CHD[k])
                     +(frame$Q.MI[k]*frame$MI[k])
                     +(frame$Q.Stroke[k]*frame$Stroke[k])
                     +(frame$Q.CVD[k]*frame$HistoryMI[k])
                     +(frame$Q.Post.Stroke[k]*frame$HistoryStroke[k]))/((1+discount)^(frame$year[k])) # added history Aug 2016
## quote from Ed about above: "Those who reach the history of MI state return to the same Qol of the history of CHD state"
# add up costs
   frame$Costs[k] = ( 
    (frame$CHD[k]*frame$CHDCost[k])
   +(frame$MI[k]*frame$CHDCost[k])
   +(frame$MI[k]*frame$MI_Cost[k])
   +(frame$Stroke[k]*frame$CHDCost[k])+
   +(frame$Stroke[k]*frame$Stroke_Cost[k]) # 
   +(frame$HistoryMI[k]*frame$CHDCost[k])
   +(frame$HistoryStroke[k]*frame$CHDCost[k])) / ((1+discount)^(frame$year[k]))
  }
  }
 }
 
 # store numbers over time
 stochastic$bigsim = s1
 stochastic$sim = s2
 numbers.over.time = rbind(numbers.over.time, stochastic)
 
 # 'Results' sheet
 sums = summaryBy(Costs + QALYs ~ Group, FUN = sum, data = stochastic)
 names(sums)[2:3] = c('Costs', 'QALYs')
 frame = data.frame(Group = 'Difference', Costs = sums$Costs[1] - sums$Costs[2], QALYs = sums$QALYs[1] - sums$QALYs[2])
 sums = rbind(sums, frame)
 sums$ICER = sums$Costs / sums$QALYs
 
 s.frame = data.frame(bigsim = s1, nestsim = s2, sample.size = sample.size, QALY.UC=sums$QALYs[2], QALY.TM=sums$QALYs[1], QALY = sums$QALYs[3], Costs = sums$Costs[3], ICER = sums$ICER[3], CE = sums$ICER[3] < threshold)
 sim.frame = rbind(sim.frame, s.frame)
 
 if(s2%%100 == 0){cat('inner loop = ', s2, '\n')}

 } # end of nested simulation

 cat('outer loop = ', s1, '\n')
 
} # end of big simulation
 
 # return
 to.return = list()
 to.return$numbers = numbers.over.time
 to.return$CE = sim.frame
 return(to.return)

} # end of function

# test run (commented out)
# threshold is the cost-effectiveness threshold, set at $64,000 pera QALY
dir.to.use=getwd()
result = run.one(sample.size = 300, n.sim = 100, threshold = 64000) #
