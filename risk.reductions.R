# risk.reductions.R
# function to randomly select risk reductions
# August 2016

risk.reduction = function(SBP, LDL_CHOL, smoking, age, under.null=FALSE){

## Baseline risk of an event
MI_risk_men = (1 - ((1 - 0.048)^(1/2))) # 4.8% annual rate to 6 month
Stroke_risk_men = (1 - ((1 - 0.016)^(1/2))) # 1.6% annual rate to 6 month
MI_risk_women = (1 - ((1 - 0.049)^(1/2))) # 4.9% annual rate to 6 month
Stroke_risk_women = (1 - ((1 - 0.022)^(1/2))) # 2.2% annual rate to 6 month

# death after event (see 'life tables'), not random
death.after.MI = rbind(data.frame(Gender='Male', prob=0.036),
            data.frame(Gender='Female', prob=0.036)) #
death.after.Stroke = rbind(data.frame(Gender='Male', prob=0.11),
              data.frame(Gender='Female', prob=0.11)) # 

### Risk reductions for MI and Stroke
# associated with changes in risk factors
# Based on the literature

## SBP
# men
SBP_change = -10
risk_reduction_MI_mean = 0.76
risk_reduction_MI_high = 0.86
risk_reduction_MI_low = 0.68
# if the null is true put risk at 1 (Sep 2018)
if(under.null == TRUE){
  rdiff = 1 - risk_reduction_MI_mean # difference to 1  
  risk_reduction_MI_mean = risk_reduction_MI_mean + rdiff
  risk_reduction_MI_high = risk_reduction_MI_high + rdiff # move CIs too
  risk_reduction_MI_low = risk_reduction_MI_low + rdiff
}
risk_reduction_MI_std_error = (risk_reduction_MI_high-
                 risk_reduction_MI_low)/3.92
risk_reduction_MI_random = rnorm(1,risk_reduction_MI_mean,
                 risk_reduction_MI_std_error)

risk_reduction_Stroke_mean = 0.65
risk_reduction_Stroke_high = 0.8
risk_reduction_Stroke_low = 0.53
if(under.null == TRUE){
  rdiff = 1 - risk_reduction_Stroke_mean # difference to 1  
  risk_reduction_Stroke_mean = risk_reduction_Stroke_mean + rdiff
  risk_reduction_Stroke_high = risk_reduction_Stroke_high + rdiff # move CIs too
  risk_reduction_Stroke_low = risk_reduction_Stroke_low + rdiff
}
risk_reduction_Stroke_std_error = (risk_reduction_Stroke_high-
                   risk_reduction_Stroke_low)/3.92
risk_reduction_Stroke_random = rnorm(1,risk_reduction_Stroke_mean,
                   risk_reduction_Stroke_std_error)

rr_SBP = data.frame(SBP_change,risk_reduction_MI_mean,risk_reduction_MI_random,
          risk_reduction_Stroke_mean,risk_reduction_Stroke_random)
rm(SBP_change,
  risk_reduction_MI_mean,risk_reduction_MI_high,
  risk_reduction_MI_low,risk_reduction_MI_std_error,risk_reduction_MI_random,
  risk_reduction_Stroke_mean,risk_reduction_Stroke_high,
  risk_reduction_Stroke_low,risk_reduction_Stroke_std_error,risk_reduction_Stroke_random)

# women: same as men
rr_SBP_women = rr_SBP
rr_SBP_women$Gender = 1

rr_SBP_men = rr_SBP
rr_SBP_men$Gender = 0

rr_SBP = rbind(rr_SBP_men,rr_SBP_women)
rm(rr_SBP_men,rr_SBP_women)

## LDL
# men
LDL_change = -1
risk_reduction_MI_mean = 0.74
risk_reduction_MI_high = 0.78
risk_reduction_MI_low = 0.7
if(under.null == TRUE){
  rdiff = 1 - risk_reduction_MI_mean # difference to 1  
  risk_reduction_MI_mean = risk_reduction_MI_mean + rdiff
  risk_reduction_MI_high = risk_reduction_MI_high + rdiff # move CIs too
  risk_reduction_MI_low = risk_reduction_MI_low + rdiff
}
risk_reduction_MI_std_error = (risk_reduction_MI_high-
                 risk_reduction_MI_low)/3.92
risk_reduction_MI_random = rnorm(1,risk_reduction_MI_mean,
                 risk_reduction_MI_std_error)

risk_reduction_Stroke_mean = 0.83
risk_reduction_Stroke_high = 0.90
risk_reduction_Stroke_low = 0.76
if(under.null == TRUE){
  rdiff = 1 - risk_reduction_Stroke_mean # difference to 1  
  risk_reduction_Stroke_mean = risk_reduction_Stroke_mean + rdiff
  risk_reduction_Stroke_high = risk_reduction_Stroke_high + rdiff # move CIs too
  risk_reduction_Stroke_low = risk_reduction_Stroke_low + rdiff
}
risk_reduction_Stroke_std_error = (risk_reduction_Stroke_high-
                   risk_reduction_Stroke_low)/3.92
risk_reduction_Stroke_random = rnorm(1,risk_reduction_Stroke_mean,
                   risk_reduction_Stroke_std_error)

rr_LDL = data.frame(LDL_change,risk_reduction_MI_mean,risk_reduction_MI_random,
          risk_reduction_Stroke_mean,risk_reduction_Stroke_random)
rm(LDL_change,
  risk_reduction_MI_mean,risk_reduction_MI_high,
  risk_reduction_MI_low,risk_reduction_MI_std_error,risk_reduction_MI_random,
  risk_reduction_Stroke_mean,risk_reduction_Stroke_high,
  risk_reduction_Stroke_low,risk_reduction_Stroke_std_error,risk_reduction_Stroke_random)

# women
LDL_change = -1
risk_reduction_MI_mean = 0.83
risk_reduction_MI_high = 0.93
risk_reduction_MI_low = 0.74
if(under.null == TRUE){
  rdiff = 1 - risk_reduction_MI_mean # difference to 1  
  risk_reduction_MI_mean = risk_reduction_MI_mean + rdiff
  risk_reduction_MI_high = risk_reduction_MI_high + rdiff # move CIs too
  risk_reduction_MI_low = risk_reduction_MI_low + rdiff
}
risk_reduction_MI_std_error = (risk_reduction_MI_high-
                 risk_reduction_MI_low)/3.92
risk_reduction_MI_random = rnorm(1,risk_reduction_MI_mean,
                 risk_reduction_MI_std_error)

risk_reduction_Stroke_mean = 0.9
risk_reduction_Stroke_high = 1.04
risk_reduction_Stroke_low = 0.78
if(under.null == TRUE){
  rdiff = 1 - risk_reduction_Stroke_mean # difference to 1  
  risk_reduction_Stroke_mean = risk_reduction_Stroke_mean + rdiff
  risk_reduction_Stroke_high = risk_reduction_Stroke_high + rdiff # move CIs too
  risk_reduction_Stroke_low = risk_reduction_Stroke_low + rdiff
}
risk_reduction_Stroke_std_error = (risk_reduction_Stroke_high-
                   risk_reduction_Stroke_low)/3.92
risk_reduction_Stroke_random = rnorm(1,risk_reduction_Stroke_mean,
                   risk_reduction_Stroke_std_error)

rr_LDL_women = data.frame(LDL_change,risk_reduction_MI_mean,risk_reduction_MI_random,
             risk_reduction_Stroke_mean,risk_reduction_Stroke_random)

rr_LDL_women$Gender = 1

rr_LDL_men = rr_LDL
rr_LDL_men$Gender = 0

rr_LDL = rbind(rr_LDL_men,rr_LDL_women)
rm(rr_LDL_men,rr_LDL_women)

## Smoking
# men
smoking_change = "Quit"
risk_reduction_MI_mean = 0.68
risk_reduction_MI_high = 0.82
risk_reduction_MI_low = 0.57
if(under.null == TRUE){
  rdiff = 1 - risk_reduction_MI_mean # difference to 1  
  risk_reduction_MI_mean = risk_reduction_MI_mean + rdiff
  risk_reduction_MI_high = risk_reduction_MI_high + rdiff # move CIs too
  risk_reduction_MI_low = risk_reduction_MI_low + rdiff
}
risk_reduction_MI_std_error = (risk_reduction_MI_high-
                 risk_reduction_MI_low)/3.92
risk_reduction_MI_random = rnorm(1,risk_reduction_MI_mean,
                 risk_reduction_MI_std_error)

risk_reduction_Stroke_mean = 0.60
risk_reduction_Stroke_high = 0.67
risk_reduction_Stroke_low = 0.53
if(under.null == TRUE){
  rdiff = 1 - risk_reduction_Stroke_mean # difference to 1  
  risk_reduction_Stroke_mean = risk_reduction_Stroke_mean + rdiff
  risk_reduction_Stroke_high = risk_reduction_Stroke_high + rdiff # move CIs too
  risk_reduction_Stroke_low = risk_reduction_Stroke_low + rdiff
}
risk_reduction_Stroke_std_error = (risk_reduction_Stroke_high-
                   risk_reduction_Stroke_low)/3.92
risk_reduction_Stroke_random = rnorm(1,risk_reduction_Stroke_mean,
                   risk_reduction_Stroke_std_error)

rr_smoking = data.frame(smoking_change,risk_reduction_MI_mean,risk_reduction_MI_random,
            risk_reduction_Stroke_mean,risk_reduction_Stroke_random)
rm(smoking_change,
  risk_reduction_MI_mean,risk_reduction_MI_high,
  risk_reduction_MI_low,risk_reduction_MI_std_error,risk_reduction_MI_random,
  risk_reduction_Stroke_mean,risk_reduction_Stroke_high,
  risk_reduction_Stroke_low,risk_reduction_Stroke_std_error,risk_reduction_Stroke_random)


# women
smoking_change = "Quit"
risk_reduction_MI_mean = 0.68
risk_reduction_MI_high = 0.82
risk_reduction_MI_low = 0.57
if(under.null == TRUE){
  rdiff = 1 - risk_reduction_MI_mean # difference to 1  
  risk_reduction_MI_mean = risk_reduction_MI_mean + rdiff
  risk_reduction_MI_high = risk_reduction_MI_high + rdiff # move CIs too
  risk_reduction_MI_low = risk_reduction_MI_low + rdiff
}
risk_reduction_MI_std_error = (risk_reduction_MI_high-
                 risk_reduction_MI_low)/3.92
risk_reduction_MI_random = rnorm(1,risk_reduction_MI_mean,
                 risk_reduction_MI_std_error)

risk_reduction_Stroke_mean = 0.55
risk_reduction_Stroke_high = 0.63
risk_reduction_Stroke_low = 0.47
if(under.null == TRUE){
  rdiff = 1 - risk_reduction_Stroke_mean # difference to 1  
  risk_reduction_Stroke_mean = risk_reduction_Stroke_mean + rdiff
  risk_reduction_Stroke_high = risk_reduction_Stroke_high + rdiff # move CIs too
  risk_reduction_Stroke_low = risk_reduction_Stroke_low + rdiff
}
risk_reduction_Stroke_std_error = (risk_reduction_Stroke_high-
                   risk_reduction_Stroke_low)/3.92
risk_reduction_Stroke_random = rnorm(1,risk_reduction_Stroke_mean,
                   risk_reduction_Stroke_std_error)

rr_smoking_women = data.frame(smoking_change,risk_reduction_MI_mean,risk_reduction_MI_random,
               risk_reduction_Stroke_mean,risk_reduction_Stroke_random)
rm(smoking_change,
  risk_reduction_MI_mean,risk_reduction_MI_high,
  risk_reduction_MI_low,risk_reduction_MI_std_error,risk_reduction_MI_random,
  risk_reduction_Stroke_mean,risk_reduction_Stroke_high,
  risk_reduction_Stroke_low,risk_reduction_Stroke_std_error,risk_reduction_Stroke_random)


rr_smoking_women$Gender = 1

rr_smoking_men = rr_smoking
rr_smoking_men$Gender = 0

rr_smoking = rbind(rr_smoking_men,rr_smoking_women)
rm(rr_smoking_men,rr_smoking_women)


## TEXT ME risk reductions
# men
# MI 
MI_rr_SBP_men = rr_SBP$risk_reduction_MI_random[1]^((SBP$random[1] /rr_SBP$SBP_change[1]))
MI_rr_LDL_men = rr_LDL$risk_reduction_MI_random[1]^((LDL_CHOL$random[1] /rr_LDL$LDL_change[1]))
MI_rr_smoking_men = 1 - (1 - rr_smoking$risk_reduction_MI_random[1])*(abs(smoking$random[1]))
# Stroke
Stroke_rr_SBP_men = rr_SBP$risk_reduction_Stroke_random[1]^((SBP$random[1] /rr_SBP$SBP_change[1]))
Stroke_rr_LDL_men = rr_LDL$risk_reduction_Stroke_random[1]^((LDL_CHOL$random[1] /rr_LDL$LDL_change[1]))
Stroke_rr_smoking_men = 1 - (1 - rr_smoking$risk_reduction_Stroke_random[1])*(abs(smoking$random[1]))

RR_men = data.frame(MI_rr_SBP_men,MI_rr_LDL_men,MI_rr_smoking_men,
          Stroke_rr_SBP_men,Stroke_rr_LDL_men,Stroke_rr_smoking_men) # random variation in here (AGB)
RR_men = RR_men[rep(seq_len(nrow(RR_men)), each=101),]
rm(MI_rr_SBP_men,MI_rr_LDL_men,MI_rr_smoking_men,Stroke_rr_SBP_men,Stroke_rr_LDL_men,Stroke_rr_smoking_men) 

RR_men$Cycle = c(0:100)
RR_men$year = (0:100)/2
RR_men$delay = c(0.25,0.25,rep.int(0,99))
RR_men$attenuation = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,rep.int(1,91))

#
RR_men$MI_baseline_risk = MI_risk_men
RR_men$MI_rr = 1 - (1 - RR_men$delay-RR_men$attenuation)*
 (1 - (RR_men$MI_rr_SBP_men*RR_men$MI_rr_LDL_men*
    RR_men$MI_rr_smoking_men)) 
RR_men$TM_risk_MI = RR_men$MI_rr*RR_men$MI_baseline_risk

RR_men$Stroke_baseline_risk = Stroke_risk_men
RR_men$Stroke_rr = 1 - (1 - RR_men$delay-RR_men$attenuation)*
 (1 - (RR_men$Stroke_rr_SBP_men*RR_men$Stroke_rr_LDL_men*
    RR_men$Stroke_rr_smoking_men)) 
RR_men$TM_risk_Stroke = RR_men$Stroke_rr*RR_men$Stroke_baseline_risk # randomness still here (AGB)

## women
# MI 
MI_rr_SBP_women = rr_SBP$risk_reduction_MI_random[2]^((SBP$random[2] /rr_SBP$SBP_change[2]))
MI_rr_LDL_women = rr_LDL$risk_reduction_MI_random[2]^((LDL_CHOL$random[2] /rr_LDL$LDL_change[2]))
MI_rr_smoking_women = 1 - (1 - rr_smoking$risk_reduction_MI_random[2])*(abs(smoking$random[2]))
# Stroke
Stroke_rr_SBP_women = rr_SBP$risk_reduction_Stroke_random[2]^((SBP$random[2]/rr_SBP$SBP_change[2]))
Stroke_rr_LDL_women = rr_LDL$risk_reduction_Stroke_random[2]^((LDL_CHOL$random[2] /rr_LDL$LDL_change[2]))
Stroke_rr_smoking_women = 1 - (1 - rr_smoking$risk_reduction_Stroke_random[2])*(abs(smoking$random[2]))

RR_women = data.frame(MI_rr_SBP_women,MI_rr_LDL_women,MI_rr_smoking_women,
           Stroke_rr_SBP_women,Stroke_rr_LDL_women,Stroke_rr_smoking_women)
RR_women = RR_women[rep(seq_len(nrow(RR_women)), each=101),]
rm(MI_rr_SBP_women,MI_rr_LDL_women,MI_rr_smoking_women,Stroke_rr_SBP_women,Stroke_rr_LDL_women,Stroke_rr_smoking_women)

RR_women$Cycle = c(0:100)
RR_women$year = (0:100)/2
RR_women$delay = c(0.25,0.25,rep.int(0,99))
RR_women$attenuation = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,rep.int(1,91))

#
RR_women$MI_baseline_risk = MI_risk_women
RR_women$MI_rr = 1 - (1 - RR_women$delay-RR_women$attenuation)*
 (1 - (RR_women$MI_rr_SBP_women*RR_women$MI_rr_LDL_women*
    RR_women$MI_rr_smoking_women)) 
RR_women$TM_risk_MI = RR_women$MI_rr*RR_women$MI_baseline_risk

RR_women$Stroke_baseline_risk = Stroke_risk_women
RR_women$Stroke_rr = 1 - (1 - RR_women$delay-RR_women$attenuation)*
 (1 - (RR_women$Stroke_rr_SBP_women*RR_women$Stroke_rr_LDL_women*
    RR_women$Stroke_rr_smoking_women)) 
RR_women$TM_risk_Stroke = RR_women$Stroke_rr*RR_women$Stroke_baseline_risk

## transitions
# usual care groups have baseline risks
# text me groups have risk reduced based on their changes in behaviour....

## men_usual_care
transitions_men_usual_care = data.frame(Cycle = 0:100)
transitions_men_usual_care$year = (0:100)/2

# add in age for merging with death
transitions_men_usual_care$Age = round(age$starting.age[1]+
                     transitions_men_usual_care$year)
transitions_men_usual_care$Gender = "Male"

transitions_men_usual_care$CHD2MI = MI_risk_men # not random in usual care group (AGB)
transitions_men_usual_care$CHD2MI = ifelse(transitions_men_usual_care$Age >= 100,0,
                      transitions_men_usual_care$CHD2MI) # replace risk with zero if age over 100

transitions_men_usual_care$CHD2Stroke = Stroke_risk_men # not random (AGB)
transitions_men_usual_care$CHD2Stroke = ifelse(transitions_men_usual_care$Age >= 100,0,
                        transitions_men_usual_care$CHD2Stroke)

transitions_men_usual_care = merge(transitions_men_usual_care, life.table, by=c("Age", "Gender"), all.x=T) # add death
names(transitions_men_usual_care)[names(transitions_men_usual_care)=='prob'] = 'CHD2Dead'
transitions_men_usual_care$CHD2Dead = ifelse(transitions_men_usual_care$Age >= 100, 1,
                       transitions_men_usual_care$CHD2Dead )

transitions_men_usual_care$CHD2CHD = 1 - transitions_men_usual_care$CHD2MI-
 transitions_men_usual_care$CHD2Stroke-transitions_men_usual_care$CHD2Dead

transitions_men_usual_care$MI2Dead = death.after.MI$prob[1]
transitions_men_usual_care$MI2Dead = ifelse(transitions_men_usual_care$Age >= 100, 1,
                      transitions_men_usual_care$MI2Dead)
transitions_men_usual_care$MI2HistoryMI = 1 - transitions_men_usual_care$MI2Dead # update Jan 2017

transitions_men_usual_care$Stroke2Dead = death.after.Stroke$prob[1]
transitions_men_usual_care$Stroke2Dead = ifelse(transitions_men_usual_care$Age>=100, 1,
                        transitions_men_usual_care$Stroke2Dead)
transitions_men_usual_care$Stroke2HistoryStroke = 1 - transitions_men_usual_care$Stroke2Dead # update Jan 2017

transitions_men_usual_care$HistoryMI2Dead = transitions_men_usual_care$CHD2Dead # update Jan 2017
transitions_men_usual_care$HistoryStroke2Dead = transitions_men_usual_care$CHD2Dead # update Jan 2017

transitions_men_usual_care$HistoryMI2HistoryMI = 1 - transitions_men_usual_care$HistoryMI2Dead # update Jan 2017
transitions_men_usual_care$HistoryStroke2HistoryStroke = 1 - transitions_men_usual_care$HistoryStroke2Dead # update Jan 2017

## women_usual_care
Cycle = 0:100
transitions_women_usual_care = data.frame(Cycle)
rm(Cycle)
transitions_women_usual_care$year = (0:100)/2

transitions_women_usual_care$Age = round(age$starting.age[2]+
                      transitions_women_usual_care$year)
transitions_women_usual_care$Gender = "Female"

transitions_women_usual_care$CHD2MI = MI_risk_women # not random in usual care group
transitions_women_usual_care$CHD2MI = ifelse(transitions_women_usual_care$Age >= 100, 0,
                       transitions_women_usual_care$CHD2MI)

transitions_women_usual_care$CHD2Stroke = Stroke_risk_women
transitions_women_usual_care$CHD2Stroke = ifelse(transitions_women_usual_care$Age >= 100, 0,
                         transitions_women_usual_care$CHD2Stroke)

transitions_women_usual_care = merge(transitions_women_usual_care,life.table, by=c("Age", "Gender"), all.x=T)
names(transitions_women_usual_care)[names(transitions_women_usual_care)=='prob'] = 'CHD2Dead'
transitions_women_usual_care$CHD2Dead = ifelse(transitions_women_usual_care$Age>=100,1,
                        transitions_women_usual_care$CHD2Dead )

transitions_women_usual_care$CHD2CHD = 1 - transitions_women_usual_care$CHD2MI-
 transitions_women_usual_care$CHD2Stroke-transitions_women_usual_care$CHD2Dead

transitions_women_usual_care$MI2Dead = death.after.MI$prob[2]
transitions_women_usual_care$MI2Dead = ifelse(transitions_women_usual_care$Age >= 100, 1,
                       transitions_women_usual_care$MI2Dead)
transitions_women_usual_care$MI2HistoryMI = 1 - transitions_women_usual_care$MI2Dead

transitions_women_usual_care$Stroke2Dead = death.after.Stroke$prob[2]
transitions_women_usual_care$Stroke2Dead = ifelse(transitions_women_usual_care$Age >= 100, 1,
                         transitions_women_usual_care$Stroke2Dead)
transitions_women_usual_care$Stroke2HistoryStroke = 1 - transitions_women_usual_care$Stroke2Dead

transitions_women_usual_care$HistoryMI2Dead = transitions_women_usual_care$CHD2Dead
transitions_women_usual_care$HistoryMI2HistoryMI = 1 - transitions_women_usual_care$HistoryMI2Dead

transitions_women_usual_care$HistoryStroke2Dead = transitions_women_usual_care$CHD2Dead
transitions_women_usual_care$HistoryStroke2HistoryStroke = 1 - transitions_women_usual_care$HistoryStroke2Dead

## men, in Text_Me group
Cycle = 0:100
transitions_men_Text_Me = data.frame(Cycle)
rm(Cycle)
transitions_men_Text_Me$year = (0:100)/2

transitions_men_Text_Me$Age = round(age$starting.age[1]+
                   transitions_men_Text_Me$year)
transitions_men_Text_Me$Gender = "Male"

transitions_men_Text_Me$CHD2MI = RR_men$TM_risk_MI # random in Text me group (AGB)
transitions_men_Text_Me$CHD2MI = ifelse(transitions_men_Text_Me$Age >= 100, 0,
                    transitions_men_Text_Me$CHD2MI)

transitions_men_Text_Me$CHD2Stroke = RR_men$TM_risk_Stroke
transitions_men_Text_Me$CHD2Stroke = ifelse(transitions_men_Text_Me$Age >= 100, 0,
                      transitions_men_Text_Me$CHD2Stroke)

transitions_men_Text_Me = merge(transitions_men_Text_Me,life.table, by=c("Age", "Gender"), all.x=T)
names(transitions_men_Text_Me)[names(transitions_men_Text_Me)=='prob'] = 'CHD2Dead'
transitions_men_Text_Me$CHD2Dead = ifelse(transitions_men_Text_Me$Age >= 100, 1,
                     transitions_men_Text_Me$CHD2Dead )

transitions_men_Text_Me$CHD2CHD = 1 - transitions_men_Text_Me$CHD2MI-
 transitions_men_Text_Me$CHD2Stroke-transitions_men_Text_Me$CHD2Dead

transitions_men_Text_Me$MI2Dead = death.after.MI$prob[1]
transitions_men_Text_Me$MI2Dead = ifelse(transitions_men_Text_Me$Age >= 100, 1,
                     transitions_men_Text_Me$MI2Dead)
transitions_men_Text_Me$MI2HistoryMI = 1 - transitions_men_Text_Me$MI2Dead

transitions_men_Text_Me$Stroke2Dead = death.after.Stroke$prob[1]
transitions_men_Text_Me$Stroke2Dead = ifelse(transitions_men_Text_Me$Age >= 100, 1,
                       transitions_men_Text_Me$Stroke2Dead)
transitions_men_Text_Me$Stroke2HistoryStroke = 1 - transitions_men_Text_Me$Stroke2Dead

# updated Jan 2017
transitions_men_Text_Me$HistoryMI2Dead = transitions_men_Text_Me$CHD2Dead
transitions_men_Text_Me$HistoryMI2HistoryMI = 1 - transitions_men_Text_Me$HistoryMI2Dead

# updated Jan 2017
transitions_men_Text_Me$HistoryStroke2Dead = transitions_men_Text_Me$CHD2Dead
transitions_men_Text_Me$HistoryStroke2HistoryStroke = 1 - transitions_men_Text_Me$HistoryStroke2Dead


## women, Text_Me group
Cycle = 0:100
transitions_women_Text_Me = data.frame(Cycle)
rm(Cycle)
transitions_women_Text_Me$year = (0:100)/2

transitions_women_Text_Me$Age = round(age$starting.age[2]+
                    transitions_women_Text_Me$year)
transitions_women_Text_Me$Gender = "Female"

transitions_women_Text_Me$CHD2MI = RR_women$TM_risk_MI
transitions_women_Text_Me$CHD2MI = ifelse(transitions_women_Text_Me$Age >= 100, 0,
                     transitions_women_Text_Me$CHD2MI)

transitions_women_Text_Me$CHD2Stroke = RR_women$TM_risk_Stroke
transitions_women_Text_Me$CHD2Stroke = ifelse(transitions_women_Text_Me$Age >= 100, 0,
                       transitions_women_Text_Me$CHD2Stroke)

transitions_women_Text_Me = merge(transitions_women_Text_Me,life.table, by=c("Age", "Gender"), all.x=T)
names(transitions_women_Text_Me)[names(transitions_women_Text_Me)=='prob'] = 'CHD2Dead'
transitions_women_Text_Me$CHD2Dead = ifelse(transitions_women_Text_Me$Age >= 100, 1,
                      transitions_women_Text_Me$CHD2Dead )

transitions_women_Text_Me$CHD2CHD = 1 - transitions_women_Text_Me$CHD2MI-
 transitions_women_Text_Me$CHD2Stroke-transitions_women_Text_Me$CHD2Dead

transitions_women_Text_Me$MI2Dead = death.after.MI$prob[2]
transitions_women_Text_Me$MI2Dead = ifelse(transitions_women_Text_Me$Age >= 100, 1,
                      transitions_women_Text_Me$MI2Dead)
transitions_women_Text_Me$MI2HistoryMI = 1 - transitions_women_Text_Me$MI2Dead

transitions_women_Text_Me$Stroke2Dead = death.after.Stroke$prob[2]
transitions_women_Text_Me$Stroke2Dead = ifelse(transitions_women_Text_Me$Age>=100,1,
                        transitions_women_Text_Me$Stroke2Dead)
transitions_women_Text_Me$Stroke2HistoryStroke = 1 - transitions_women_Text_Me$Stroke2Dead

transitions_women_Text_Me$HistoryMI2Dead = transitions_women_Text_Me$CHD2Dead
transitions_women_Text_Me$HistoryMI2HistoryMI = 1 - transitions_women_Text_Me$HistoryMI2Dead

transitions_women_Text_Me$HistoryStroke2Dead = transitions_women_Text_Me$CHD2Dead
transitions_women_Text_Me$HistoryStroke2HistoryStroke = 1 - transitions_women_Text_Me$HistoryStroke2Dead

# concatenate
transitions_women_usual_care$Group = "Usual care"
transitions_men_usual_care$Group = "Usual care"
transitions_women_Text_Me$Group = "TextMe"
transitions_men_Text_Me$Group = "TextMe"
transitions = rbind(transitions_men_usual_care, transitions_women_usual_care, 
          transitions_men_Text_Me, transitions_women_Text_Me)

transitions$checkCHD

transitions$age.group = as.numeric(transitions$Age>55) + as.numeric(transitions$Age>65) # for merge with costs

# return
return(transitions)
}
