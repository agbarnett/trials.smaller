# text.me.sample.random.R
# Make a random sample that looks like the TEXT ME data
# created for sharing purposes because we can't share the actual data
# July 2018


text.me.sample = function(sample.size=710){
  
  # make random data
  text.me.data = data.frame(SubjectID=1:sample.size, 
                            TRTGP = rbinom(size=1, n=sample.size, prob=0.5), # treatment group
                            AGE = runif(min=31, max=87, n=sample.size), # age
                            GENDER = rbinom(size=1, n=sample.size, prob=0.18), # Gender, 0=Men, 1=Women (many more men)
                            SBP.0 = rnorm(mean=129, sd=11, n=sample.size), # systolic blood pressure at baseline
                            SBP.6 = rnorm(mean=134, sd=16, n=sample.size), # systolic blood pressure at six months
                            LDL_CHOL.0 = rnorm(mean=2.51, sd=0.955, n=sample.size), # LDL cholesterol at baseline
                            LDL_CHOL.6 = rnorm(mean=2.03, sd=0.709, n=sample.size), # LDL cholesterol at six months
                            F_CURRENTSMOKER.0 = rbinom(size=1, n=sample.size, prob=0.52), # smoking baseline
                            F_CURRENTSMOKER.6 = rbinom(size=1, n=sample.size, prob=0.33) # smoking at six months
                            )
  
  # Gender
  gender = table(text.me.data$GENDER)
  ## Proportions # see 'Inputs'
  men = rbeta(1, gender[1], gender[2])
  Proportions = rbind(data.frame(Gender='Male', Expected=men),
                      data.frame(Gender='Female', Expected=1-men))
  
  # QALYs
  Q.CVD.mean = 0.729
  Q.CVD.sd = 0.0679
  Q.CVD.sem = 0.00187
  Q.CVD.alpha = Q.CVD.mean*(Q.CVD.mean*(1-Q.CVD.mean)/(Q.CVD.sem^2)-1)
  Q.CVD.beta =  Q.CVD.mean*(1-Q.CVD.mean)/(Q.CVD.sem^2)-1-Q.CVD.alpha
  
  # age stats
  age = summaryBy(AGE ~ GENDER, data=text.me.data, FUN=c(mean,sd))

  ### SBP stats
  SBP = text.me.data %>% group_by(GENDER) %>%
    do(SBP_test = lm(SBP.6 ~ SBP.0 + TRTGP, data = .))
  SBP = tidy(SBP, SBP_test)
  SBP = subset(SBP, term=="TRTGP", select=c("GENDER", "estimate", "std.error"))
  # if sample too small, then ignore gender (August)
  if(nrow(SBP)<2){
    SBP = text.me.data %>% do(SBP_test = lm(SBP.6 ~ SBP.0 + TRTGP, data = .))
    SBP = tidy(SBP, SBP_test)
    SBP = subset(SBP, term=="TRTGP", select=c("estimate", "std.error"))
    S1 = S2 = SBP;
    S1$GENDER = 0;
    S2$GENDER = 1;
    SBP = rbind(S1, S2)
  }
  # p-value (combined gender) August 2016
  SBP_test = lm(SBP.6 ~ SBP.0 + TRTGP, data = text.me.data)
  pval.frame1 = data.frame(type='SBP', pvalue=summary(SBP_test)$coefficients[3,4])

  ### LDL treatment effect
  LDL_CHOL = text.me.data %>% group_by(GENDER) %>%
    do(LDL_CHOL_test = lm(LDL_CHOL.6 ~ LDL_CHOL.0 + TRTGP, data = .))
  LDL_CHOL = tidy(LDL_CHOL, LDL_CHOL_test)
  LDL_CHOL = subset(LDL_CHOL, term=="TRTGP", select=c("GENDER", "estimate", "std.error"))
  # if too small ignore gender (August)
  if(nrow(LDL_CHOL)<2){
    LDL_CHOL = text.me.data %>% do(LDL_CHOL_test = lm(LDL_CHOL.6 ~ LDL_CHOL.0 + TRTGP, data = .))
    LDL_CHOL = tidy(LDL_CHOL, LDL_CHOL_test)
    LDL_CHOL = subset(LDL_CHOL, term=="TRTGP", select=c("estimate", "std.error"))
    S1 = S2 = LDL_CHOL;
    S1$GENDER = 0;
    S2$GENDER = 1;
    LDL_CHOL = rbind(S1, S2)
  }
  # p-value (combined gender) August 2016
  LDL_test = lm(LDL_CHOL.6 ~ LDL_CHOL.0  + TRTGP, data = text.me.data)
  pval.frame2 = data.frame(type='LDL Cholesterol', pvalue=summary(LDL_test)$coefficients[3,4])
  
  # smoker
  text.me.data.men = subset(text.me.data, GENDER==0)
  TM.men = text.me.data.men$TRTGP == 1 
  Expected = mean(text.me.data.men[TM.men,]$F_CURRENTSMOKER.6)-mean(text.me.data.men[!TM.men,]$F_CURRENTSMOKER.6)
  smoke.6 = table(text.me.data.men$TRTGP, text.me.data.men$F_CURRENTSMOKER.6)
  if(nrow(smoke.6)<2|ncol(smoke.6)<2|any(smoke.6==0)){ # add one to each cell for small data
    for.smoker.fill = data.frame(TRTGP=c(0,1,0,1), F_CURRENTSMOKER.6=c(0,0,1,1))
    text.me.data.men = subset(text.me.data.men, select=c('TRTGP','F_CURRENTSMOKER.6'))
    for.smoker = rbind(text.me.data.men, for.smoker.fill)
    smoke.6 = table(for.smoker$TRTGP, for.smoker$F_CURRENTSMOKER.6)
  }
  SM = prop.test(smoke.6, correct=FALSE)
  conf_sm_men = SM$conf.int
  Low = conf_sm_men[1]
  High = conf_sm_men[2] 
  Std.error = (High-Low)/3.92
  SM_men = as.data.frame(cbind(Expected,Std.error))
  rm(SM,conf_sm_men,Low, High, Expected, Std.error)
  rm(TM.men)
  # pvalue (added August 2016)
  smoke.test = chisq.test(table(text.me.data$F_CURRENTSMOKER.6, text.me.data$TRTGP))
  pval.frame3 = data.frame(type='Smoking', pvalue=smoke.test$p.value)
  
  text.me.data.women = subset(text.me.data, GENDER==1)
  TM.women = text.me.data.women$TRTGP == 1 
  Expected = mean(text.me.data.women[TM.women,]$F_CURRENTSMOKER.6)-mean(text.me.data.women[!TM.women,]$F_CURRENTSMOKER.6)
  smoke.6 = table(text.me.data.women$TRTGP, text.me.data.women$F_CURRENTSMOKER.6)
  if(nrow(smoke.6)<2|ncol(smoke.6)<2|any(smoke.6==0)){ # add one to each cell
    for.smoker.fill = data.frame(TRTGP=c(0,1,0,1), F_CURRENTSMOKER.6=c(0,0,1,1))
    text.me.data.women = subset(text.me.data.women, select=c('TRTGP','F_CURRENTSMOKER.6'))
    for.smoker = rbind(text.me.data.women, for.smoker.fill)
    smoke.6 = table(for.smoker$TRTGP, for.smoker$F_CURRENTSMOKER.6)
  }
  SM = prop.test(smoke.6, correct=FALSE)
  conf_sm_women = SM$conf.int
  Low = conf_sm_women[1]
  High = conf_sm_women[2] 
  Std.error = (High-Low)/3.92
  SM_women = as.data.frame(cbind(Expected, Std.error))
  rm(SM,conf_sm_women,Low, High, Expected, Std.error)
  rm(TM.women)
  
  SM_men$GENDER = 0
  SM_women$GENDER = 1
  smoking = rbind(SM_men,SM_women)
  smoking = subset(smoking,select=c("GENDER", "Expected","Std.error"))
  rm(SM_men,SM_women)
  
  # added August 2016
  pvalues = rbind(pval.frame1, pval.frame2, pval.frame3)
  # return
  to.return = list()
  to.return$pvalues = pvalues
  to.return$text.me.data = text.me.data
  to.return$mbs.pbs.data = mbs.pbs.data
  to.return$age = age
  to.return$SBP = SBP
  to.return$LDL_CHOL = LDL_CHOL
  to.return$smoking = smoking
  to.return$Proportions = Proportions
  to.return$Q.CVD.alpha = Q.CVD.alpha # parameters to sample from
  to.return$Q.CVD.beta = Q.CVD.beta
  return(to.return)
}
