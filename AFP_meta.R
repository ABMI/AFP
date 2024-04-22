############################### all about meta ###############################

#target_id	comparator_id	outcome_id	analysis_id	logRr	logLb95Ci	logUb95Ci	seLogRr	database_id	target_subjects	comparator_subjects	target_days	comparator_days	target_outcomes	comparator_outcomes
library(dplyr)
library(meta)

result <- readRDS("~/meta_240310.RDS")
result <- combined_df %>% select(target_id, comparator_id, outcome_id, analysis_id, log_rr, ci_95_lb, ci_95_ub, se_log_rr, database_id, target_subjects, comparator_subjects, target_days, comparator_days, target_outcomes, comparator_outcomes)

# change 2 values in GUNH results

result[7, "target_outcomes"] <- 4
result[12, "comparator_outcomes"] <- 2
result[19, "target_outcomes"] <- 2
result[23, "target_outcomes"] <- 3
result[23, "comparator_outcomes"] <- 3

# 1279, 1280 : main
# 1281, 1282 : a
# 1283, 1280 : b
# 1316, 1317 : c

targetId = 1316
comparatorId = 1317
outcomeId = 852
analysisId = 3
targetName = "       High-AFP"
comparatorName = "           Normal-AFP"
outcomeName = "HCC"
calibration=F


#####
result <- result  %>% filter(result$target_id == targetId,
                             result$comparator_id == comparatorId,
                             result$outcome_id == outcomeId,
                             result$analysis_id == analysisId)

order <- c("AUMC", "DCMC", "DONGSAN", "MJH", "KDH", "SCHBC", "SCHCA")
result <- result %>% filter(database_id %in% order) %>% arrange(match(database_id, order))
databaseIds <- result$database_id

logRr = result$log_rr
logLb95Ci = log( result$ci_95_lb )
logUb95Ci = log( result$ci_95_ub )
#seLogRr = (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
seLogRr = result$se_log_rr

meta <- meta::metagen(TE = logRr, 
                      seTE = seLogRr, 
                      studlab = databaseIds, 
                      sm = "HR", 
                      hakn = FALSE,
                      comb.fixed = T,
                      comb.random = F)

meta$n.e <- result$target_subjects
meta$event.e <- result$target_outcomes
meta$event.rate.t <- round(with(result, target_outcomes/(target_days/365))*1000,1)
meta$person.year.t <- with(result, round((target_days/365),0))

meta$n.c <- result$comparator_subjects
meta$event.c <- result$comparator_outcomes
meta$event.rate.c <- round(with(result, comparator_outcomes/(comparator_days/365))*1000,1)
meta$person.year.c<-with(result, round((comparator_days/365),0))

#####

xLim= ceiling(max (1/exp(meta$lower.random),exp(meta$upper.random)))

leftCols = c("studlab",
             "n.e","event.e" ,"event.rate.t", 
             "n.c","event.c","event.rate.c",
             "effect","ci"#,"HR"
)
leftLabs= c("Source","Total","Event", " Incidence rate","Total","Event", " Incidence rate", "HR", "95% CI")

space <- " "

#####
meta::forest.meta(meta,
                  studlab=TRUE,
                  # overall=TRUE,
                  # pooled.totals=meta$comb.random, 
                  pooled.events=TRUE,
                  leftcols = leftCols,
                  rightcols=F,
                  #col.study="black",
                  #col.square="gray",
                  #col.inside="white",
                  #col.diamond="gray",
                  #col.diamond.lines="black",
                  leftlabs = leftLabs,
                  lab.e = paste0(space,targetName),
                  lab.c = paste0(space,comparatorName),
                  lab.e.attach.to.col = c("n.e"),
                  lab.c.attach.to.col = c("n.c"),
                  # leftlabs = c("Event rate", "Event rate"#, "HR (95% CI)"
                  #              ),
                  # rightcols = c("w.random"),
                  fontsize=12,
                  comb.fixed = T,
                  comb.random = F,
                  text.fixed = "Overall",
                  text.random = "Overall",
                  col.diamond.fixed = "royal blue",
                  col.diamond.lines = "black",
                  #xlab = "Hazard Ratio (95% CI)",
                  
                  digits = 2,
                  digits.pval =3,
                  digits.I2 = 1,
                  just.studlab="left",
                  #just.addcols ="right",
                  just.addcols.left= "right",
                  #just.addcols.right= "right",
                  just = "center",
                  # xlim = c(round(1/xLim,2),xLim),
                  xlim = c(0.5, 15),
                  plotwidth ="8cm",
                  #layout="JAMA",
                  spacing =1,
                  addrow.overall=TRUE,
                  print.I2 = TRUE,
                  # overall.hetstat = T,
                  # hetstat= F,
                  # print.I2=F,
                  print.pval.I2=F,
                  print.tau2 = F,
                  # print.Q	=F,
                  print.pval.Q = F,
                  # zero.pval = F,
                  # print.Rb  = F,
                  #smlab = "",
                  #sortvar=TE,
                  #label.lef = sprintf("Favors\n%s",targetName),#capitalize(targetName)),
                  #label.right = sprintf("Favors\n%s",comparatorName),#capitalize(comparatorName)),
                  scientific.pval = F,#meta::gs("scientific.pval"), 
                  big.mark =","#meta::gs("big.mark),
                  
)

round(sum(result$target_outcomes)/(sum(result$target_days)/365)*1000,1)
round(sum(result$comparator_outcomes)/(sum(result$comparator_days)/365)*1000,1)
meta$pval.Q
