LLMbyOutcome <- function(folder_path, measure_Pop, censoring, start_offset, end_offset, concept_id, islog) {
  file_name <- paste0(folder_path, concept_id, "dur", start_offset, "to", end_offset, "cs", censoring, "log", islog)
  
  if (censoring == 1) {
    measure_Pop <- measure_Pop %>% filter(measurement_date <= cohortStartDate + survivalTime)
  } else {
    measure_Pop <- measure_Pop
  }
  
  measure_llm <- measure_Pop %>%
    filter(measurement_concept_id == concept_id) %>% 
    filter(measurement_date >= cohortStartDate+start_offset) %>% filter(measurement_date <= cohortStartDate+end_offset) %>%
    filter(value_as_number > 0) %>%
    mutate(followup_months = floor((followup_days-1)/30)) %>%
    mutate(logvalue = log(value_as_number, base = 10)) 
  
  if (islog == 1){
    lmm_model <- lmer(logvalue ~ followup_months * outcome + age + gender + HBVdrug + cirrhosis + (1 | person_id), data = measure_llm)
  } else {
    lmm_model <- lmer(value_as_number ~ followup_months * outcome + age + gender + HBVdrug + cirrhosis + (1 | person_id), data = measure_llm)
  }

  summary <- summary(lmm_model)
  #saveRDS(summary, file = paste0(file_name,".rds"))
  
  #terms_str <- paste0("followup_weeks [", floor((start_offset-1)/7), ":", floor((end_offset-1)/7), ", by=4]")

  #ggpredict 함수에 적용
  preds <- ggpredict(
    lmm_model, 
    #terms = c(terms_str, "treatment")
    terms = c("followup_months [-3:24, by = 9]", "outcome")
  )

  x_breaks <- seq(-3, 24, length.out = 4)  # x축을 위한 5개의 균등한 점
  #y_min <- min(preds$conf.low)
  #y_max <- max(preds$conf.high)
  #y_breaks <- seq(floor(y_min/10), floor(y_max+10/10), length.out = 4)  # y축을 위한 5개의 균등한 점
  
  p <- ggplot(preds, aes(x = x, y = predicted, group = group, linetype = group, color = group)) +
    theme_minimal() +
    theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "grey50")) +
    labs(title = file_name, x = "Follow-up Months", color = "Outcome", linetype = "Outcome", fill = "Outcome") +  # fill 레이블 추가
    scale_x_continuous(limits = c(-5, 25), breaks = x_breaks) +
    scale_y_continuous(limits = c(min(preds$conf.low)-0.2, max(preds$conf.high)+0.05)) +
    geom_line() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5, position = position_dodge(width = 0.5), linetype = 1) +
    geom_point(aes(fill = group), shape = 21, position = position_dodge(width = 0.5), size = 2, stroke = .5) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_fill_manual(values = c("black", "white")) +
    scale_color_manual(values=c("black", "black"))
  
  #ggsave(file = paste0(file_name,".png"), plot = p, width = 10, height = 8, dpi = 300)
  a <- sjPlot::tab_model(lmm_model)
  print(summary)
  print(a)
}

# ######################
# folder_path = byTreat
# measure_Pop = Pop
# censoring = 1
# start_offset = -90
# end_offset = +730
# concept_id = 3013721
# islog = 0
# 
# ######################
LLMbyTreat <- function(folder_path, measure_Pop, censoring, start_offset, end_offset, concept_id, islog) {
  file_name <- paste0(folder_path, concept_id, "dur", start_offset, "to", end_offset, "cs", censoring, "log", islog)
  
  if (censoring == 1) {
    measure_Pop <- measure_Pop %>% filter(measurement_date <= cohortStartDate + survivalTime)
  } else {
    measure_Pop <- measure_Pop
  }
  
  measure_llm <- measure_Pop %>%
    filter(measurement_concept_id == concept_id) %>% 
    filter(measurement_date >= cohortStartDate+start_offset) %>% filter(measurement_date <= cohortStartDate+end_offset) %>%
    filter(value_as_number > 0) %>%
    mutate(followup_months = floor((followup_days-1)/30)) %>%
    mutate(logvalue = log(value_as_number, base = 10)) 
  
  if (islog == 1){
    lmm_model <- lmer(logvalue ~ followup_months * treatment + age + gender + HBVdrug + cirrhosis + (1 | person_id), data = measure_llm)
  } else {
    lmm_model <- lmer(value_as_number ~ followup_months * treatment + age + gender + HBVdrug + cirrhosis + (1 | person_id), data = measure_llm)
  }
  
  summary <- summary(lmm_model)
  #saveRDS(summary, file = paste0(file_name,".rds"))
  
  #terms_str <- paste0("followup_weeks [", floor((start_offset-1)/7), ":", floor((end_offset-1)/7), ", by=4]")
  
  #################################
  predicted <- emmeans(lmm_model, specs = ~ treatment, at = list(followup_months = 24), pbkrtest.limit = 100000, lmerTest.limit = 100000)
  conf_int <- confint(predicted)

  print(conf_int)
  print(pairs(predicted))
  #################################
  
  # ggpredict 함수에 적용
  preds <- ggpredict(
    lmm_model, 
    #terms = c(terms_str, "treatment")
    terms = c("followup_months [-3:24, by = 9]", "treatment")
  )
  
  x_breaks <- seq(-3, 24, length.out = 4)  # x축을 위한 5개의 균등한 점
  #y_min <- min(preds$conf.low)
  #y_max <- max(preds$conf.high)
  #y_breaks <- seq(floor(y_min/10), floor(y_max+10/10), length.out = 4)  # y축을 위한 5개의 균등한 점
  
  p <- ggplot(preds, aes(x = x, y = predicted, group = group, linetype = group, color = group)) +
    theme_minimal() +
    theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "grey50")) +
    labs(title = file_name, x = "Follow-up Months", color = "Treatment", linetype = "Treatment", fill = "Treatment") +  # fill 레이블 추가
    scale_x_continuous(limits = c(-5, 25), breaks = x_breaks) +
    scale_y_continuous(limits = c(min(preds$conf.low), max(preds$conf.high))) +
    geom_line() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5, position = position_dodge(width = 0.5), linetype = 1) +
    geom_point(aes(fill = group), shape = 21, position = position_dodge(width = 0.5), size = 2, stroke = .5) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_fill_manual(values = c("white", "black")) +
    scale_color_manual(values=c("black", "black")) +
    geom_vline(xintercept = 0, linetype = "solid", color = "gray", size = 0.5)
  
  #ggsave(file = paste0(file_name,".png"), plot = p, width = 10, height = 8, dpi = 300)
  a <- sjPlot::tab_model(lmm_model)
  print(summary)
  print(a)
}
