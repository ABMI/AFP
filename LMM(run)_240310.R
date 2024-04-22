library(dplyr)
library(lme4)
library(emmeans)
library(ggeffects)
library(ggplot2)
measure_LMM <- readRDS("~/measure_LMM_240310.RDS")

# (Target 안에서 : treatment == 1) outcome 0 vs 1
byOutcome = "~/ATLAS/AFP/data/byOutcome"

Pop = measure_LMM %>% filter(treatment == 1)
LLMbyOutcome(folder_path = byOutcome, measure_Pop = Pop , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3009306, islog = 1)
LLMbyOutcome(folder_path = byOutcome, measure_Pop = Pop , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3013721, islog = 0)
LLMbyOutcome(folder_path = byOutcome, measure_Pop = Pop , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3006923, islog = 0)
LLMbyOutcome(folder_path = byOutcome, measure_Pop = Pop , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3024561, islog = 0)
LLMbyOutcome(folder_path = byOutcome, measure_Pop = Pop , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3024128, islog = 0)
LLMbyOutcome(folder_path = byOutcome, measure_Pop = Pop , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3007461, islog = 0)

# (미발병군 안에서 outcome == 0) outcome 0 vs 1
byTreat = "~/ATLAS/AFP/data/byTreat"
Pop_outcomezero = measure_LMM %>% filter(outcome == 0)

LLMbyTreat(folder_path = byTreat, measure_Pop = Pop_outcomezero , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3009306, islog = 1)
LLMbyTreat(folder_path = byTreat, measure_Pop = Pop_outcomezero , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3013721, islog = 0)
LLMbyTreat(folder_path = byTreat, measure_Pop = Pop_outcomezero , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3006923, islog = 0)
LLMbyTreat(folder_path = byTreat, measure_Pop = Pop_outcomezero , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3024561, islog = 0)
LLMbyTreat(folder_path = byTreat, measure_Pop = Pop_outcomezero , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3024128, islog = 0)
LLMbyTreat(folder_path = byTreat, measure_Pop = Pop_outcomezero , censoring = 1, start_offset = -90, end_offset = +730, concept_id = 3007461, islog = 0)
