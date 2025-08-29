

#浏览数据，估计内存
d0 = read.table(file = "designs.txt",
                      header = TRUE,
                      sep = "|",
                      na.strings = "",
                      stringsAsFactors = T,
                      comment.char = "",
                      quote = "\"",
                      fill = FALSE,
                      nrows = 200000)

sz <- object.size(d0)   #  determine approximate memory used to store d0
#  multiply by (nrows for full data set / 1000) 
#  to estimate total memory needed to store the full data
print( sz * 500000 / 200000, units="Mb")
#  Estimate that ~50 Mb of memory will be needed. 
#  This is fine since my system has > 4 Gb of RAM.
rm(sz,d0)

# 获取干预类型数据
designs = read.table(file = "designs.txt",
                      header = TRUE,
                      sep = "|",
                      na.strings = "",
                      stringsAsFactors = T,
                      comment.char = "",
                      quote = "\"",
                      fill = FALSE,
                      nrows = 500000)

#选取primary_purpose为treatment的临床试验
designs_treatment=designs[which(designs$primary_purpose=="Treatment"),]
saveRDS(designs_treatment,"designs_treatment.rds")
rm(designs)

#在studies里面筛选pp为treatment的interventional研究
studies = read.table(file = "studies.txt",
                     header = TRUE,
                     sep = "|",
                     na.strings = "",
                     stringsAsFactors = T,
                     comment.char = "",
                     quote = "\"",
                     fill = FALSE,
                     nrows = 1500000)

data_treatment = merge(studies, designs_treatment, by = "nct_id")
rm(studies,designs_treatment)

#筛选干预为Radiation相关的临床研究

interventions = read.table(file = "interventions.txt",
                     header = TRUE,
                     sep = "|",
                     na.strings = "",
                     stringsAsFactors = T,
                     comment.char = "",
                     quote = "\"",
                     fill = FALSE,
                     nrows = 1500000)
## 筛选intervention_type为radiation的项目
interventions_type_radiation = interventions[which(interventions$intervention_type=="Radiation"),]
## 筛选decription里面包含“radiotherapy","radiation"的项目
interventions_description = interventions[grepl("radiotherapy|radiation", interventions$description, ignore.case = T), ]
interventions_description = interventions_description[which(interventions_description$intervention_type != "Radiation"),]

interventions_description_others = interventions[grepl("carbon|proton|photon", interventions$description, ignore.case = T),]
interventions_description_others = interventions_description_others[which(interventions_description_others$intervention_type != "Radiation"),]

interventions_description = rbind(interventions_description,interventions_description_others)
## 合并两者并去掉重复行
interventions_description = interventions_description[!duplicated(interventions_description),]

rm(interventions)

interventions = rbind(interventions_type_radiation, interventions_description)
rm(interventions_description,interventions_description_others,interventions_type_radiation)


#合并intervention初筛和treatment
data_radiotherapy_v1 = merge(data_treatment, interventions, by = "nct_id")
rm(data_treatment,interventions)


# 数据清洗，根据description删掉明显不符的临床项目
non1 = data_radiotherapy_v1[grepl("proton pump", data_radiotherapy_v1$description, ignore.case = T),]
non2 = data_radiotherapy_v1[grepl("carbonate", data_radiotherapy_v1$description, ignore.case = T),]
non3 = data_radiotherapy_v1[grepl("carbon dioxide", data_radiotherapy_v1$description, ignore.case = T),]
non4 = data_radiotherapy_v1[grepl("proton density", data_radiotherapy_v1$description, ignore.case = T),]

nonA = rbind(non1, non2, non3, non4)
rm(non1, non2, non3, non4)

data_radiotherapy_v2_2 = dplyr::anti_join(data_radiotherapy_v1, nonA, by = "nct_id")
rm(data_radiotherapy_v1, nonA)
saveRDS(data_radiotherapy_v2_2, file = "data_radiotherapy_v2_2.rds")

library(dplyr)

data_carbon_2 = data_radiotherapy_v2 %>%
  filter(if_any(everything(), ~grepl("carbon|Carbon",., ignore.case = T)))
saveRDS(data_carbon_2, file = "data_carbon.rds")


data_proton_2 = data_radiotherapy_v2 %>%
  filter(if_any(everything(), ~grepl("proton|Proton",., ignore.case = T)))
saveRDS(data_proton_2, file = "data_proton.rds")


data_carbon_forfilter = data_carbon_2[, c("start_date","nct_id","brief_title", "official_title", "intervention_type","name", "description")]
data_proton_forfilter = data_proton_2[, c("start_date","nct_id","brief_title", "official_title", "intervention_type","name", "description")]
library(openxlsx)
write.xlsx(data_carbon_forfilter, file = "data_carbon_forfilter.xlsx")
write.xlsx(data_proton_forfilter, file = "data_proton_forfilter.xlsx")

data_photon = anti_join(data_radiotherapy_v2_2,data_carbon_2) %>% anti_join(data_proton_2)
saveRDS(data_photon, file = "data_photon.rds")
data_photon_forfilter = data_photon[,c("start_date","nct_id","brief_title", "official_title", "intervention_type","name", "description")]
library(openxlsx)
write.xlsx(data_photon_forfilter, file = "data_photon_forfilter.xlsx")


##################################### PROTON 数据合并 ############################################
### for proton
library(dplyr)
nct_id_proton = read.table("nct_id_proton.txt", header = T)
nct_id_proton = subset(nct_id_proton, !duplicated(nct_id_proton))
nct_id_proton = nct_id_proton$nct_id

studies = read.table(file = "studies.txt",
                     header = TRUE,
                     sep = "|",
                     na.strings = "",
                     stringsAsFactors = T,
                     comment.char = "",
                     quote = "\"",
                     fill = FALSE,
                     nrows = 1500000)

studies_proton = studies %>% filter(nct_id %in% nct_id_proton)
#data_proton = merge(nct_id_proton, studies, by = "nct_id")
rm(studies)


designs = read.table(file = "designs.txt",
                     header = TRUE,
                     sep = "|",
                     na.strings = "",
                     stringsAsFactors = T,
                     comment.char = "",
                     quote = "\"",
                     fill = FALSE,
                     nrows = 500000)
designs_proton = designs %>% filter(nct_id %in% nct_id_proton)
#data_proton = merge(data_proton, designs, by = "nct_id")
rm(designs)


conditions = read.table(file = "conditions.txt",
                        header = TRUE,
                        sep = "|",
                        na.strings = "",
                        stringsAsFactors = T,
                        comment.char = "",
                        quote = "\"",
                        fill = FALSE,
                        nrows = 1500000)

conditions_proton = conditions %>% filter(nct_id %in% nct_id_proton)
rm(conditions)

interventions = read.table(file = "interventions.txt",
                           header = TRUE,
                           sep = "|",
                           na.strings = "",
                           stringsAsFactors = T,
                           comment.char = "",
                           quote = "\"",
                           fill = FALSE,
                           nrows = 1500000)
interventions_proton = interventions %>% filter(nct_id %in% nct_id_proton)
rm(interventions)


eligibilities = read.table(file = "eligibilities.txt",
                           header = T,
                           sep = "|",
                           na.strings = "",
                           stringsAsFactors = T,
                           comment.char = "",
                           quote = "\"",
                           fill = F,
                           nrows = 1500000)
eligibilities_proton = eligibilities %>% filter(nct_id %in% nct_id_proton)
rm(eligibilities)


countries = read.table(file = "countries.txt",
                           header = T,
                           sep = "|",
                           na.strings = "",
                           stringsAsFactors = T,
                           comment.char = "",
                           quote = "\"",
                           fill = F,
                           nrows = 1500000)
countries_proton = countries %>% filter(nct_id %in% nct_id_proton)
rm(countries)


design_groups = read.table(file = "design_groups.txt",
                          header = T,
                          sep = "|",
                          na.strings = "",
                          stringsAsFactors = T,
                          comment.char = "",
                          quote = "\"",
                          fill = F,
                          nrows = 1500000)
design_groups_proton = design_groups %>% filter(nct_id %in% nct_id_proton)
rm(design_groups)


facilities = read.table(file = "facilities.txt",
                           header = T,
                           sep = "|",
                           na.strings = "",
                           stringsAsFactors = T,
                           comment.char = "",
                           quote = "\"",
                           fill = F,
                           nrows = 15000000)
facilities_proton = facilities %>% filter(nct_id %in% nct_id_proton)
rm(facilities)


facility_investigators = read.table(file = "facility_investigators.txt",
                                    header = T,
                                    sep = "|",
                                    na.strings = "",
                                    stringsAsFactors = T,
                                    comment.char = "",
                                    quote = "\"",
                                    fill = F,
                                    nrows = 15000000)
facility_investigators_proton = facility_investigators %>% filter(nct_id %in% nct_id_proton)
rm(facility_investigators)

responsible_parties = read.table(file = "responsible_parties.txt",
                                 header = T,
                                 sep = "|",
                                 na.strings = "",
                                 stringsAsFactors = T,
                                 comment.char = "",
                                 quote = "\"",
                                 fill = F,
                                 nrows = 15000000)
responsible_parties_proton = responsible_parties %>% filter(nct_id %in% nct_id_proton)
rm(responsible_parties)

sponsors = read.table(file = "sponsors.txt",
                      header = T,
                      sep = "|",
                      na.strings = "",
                      stringsAsFactors = T,
                      comment.char = "",
                      quote = "\"",
                      fill = F,
                      nrows = 15000000)
sponsors_proton = sponsors %>% filter(nct_id %in% nct_id_proton)
rm(sponsors)

study_references = read.table(file = "study_references.txt",
                             header = T,
                             sep = "|",
                             na.strings = "",
                             stringsAsFactors = T,
                             comment.char = "",
                             quote = "\"",
                             fill = F,
                             nrows = 15000000)
study_references_proton = study_references %>% filter(nct_id %in% nct_id_proton)
rm(study_references)

drop_withdrawals = read.table(file = "drop_withdrawals.txt",
                              header = T,
                              sep = "|",
                              na.strings = "",
                              stringsAsFactors = T,
                              comment.char = "",
                              quote = "\"",
                              fill = F,
                              nrows = 15000000)
drop_withdrawals_proton = drop_withdrawals %>% filter(nct_id %in% nct_id_proton)
rm(drop_withdrawals)


reported_events = read.table(file = "reported_events.txt",
                              header = T,
                              sep = "|",
                              na.strings = "",
                              stringsAsFactors = T,
                              comment.char = "",
                              quote = "\"",
                              fill = F,
                              nrows = 15000000)
reported_events_proton = reported_events %>% filter(nct_id %in% nct_id_proton)
rm(reported_events)
gc()

outcomes = read.table(file = "outcomes.txt",
                             header = T,
                             sep = "|",
                             na.strings = "",
                             stringsAsFactors = T,
                             comment.char = "",
                             quote = "\"",
                             fill = F,
                             nrows = 15000000)
outcomes_proton = outcomes %>% filter(nct_id %in% nct_id_proton)
rm(outcomes)
gc()


## 批量保存以上获得的数据框
data_frame_list <- Filter(is.data.frame, mget(ls()))
dir.create("Extraction_proton_v1")
save_path = "Extraction_proton_v1"

for (i in seq_along(data_frame_list)) {
  filename = paste0(names(data_frame_list)[i], ".rds")
  file_path = file.path(save_path,filename)
  saveRDS(data_frame_list[[i]], file = file_path)
}

library(openxlsx)
for (i in seq_along(data_frame_list)) {
  filename <- paste0(names(data_frame_list)[i], ".xlsx")
  file_path = file.path(save_path,filename)
  write.xlsx(data_frame_list[[i]], file = file_path)
}



##############################################################################
#
############################数据清洗##
#
##############################################################################

############################# For conditions ################################
### 找出重复项目
#duplicated_conditions_proton = conditions_proton[duplicated(conditions_proton$nct_id),]
#done_conditions_proton = conditions_proton[!duplicated(conditions_proton$nct_id),]
#uni_dup_conditions_proton = duplicated_conditions_proton[!duplicated(duplicated_conditions_proton$nct_id),]
conditions_proton = readRDS("Extraction_proton_v1/conditions_proton.rds")
done_conditions_proton = conditions_proton
done_conditions_proton$cancer_type = NA
## CNS, Head_and_Neck, gastrointestinal, Lung, Breast, Sarcoma, Prostate, Gynecology


done_conditions_proton$cancer_type = ifelse(grepl("nerve|pituitary|intracranial|acoustic|schwannoma|pineoblastoma|medulloblastoma|pineocytoma|glioma|brain|ependymoma|astrocytoma|meningioma|craniopharyngioma|cns cancer|central nervous system|glioblastoma", done_conditions_proton$name, ignore.case = T),
                                            "CNS", done_conditions_proton$cancer_type)

done_conditions_proton$cancer_type = ifelse(grepl("schneiderian|tongue|head and neck|oropharyngeal|nasal|head-and-neck|hnscc|head cancer|neck cancer|tonsil|nasopharyngeal|pharyngeal|laryngeal|oral cavity", done_conditions_proton$name, ignore.case = T),
                                            "Head_and_Neck", done_conditions_proton$cancer_type)

done_conditions_proton$cancer_type = ifelse(grepl("cholangiocarcinoma|anus|colorectal|esophageal|pancreatic|rectal|anal|esophagus|gi cancer|gastrointestinal|liver|hepatocellular", done_conditions_proton$name, ignore.case = T),
                                            "Digestive_System", done_conditions_proton$cancer_type)

done_conditions_proton$cancer_type = ifelse(grepl("lung", done_conditions_proton$name, ignore.case = T),
                                            "Lung", done_conditions_proton$cancer_type)

done_conditions_proton$cancer_type = ifelse(grepl("breast", done_conditions_proton$name, ignore.case = T),
                                            "Breast", done_conditions_proton$cancer_type)

done_conditions_proton$cancer_type = ifelse(grepl("sarcoma|chordoma", done_conditions_proton$name, ignore.case = T), 
                                            "sarcoma", done_conditions_proton$cancer_type)

done_conditions_proton$cancer_type = ifelse(grepl("prostate|prostatic", done_conditions_proton$name, ignore.case = T), 
                                            "Prostate", done_conditions_proton$cancer_type)
 
done_conditions_proton$cancer_type = ifelse(grepl("cervical|endometrial|uterine|gynecologic", done_conditions_proton$name, ignore.case = T), 
                                            "Gynecology", done_conditions_proton$cancer_type)

done_conditions_proton$cancer_type = ifelse(is.na(done_conditions_proton$cancer_type), "Others", done_conditions_proton$cancer_type)
library(openxlsx)
write.xlsx(done_conditions_proton, file = file.path("Extraction_proton_v1","done_conditions_proton.xlsx"))
saveRDS(done_conditions_proton, file = "done_conditions_proton.rds")
rm(done_conditions_proton)
### 余下手动555555.筛选完成，下一步


######################### For facilities    ###################################
facility_investigators_proton = readRDS(file = "Extraction_proton_v1/facility_investigators_proton.rds")
done_facilities = facility_investigators_proton[which(facility_investigators_proton$role == "Principal Investigator"),]



######################## For Studies ##########################################
### Studies 数据提取项目 包括：
#### nct_id, start_date, completion_date, overall_status, phase, enrollment. source,
#### number_of_arms, why_stopped, source_class, has_dmc, plan_to_share_ipd
studies_proton = readRDS(file = "Extraction_proton_v1/studies_proton.rds")
data2_studies_proton = studies_proton[,c("nct_id", "study_first_submitted_date","start_date","completion_date",
                                         "overall_status","phase","enrollment",
                                         "source","source_class","number_of_arms",
                                         "why_stopped","has_dmc","plan_to_share_ipd")]

#install.packages("lubridate")
library(dplyr)
library(lubridate)
library(tibble)

data2_studies_proton = data2_studies_proton %>%
  mutate(submitted_year = year(as.Date(data2_studies_proton$study_first_submitted_date))) %>%
  select(1:study_first_submitted_date, submitted_year, everything())

data2_studies_proton = data2_studies_proton %>%
  mutate(start_year = year(as.Date(data2_studies_proton$start_date))) %>%
  select(1:start_date, start_year, everything())

#data2_studies_proton = data2_studies_proton %>%
#  mutate(completion_year = year(as.Date(data2_studies_proton$completion_date))) %>%
#  select(1:completion_date, completion_year, everything())

data2_studies_proton$start_date = as.Date(data2_studies_proton$start_date)
data2_studies_proton$study_first_submitted_date = as.Date(data2_studies_proton$study_first_submitted_date)

data2_studies_proton = data2_studies_proton %>%
  filter(study_first_submitted_date > as.Date("2007-09-27") & study_first_submitted_date <= as.Date("2023-06-03"))

### 注意注意，submitted_year才是注册时间！

data2_studies_proton = data2_studies_proton %>%
  mutate(year_segm = NA) %>%
  select(1:submitted_year, year_segm, everything())

data2_studies_proton$year_segm = case_when(
  data2_studies_proton$study_first_submitted_date > as.Date("2007-09-27") & 
    data2_studies_proton$study_first_submitted_date <= as.Date("2012-09-27") ~ "2007-2012",
  data2_studies_proton$study_first_submitted_date > as.Date("2012-09-27") &
    data2_studies_proton$study_first_submitted_date <= as.Date("2017-09-27") ~ "2012-2017",
  data2_studies_proton$study_first_submitted_date > as.Date("2017-09-27") ~ "2017-2023",
  TRUE ~ NA_character_   #不满足任何条件则为缺失值
)

data2_studies_proton$year_segm = as.factor(data2_studies_proton$year_segm)

data2_studies_proton = data2_studies_proton %>%
  mutate(Registration_before_participant_enrollment = NA) %>%
  select(1:submitted_year, Registration_before_participant_enrollment, everything())

data2_studies_proton$Registration_before_participant_enrollment = case_when(
  as.Date(data2_studies_proton$study_first_submitted_date) < as.Date(data2_studies_proton$start_date) ~ "t",
  as.Date(data2_studies_proton$study_first_submitted_date) >= as.Date(data2_studies_proton$start_date) ~ "f",
  TRUE ~ NA_character_
)

data2_studies_proton = data2_studies_proton %>%
  mutate(phase_done = phase) %>%
  select(1:phase, phase_done, everything())

data2_studies_proton$phase_done[which(data2_studies_proton$phase == "Early Phase 1")] = "Phase 1"

data2_studies_proton = data2_studies_proton %>%
  mutate(Recruitment_status = overall_status) %>%
  select(1:overall_status, Recruitment_status, everything())

      #ongoing为因子层次外的值，不能直接赋值，需要转换
data2_studies_proton$Recruitment_status = as.character(data2_studies_proton$Recruitment_status)
data2_studies_proton$Recruitment_status[data2_studies_proton$overall_status %in% 
                                          c("Not yet recruiting", "Recruiting",
                                            "Enrolling by invitation", "Active, not recruiting", 
                                            "Suspended")] <- "Ongoing"
data2_studies_proton$Recruitment_status[data2_studies_proton$overall_status %in%
                                          c("Withdrawn", "Terminated")] <- "Stopped Early"

data2_studies_proton = data2_studies_proton %>%
  mutate(enrollment_segm = NA) %>%
  select(1:enrollment, enrollment_segm, everything())
data2_studies_proton$enrollment_segm = case_when(
  data2_studies_proton$enrollment <= 50 ~ "<50",
  data2_studies_proton$enrollment >50 & data2_studies_proton$enrollment <= 100 ~ "50-100",
  data2_studies_proton$enrollment >100 ~ ">100",
  TRUE ~ NA_character_
)

data2_studies_proton = data2_studies_proton %>%
  mutate(arms_segm = NA) %>%
  select(1:number_of_arms, arms_segm,everything())
data2_studies_proton$arms_segm = case_when(
  data2_studies_proton$number_of_arms == 1 ~ "1",
  data2_studies_proton$number_of_arms == 2 ~ "2",
  data2_studies_proton$number_of_arms >= 3 ~ ">3",
  TRUE ~ NA_character_
)

data2_studies_proton$source_class = as.character(data2_studies_proton$source_class)
data2_studies_proton$source_class[data2_studies_proton$source_class == "NIH"] = "GOV"
data2_studies_proton$source_class[data2_studies_proton$source_class == "OTHER_GOV"] = "GOV"
data2_studies_proton$source_class[data2_studies_proton$source_class == "NETWORK"] = "OTHER"

save(data2_studies_proton, file = "data2/data2_studies_proton.rdata")

############################## for designs #####################################
#### nct_id, allocation, intervention_model, masking
designs_proton = readRDS(file = "Extraction_proton_v1/designs_proton.rds")

year_nct = data2_studies_proton$nct_id
data2_designs_proton = designs_proton %>% filter(nct_id %in% year_nct)

data2_designs_proton = data2_designs_proton[,c("nct_id", "allocation", "intervention_model", "masking")]

save(data2_designs_proton, file = "data2/data2_designs_proton.rdata")

############################# for eligibilities ################################
#### nct_id, gender, minimun_age, maximum_age, adult, child, older_adult
eligibilities_proton = readRDS(file = "Extraction_proton_v1/eligibilities_proton.rds")

data2_eligibilities_proton = eligibilities_proton[,c("nct_id", "gender", "minimum_age",
                                                     "maximum_age", "adult", "child", "older_adult")]
data2_eligibilities_proton = data2_eligibilities_proton %>% filter(nct_id %in% year_nct)


data2_eligibilities_proton = data2_eligibilities_proton %>%
  mutate(excludes_children = NA) %>%
  select(1:child, excludes_children, everything())
data2_eligibilities_proton$excludes_children = case_when(
    data2_eligibilities_proton$child == "f" ~ "t",
    data2_eligibilities_proton$child == "t" ~ "f",
    TRUE ~ NA_character_
  )


data2_eligibilities_proton = data2_eligibilities_proton %>%
  mutate(excludes_elderly = NA) %>%
  select(1:older_adult, excludes_elderly, everything())
data2_eligibilities_proton$excludes_elderly = case_when(
  data2_eligibilities_proton$older_adult == "t" ~ "f",
  data2_eligibilities_proton$older_adult == "f" ~ "t",
  TRUE ~ NA_character_
)

save(data2_eligibilities_proton, file = "data2/data2_eligibilities_proton.rdata")


############################ for countries #####################################
data2_countries_proton = readRDS(file = "Extraction_proton_v1/countries_proton.rds")
data2_countries_proton = data2_countries_proton %>% filter(nct_id %in% year_nct)

data2_countries_proton = data2_countries_proton[, c("nct_id", "name")]

data2_countries_proton = data2_countries_proton %>%
  mutate(Region = NA) %>%
  select(1:name, Region, everything())

data2_countries_proton$Region = case_when(
  data2_countries_proton$name %in% c("United States", "Canada") ~ "North America",
  data2_countries_proton$name %in% c("Germany", "Denmark", "Sweden",
                                     "France", "Norway", "Italy", "United Kingdom", 
                                     "Spain", "Netherlands", "Switzerland", "Belgium") ~ "Europe",
  data2_countries_proton$name %in% c("Korea, Republic of", "China", "Taiwan") ~ "Asia",
)

save(data2_countries_proton, file = "data2/data2_countries_proton.rdata")

############################ for condition #####################################
#### 手动整理获得
data2_conditions_proton = read.xlsx("data2_conditions_proton.xlsx")

save(data2_conditions_proton, file = "data2/data2_conditions_proton.rdata")

############################ for facilities #####################################
data2_facilities_proton = readRDS(file = "Extraction_proton_v1/facilities_proton.rds")
data2_facilities_proton = data2_facilities_proton[,c("nct_id")]
data2_facilities_proton = data.frame(nct_id = data2_facilities_proton)
data2_facilities_proton = data2_facilities_proton %>%
  group_by(nct_id) %>%
  summarise(count=n())
colnames(data2_facilities_proton)[colnames(data2_facilities_proton)=="count"] <- "No_of_geographic_regions"

data2_facilities_proton = data2_facilities_proton %>%
  mutate(No_of_geographic_regions_segm = NA) %>%
  select(1:No_of_geographic_regions, No_of_geographic_regions_segm, everything())

data2_facilities_proton$No_of_geographic_regions_segm = case_when(
  data2_facilities_proton$No_of_geographic_regions == 1 ~ "1",
  data2_facilities_proton$No_of_geographic_regions >=2  ~ "2",
  data2_facilities_proton$No_of_geographic_regions >= 3 ~ ">=3"
)

save(data2_facilities_proton, file = "data2/data2_facilities_proton.rdata")
rm(data2_facilities_proton)
############################ for Reference #####################################
data2_study_references_proton = readRDS(file = "Extraction_proton_v1/study_references_proton.rds")
data2_study_references_proton = data2_study_references_proton[,c("nct_id")]
data2_study_references_proton = data.frame(nct_id = data2_study_references_proton)
data2_study_references_proton = data2_study_references_proton %>%
  group_by(nct_id) %>%
  summarise(count=n())

nct_id_proton = read.table("nct_id_proton.txt", header = T)
nct_id_proton = subset(nct_id_proton, !duplicated(nct_id_proton))

data2_study_references_proton = merge(nct_id_proton, data2_study_references_proton, all.x = T)
data2_study_references_proton[is.na(data2_study_references_proton)] = 0

colnames(data2_study_references_proton)[colnames(data2_study_references_proton)=="count"] <- "No_of_References"

data2_study_references_proton = data2_study_references_proton %>%
  mutate(No_of_References_segm = NA) %>%
  select(1:No_of_References, No_of_References_segm, everything())
data2_study_references_proton$No_of_References_segm = case_when(
  data2_study_references_proton$No_of_References == 1 ~ "1",
  data2_study_references_proton$No_of_References == 0 ~ "0",
  data2_study_references_proton$No_of_References >=1 ~ ">=1"
)

save(data2_study_references_proton, file = "data2/data2_study_references_proton.rdata")
rm(data2_study_references_proton)
##################################### CARBON 数据合并 ############################################
### for carbon
library(dplyr)
nct_id_carbon = read.table("nct_id_carbon.txt", header = T)
nct_id_carbon = subset(nct_id_carbon, !duplicated(nct_id_carbon))
nct_id_carbon = nct_id_carbon$nct_id

studies = read.table(file = "studies.txt",
                     header = TRUE,
                     sep = "|",
                     na.strings = "",
                     stringsAsFactors = T,
                     comment.char = "",
                     quote = "\"",
                     fill = FALSE,
                     nrows = 1500000)

studies_carbon = studies %>% filter(nct_id %in% nct_id_carbon)
#data_carbon = merge(nct_id_carbon, studies, by = "nct_id")
rm(studies)


designs = read.table(file = "designs.txt",
                     header = TRUE,
                     sep = "|",
                     na.strings = "",
                     stringsAsFactors = T,
                     comment.char = "",
                     quote = "\"",
                     fill = FALSE,
                     nrows = 500000)
designs_carbon = designs %>% filter(nct_id %in% nct_id_carbon)
#data_carbon = merge(data_carbon, designs, by = "nct_id")
rm(designs)


conditions = read.table(file = "conditions.txt",
                        header = TRUE,
                        sep = "|",
                        na.strings = "",
                        stringsAsFactors = T,
                        comment.char = "",
                        quote = "\"",
                        fill = FALSE,
                        nrows = 1500000)

conditions_carbon = conditions %>% filter(nct_id %in% nct_id_carbon)
rm(conditions)

interventions = read.table(file = "interventions.txt",
                           header = TRUE,
                           sep = "|",
                           na.strings = "",
                           stringsAsFactors = T,
                           comment.char = "",
                           quote = "\"",
                           fill = FALSE,
                           nrows = 1500000)
interventions_carbon = interventions %>% filter(nct_id %in% nct_id_carbon)
rm(interventions)


eligibilities = read.table(file = "eligibilities.txt",
                           header = T,
                           sep = "|",
                           na.strings = "",
                           stringsAsFactors = T,
                           comment.char = "",
                           quote = "\"",
                           fill = F,
                           nrows = 1500000)
eligibilities_carbon = eligibilities %>% filter(nct_id %in% nct_id_carbon)
rm(eligibilities)


countries = read.table(file = "countries.txt",
                       header = T,
                       sep = "|",
                       na.strings = "",
                       stringsAsFactors = T,
                       comment.char = "",
                       quote = "\"",
                       fill = F,
                       nrows = 1500000)
countries_carbon = countries %>% filter(nct_id %in% nct_id_carbon)
rm(countries)


design_groups = read.table(file = "design_groups.txt",
                           header = T,
                           sep = "|",
                           na.strings = "",
                           stringsAsFactors = T,
                           comment.char = "",
                           quote = "\"",
                           fill = F,
                           nrows = 1500000)
design_groups_carbon = design_groups %>% filter(nct_id %in% nct_id_carbon)
rm(design_groups)


facilities = read.table(file = "facilities.txt",
                        header = T,
                        sep = "|",
                        na.strings = "",
                        stringsAsFactors = T,
                        comment.char = "",
                        quote = "\"",
                        fill = F,
                        nrows = 15000000)
facilities_carbon = facilities %>% filter(nct_id %in% nct_id_carbon)
rm(facilities)


facility_investigators = read.table(file = "facility_investigators.txt",
                                    header = T,
                                    sep = "|",
                                    na.strings = "",
                                    stringsAsFactors = T,
                                    comment.char = "",
                                    quote = "\"",
                                    fill = F,
                                    nrows = 15000000)
facility_investigators_carbon = facility_investigators %>% filter(nct_id %in% nct_id_carbon)
rm(facility_investigators)

responsible_parties = read.table(file = "responsible_parties.txt",
                                 header = T,
                                 sep = "|",
                                 na.strings = "",
                                 stringsAsFactors = T,
                                 comment.char = "",
                                 quote = "\"",
                                 fill = F,
                                 nrows = 15000000)
responsible_parties_carbon = responsible_parties %>% filter(nct_id %in% nct_id_carbon)
rm(responsible_parties)

sponsors = read.table(file = "sponsors.txt",
                      header = T,
                      sep = "|",
                      na.strings = "",
                      stringsAsFactors = T,
                      comment.char = "",
                      quote = "\"",
                      fill = F,
                      nrows = 15000000)
sponsors_carbon = sponsors %>% filter(nct_id %in% nct_id_carbon)
rm(sponsors)

study_references = read.table(file = "study_references.txt",
                              header = T,
                              sep = "|",
                              na.strings = "",
                              stringsAsFactors = T,
                              comment.char = "",
                              quote = "\"",
                              fill = F,
                              nrows = 15000000)
study_references_carbon = study_references %>% filter(nct_id %in% nct_id_carbon)
rm(study_references)

drop_withdrawals = read.table(file = "drop_withdrawals.txt",
                              header = T,
                              sep = "|",
                              na.strings = "",
                              stringsAsFactors = T,
                              comment.char = "",
                              quote = "\"",
                              fill = F,
                              nrows = 15000000)
drop_withdrawals_carbon = drop_withdrawals %>% filter(nct_id %in% nct_id_carbon)
rm(drop_withdrawals)
gc()
## 批量保存以上获得的数据框
data_frame_list <- Filter(is.data.frame, mget(ls()))
dir.create("Extraction_carbon_v1")
save_path = "Extraction_carbon_v1"

for (i in seq_along(data_frame_list)) {
  filename = paste0(names(data_frame_list)[i], ".rds")
  file_path = file.path(save_path,filename)
  saveRDS(data_frame_list[[i]], file = file_path)
}

library(openxlsx)
for (i in seq_along(data_frame_list)) {
  filename <- paste0(names(data_frame_list)[i], ".xlsx")
  file_path = file.path(save_path,filename)
  write.xlsx(data_frame_list[[i]], file = file_path)
}



##############################################################################
#
############################数据清洗##
#
##############################################################################


######################## For Studies ##########################################
### Studies 数据提取项目 包括：
#### nct_id, start_date, completion_date, overall_status, phase, enrollment. source,
#### number_of_arms, why_stopped, source_class, has_dmc, plan_to_share_ipd

studies_carbon = readRDS(file = "Extraction_carbon_v1/studies_carbon.rds")


data2_studies_carbon = studies_carbon[,c("nct_id","study_first_submitted_date","start_date","completion_date",
                                         "overall_status","phase","enrollment",
                                         "source","source_class","number_of_arms",
                                         "why_stopped","has_dmc","plan_to_share_ipd")]

library(dplyr)
library(lubridate)
library(tibble)

data2_studies_carbon = data2_studies_carbon %>%
  mutate(submitted_year = year(as.Date(data2_studies_carbon$study_first_submitted_date))) %>%
  select(1:study_first_submitted_date, submitted_year, everything())

data2_studies_carbon = data2_studies_carbon %>%
  mutate(start_year = year(as.Date(data2_studies_carbon$start_date))) %>%
  select(1:start_date, start_year, everything())

#data2_studies_carbon = data2_studies_carbon %>%
#  mutate(completion_year = year(as.Date(data2_studies_carbon$completion_date))) %>%
#  select(1:completion_date, completion_year, everything())

data2_studies_carbon$start_date = as.Date(data2_studies_carbon$start_date)
data2_studies_carbon$study_first_submitted_date = as.Date(data2_studies_carbon$study_first_submitted_date)

data2_studies_carbon = data2_studies_carbon %>%
  filter(study_first_submitted_date > as.Date("2007-09-27") & study_first_submitted_date <= as.Date("2023-06-03"))

### 注意注意，submitted_year才是注册时间！

data2_studies_carbon = data2_studies_carbon %>%
  mutate(year_segm = NA) %>%
  select(1:submitted_year, year_segm, everything())

data2_studies_carbon$year_segm = case_when(
  data2_studies_carbon$study_first_submitted_date > as.Date("2007-09-27") & 
    data2_studies_carbon$study_first_submitted_date <= as.Date("2012-09-27") ~ "2007-2012",
  data2_studies_carbon$study_first_submitted_date > as.Date("2012-09-27") &
    data2_studies_carbon$study_first_submitted_date <= as.Date("2017-09-27") ~ "2012-2017",
  data2_studies_carbon$study_first_submitted_date > as.Date("2017-09-27") ~ "2017-2023",
  TRUE ~ NA_character_   #不满足任何条件则为缺失值
)

data2_studies_carbon$year_segm = as.factor(data2_studies_carbon$year_segm)
####### 官网未提供，这里代之以后一天
data2_studies_carbon$start_date[34] <- "2012-02-19"

data2_studies_carbon = data2_studies_carbon %>%
  mutate(Registration_before_participant_enrollment = NA) %>%
  select(1:submitted_year, Registration_before_participant_enrollment, everything())

data2_studies_carbon$Registration_before_participant_enrollment = case_when(
  as.Date(data2_studies_carbon$study_first_submitted_date) < as.Date(data2_studies_carbon$start_date) ~ "t",
  as.Date(data2_studies_carbon$study_first_submitted_date) >= as.Date(data2_studies_carbon$start_date) ~ "f",
  TRUE ~ NA_character_
)

data2_studies_carbon = data2_studies_carbon %>%
  mutate(phase_done = phase) %>%
  select(1:phase, phase_done, everything())
data2_studies_carbon$phase_done[which(data2_studies_carbon$phase == "Early Phase 1")] = "Phase 1"

data2_studies_carbon = data2_studies_carbon %>%
  mutate(Recruitment_status = overall_status) %>%
  select(1:overall_status, Recruitment_status, everything())

#ongoing为因子层次外的值，不能直接赋值，需要转换
data2_studies_carbon$Recruitment_status = as.character(data2_studies_carbon$Recruitment_status)
data2_studies_carbon$Recruitment_status[data2_studies_carbon$overall_status %in% 
                                          c("Not yet recruiting", "Recruiting",
                                            "Enrolling by invitation", "Active, not recruiting", 
                                            "Suspended")] <- "Ongoing"
data2_studies_carbon$Recruitment_status[data2_studies_carbon$overall_status %in%
                                          c("Withdrawn", "Terminated")] <- "Stopped Early"

data2_studies_carbon = data2_studies_carbon %>%
  mutate(enrollment_segm = NA) %>%
  select(1:enrollment, enrollment_segm, everything())
data2_studies_carbon$enrollment_segm = case_when(
  data2_studies_carbon$enrollment <= 50 ~ "<50",
  data2_studies_carbon$enrollment >50 & data2_studies_carbon$enrollment <= 100 ~ "50-100",
  data2_studies_carbon$enrollment >100 ~ ">100",
  TRUE ~ NA_character_
)

data2_studies_carbon = data2_studies_carbon %>%
  mutate(arms_segm = NA) %>%
  select(1:number_of_arms, arms_segm,everything())
data2_studies_carbon$arms_segm = case_when(
  data2_studies_carbon$number_of_arms == 1 ~ "1",
  data2_studies_carbon$number_of_arms == 2 ~ "2",
  data2_studies_carbon$number_of_arms >= 3 ~ ">3",
  TRUE ~ NA_character_
)

save(data2_studies_carbon, file = "data2/data2_studies_carbon.rdata")


############################# For conditions ################################
### 找出重复项目
#duplicated_conditions_carbon = conditions_carbon[duplicated(conditions_carbon$nct_id),]
#done_conditions_carbon = conditions_carbon[!duplicated(conditions_carbon$nct_id),]
#uni_dup_conditions_carbon = duplicated_conditions_carbon[!duplicated(duplicated_conditions_carbon$nct_id),]

year_nct = data2_studies_carbon$nct_id

conditions_carbon = readRDS(file = "Extraction_carbon_v1/conditions_carbon.rds")
conditions_carbon = conditions_carbon %>% filter(nct_id %in% year_nct)

done_conditions_carbon = conditions_carbon
done_conditions_carbon$cancer_type = NA
## CNS, Head_and_Neck, gastrointestinal, Lung, Breast, Sarcoma, Prostate, Gynecology


done_conditions_carbon$cancer_type = ifelse(grepl("nerve|pituitary|intracranial|acoustic|schwannoma|pineoblastoma|medulloblastoma|pineocytoma|glioma|brain|ependymoma|astrocytoma|meningioma|craniopharyngioma|cns cancer|central nervous system|glioblastoma", done_conditions_carbon$name, ignore.case = T),
                                            "CNS", done_conditions_carbon$cancer_type)

done_conditions_carbon$cancer_type = ifelse(grepl("schneiderian|tongue|head and neck|oropharyngeal|nasal|head-and-neck|hnscc|head cancer|neck cancer|tonsil|nasopharyngeal|pharyngeal|laryngeal|oral cavity", done_conditions_carbon$name, ignore.case = T),
                                            "Head_and_Neck", done_conditions_carbon$cancer_type)

done_conditions_carbon$cancer_type = ifelse(grepl("cholangiocarcinoma|anus|colorectal|esophageal|pancreatic|rectal|anal|esophagus|gi cancer|gastrointestinal|liver|hepatocellular", done_conditions_carbon$name, ignore.case = T),
                                            "Digestive_System", done_conditions_carbon$cancer_type)

done_conditions_carbon$cancer_type = ifelse(grepl("lung", done_conditions_carbon$name, ignore.case = T),
                                            "Lung", done_conditions_carbon$cancer_type)

done_conditions_carbon$cancer_type = ifelse(grepl("breast", done_conditions_carbon$name, ignore.case = T),
                                            "Breast", done_conditions_carbon$cancer_type)

done_conditions_carbon$cancer_type = ifelse(grepl("sarcoma", done_conditions_carbon$name, ignore.case = T), 
                                            "sarcoma", done_conditions_carbon$cancer_type)

done_conditions_carbon$cancer_type = ifelse(grepl("sarcoma|chordoma", done_conditions_carbon$name, ignore.case = T), 
                                            "sarcoma", done_conditions_carbon$cancer_type)

done_conditions_carbon$cancer_type = ifelse(grepl("prostate|prostatic", done_conditions_carbon$name, ignore.case = T), 
                                            "Prostate", done_conditions_carbon$cancer_type)

done_conditions_carbon$cancer_type = ifelse(grepl("cervical|endometrial|uterine|gynecologic", done_conditions_carbon$name, ignore.case = T), 
                                            "Gynecology", done_conditions_carbon$cancer_type)

done_conditions_carbon$cancer_type = ifelse(is.na(done_conditions_carbon$cancer_type), "Others", done_conditions_carbon$cancer_type)

write.xlsx(done_conditions_carbon, file = file.path("Extraction_carbon_v1","done_conditions_carbon.xlsx"))
saveRDS(done_conditions_carbon, file = "done_conditions_carbon.rds")
rm(done_conditions_carbon)
### 余下手动555555.筛选完成，下一步


######################### For facilities    ###################################
done_facilities = facility_investigators_carbon[which(facility_investigators_carbon$role == "Principal Investigator"),]


############################## for designs #####################################
#### nct_id, allocation, intervention_model, masking
designs_carbon = readRDS(file = "Extraction_carbon_v1/designs_carbon.rds")

data2_designs_carbon = designs_carbon %>% filter(nct_id %in% year_nct)

data2_designs_carbon = data2_designs_carbon[,c("nct_id", "allocation", "intervention_model", "masking")]

save(data2_designs_carbon, file = "data2/data2_designs_carbon.rdata")

############################# for eligibilities ################################
#### nct_id, gender, minimun_age, maximum_age, adult, child, older_adult
eligibilities_carbon = readRDS(file = "Extraction_carbon_v1/eligibilities_carbon.rds")

data2_eligibilities_proton = eligibilities_carbon[,c("nct_id", "gender", "minimum_age",
                                                     "maximum_age", "adult", "child", "older_adult")]
data2_eligibilities_proton = data2_eligibilities_proton %>% filter(nct_id %in% year_nct)

data2_eligibilities_carbon = data2_eligibilities_proton[,c("nct_id", "gender", "minimum_age",
                                                     "maximum_age", "adult", "child", "older_adult")]

data2_eligibilities_carbon = data2_eligibilities_carbon %>%
  mutate(excludes_children = NA) %>%
  select(1:child, excludes_children, everything())

data2_eligibilities_carbon$excludes_children = case_when(
  data2_eligibilities_carbon$child == "f" ~ "t",
  data2_eligibilities_carbon$child == "t" ~ "f",
  TRUE ~ NA_character_
)


data2_eligibilities_carbon = data2_eligibilities_carbon %>%
  mutate(excludes_elderly = NA) %>%
  select(1:older_adult, excludes_elderly, everything())
data2_eligibilities_carbon$excludes_elderly = case_when(
  data2_eligibilities_carbon$older_adult == "t" ~ "f",
  data2_eligibilities_carbon$older_adult == "f" ~ "t",
  TRUE ~ NA_character_
)

save(data2_eligibilities_carbon, file = "data2/data2_eligibilities_carbon.rdata")

############################ for condition #####################################
#### 手动整理获得
library(openxlsx)
data2_conditions_carbon = read.xlsx("data2_conditions_carbon.xlsx")

save(data2_conditions_carbon, file = "data2/data2_conditions_carbon.rdata")


############################ for countries #####################################
data2_countries_carbon = readRDS(file = "Extraction_carbon_v1/countries_carbon.rds")
data2_countries_carbon = data2_countries_carbon[, c("nct_id", "name")]
data2_countries_carbon = data2_countries_carbon %>% filter(nct_id %in% year_nct)

data2_countries_carbon = data2_countries_carbon %>%
  mutate(Region = NA) %>%
  select(1:name, Region, everything())

data2_countries_carbon$Region = case_when(
  data2_countries_carbon$name %in% c("United States", "Canada") ~ "North America",
  data2_countries_carbon$name %in% c("Germany", "Denmark", "Sweden",
                                     "France", "Norway", "Italy", "United Kingdom", 
                                     "Spain", "Netherlands", "Switzerland", "Belgium") ~ "Europe",
  data2_countries_carbon$name %in% c("Korea, Republic of", "China", "Taiwan", "Japan") ~ "Asia",
)

save(data2_countries_carbon, file = "data2/data2_countries_carbon.rdata")
rm(data2_countries_carbon)
############################ for facilities #####################################
data2_facilities_carbon = readRDS(file = "Extraction_carbon_v1/facilities_carbon.rds")
data2_facilities_carbon = data2_facilities_carbon[,c("nct_id")]
data2_facilities_carbon = data.frame(nct_id = data2_facilities_carbon)
data2_facilities_carbon = data2_facilities_carbon %>%
  group_by(nct_id) %>%
  summarise(count=n())
colnames(data2_facilities_carbon)[colnames(data2_facilities_carbon)=="count"] <- "No_of_geographic_regions"

data2_facilities_carbon = data2_facilities_carbon %>%
  mutate(No_of_geographic_regions_segm = NA) %>%
  select(1:No_of_geographic_regions, No_of_geographic_regions_segm, everything())

data2_facilities_carbon$No_of_geographic_regions_segm = case_when(
  data2_facilities_carbon$No_of_geographic_regions == 1 ~ "1",
  data2_facilities_carbon$No_of_geographic_regions >=2  ~ "2",
  data2_facilities_carbon$No_of_geographic_regions >= 3 ~ ">=3"
)

save(data2_facilities_carbon, file = "data2/data2_facilities_carbon.rdata")
rm(data2_facilities_carbon)
############################ for Reference #####################################
data2_study_references_carbon = readRDS(file = "Extraction_carbon_v1/study_references_carbon.rds")
data2_study_references_carbon = data2_study_references_carbon[,c("nct_id")]
data2_study_references_carbon = data.frame(nct_id = data2_study_references_carbon)
data2_study_references_carbon = data2_study_references_carbon %>%
  group_by(nct_id) %>%
  summarise(count=n())

nct_id_carbon = read.table("nct_id_carbon.txt", header = T)
nct_id_carbon = subset(nct_id_carbon, !duplicated(nct_id_carbon))

data2_study_references_carbon = merge(nct_id_carbon, data2_study_references_carbon, all.x = T)
data2_study_references_carbon[is.na(data2_study_references_carbon)] = 0

colnames(data2_study_references_carbon)[colnames(data2_study_references_carbon)=="count"] <- "No_of_References"

data2_study_references_carbon = data2_study_references_carbon %>%
  mutate(No_of_References_segm = NA) %>%
  select(1:No_of_References, No_of_References_segm, everything())
data2_study_references_carbon$No_of_References_segm = case_when(
  data2_study_references_carbon$No_of_References == 1 ~ "1",
  data2_study_references_carbon$No_of_References == 0 ~ "0",
  data2_study_references_carbon$No_of_References >=1 ~ ">=1"
)

save(data2_study_references_carbon, file = "data2/data2_study_references_carbon.rdata")
rm(data2_study_references_carbon)
