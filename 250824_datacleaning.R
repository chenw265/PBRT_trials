
## 清晰PRT的癌肿数据并纳入逻辑回归分析
rm(list = ls())
gc()

library(dplyr)
library(tidyr)

rawdata = read.csv("SearchResults 3104.csv")

# 选出id和conditions两列，并重命名
data_conditions = rawdata %>%
  select(NCT.Number, Conditions) %>%
  rename(Id = NCT.Number, conditions = Conditions)
  

# 将data_conditions转换为长数据格式
data_conditions2 = data_conditions %>%
  separate_rows(conditions, sep = "\\|")
## CNS, Head_and_Neck, gastrointestinal, Lung, Breast, Sarcoma, Prostate, Gynecology

# 将data_conditions2中的conditions列进行分类,创建新的列为cancer_type
data_conditions2 = data_conditions2 %>%
  mutate(cancer_type = case_when(
    grepl("Spinal|GBM|Pineal|nerve|pituitary|intracranial|acoustic|schwannoma|pineoblastoma|medulloblastoma|pineocytoma|glioma|brain|ependymoma|astrocytoma|meningioma|craniopharyngioma|cns cancer|central nervous system|glioblastoma", conditions, ignore.case = TRUE) ~ "CNS",
    grepl("Maxillary Sinus|Nasopharyngeal|Larynx|Oropharynx|Oral|Thyroid|Mouths|chneiderian|tongue|head and neck|oropharyngeal|nasal|head-and-neck|hnscc|head cancer|neck cancer|tonsil|nasopharyngeal|pharyngeal|laryngeal|oral cavity", conditions, ignore.case = TRUE) ~ "Head_and_Neck",
    grepl("Colon|HCC|Rectum|Gastric|Pancreas|cholangiocarcinoma|anus|colorectal|esophageal|pancreatic|rectal|anal|esophagus|gi cancer|gastrointestinal|liver|hepatocellular", conditions, ignore.case = TRUE) ~ "Gastrointestinal",
    grepl("sclc|Lung|Non-Small Cell Lung Cancer|NSCLC|Small Cell Lung Cancer", conditions, ignore.case = TRUE) ~ "Lung",
    grepl("Breast", conditions, ignore.case = TRUE) ~ "Breast",
    grepl("sarcoma|chordoma", conditions, ignore.case = TRUE) ~ "Sarcoma",
    grepl("prostate|prostatic|Prosatatic", conditions, ignore.case = TRUE) ~ "Prostate",
    grepl("Fallopian|Vaginal|Ovary|Ovarian|Cervix|Pelvic|cervical|endometrial|uterine|gynecologic", conditions, ignore.case = TRUE) ~ "Gynecology",
    TRUE ~ "Others"
  ))
length(unique(data_conditions2$Id))
# 进一步深度清洗"
## 将cancer_type列中为Others的行中，如果其conditions列包含"therapy"字样则删除该行
data_conditions2 = data_conditions2 %>%
  filter(!(cancer_type == "Others" & grepl("therapy|Treatment", conditions, ignore.case = TRUE)))
length(unique(data_conditions2$Id))


# 输出xlsx文件后进一步人工筛选
library(openxlsx)
write.xlsx(data_conditions2, file = "data_conditions2.xlsx")


# 重新读入人工筛选后的文件
data_conditions3 = read.xlsx("data_conditions2.xlsx")

# 如果data_conditions3中有重复的Id，则判断重复行中cancer_type是否大于等于2种，如果大于等于2种，则删掉该Id对应cancer_type为Others的行

data_conditions4 = data_conditions3 %>%
  group_by(Id) %>%
  mutate(n_cancer_types = n_distinct(cancer_type)) %>%
  ungroup() %>%
  filter(!(n_cancer_types > 1 & cancer_type == "Others")) %>%
  select(-n_cancer_types)

# 如果data_conditions3中有重复的Id,则判断重复行中cancer_type是否有重复，如果存在重复则只保留该Id对应的其中一行即可
data_conditions5 = data_conditions4 %>%
  distinct(Id, cancer_type, .keep_all = TRUE)

prt_condition = data_conditions5

# 如果data_conditions3中有重复的Id,则将cancer_type设置为multi-cancer
#load("prt_condition.rdata")
prt_condition = prt_condition %>%
  group_by(Id) %>%
  mutate(cancer_type = ifelse(n() > 1, "multi-cancer", cancer_type)) %>%
  distinct(Id, cancer_type, .keep_all = TRUE) %>%
  ungroup()
rawdata = read.csv("SearchResults 3104.csv")

# rawdata的每一个rawdata$NCT.Number都应该在prt_condition$Id中，如果不在则在prt_condition中添加该行，conditions和cancer_type列的值补充为"Others"
missing_ids = setdiff(rawdata$NCT.Number, prt_condition$Id)
if (length(missing_ids) > 0) {
  missing_rows = data.frame(Id = missing_ids, conditions = "Others", cancer_type = "Others")
  prt_condition2 = rbind(prt_condition, missing_rows)
}
length(unique(prt_condition2$Id)) 

prt_condition = prt_condition2

save(prt_condition, file = "prt_condition.rdata")
write.xlsx(prt_condition, file = "prt_condition.xlsx")





