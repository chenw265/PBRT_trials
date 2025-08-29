library(ggplot2)
library(dplyr)
library(ggsci)

####################### 整合数据 ###############################################
load("WHO/data2_carbon_who.rdata")
load("WHO/data2_proton_who.rdata")

data2_carbon_who[] = lapply(data2_carbon_who, function(x) gsub("\n", "", x))
data2_carbon_who[] = lapply(data2_carbon_who, trimws)

data2_proton_who[] = lapply(data2_proton_who, function(x) gsub("\n", "", x))
data2_proton_who[] = lapply(data2_proton_who, trimws)

#load(file = "data2/data2_conditions_proton.rdata")
load(file = "data2/data2_designs_proton.rdata")
load(file = "data2/data2_eligibilities_proton.rdata")
load(file = "data2/data2_studies_proton.rdata")
load(file = "data2/data2_countries_proton.rdata")

#load(file = "data2/data2_facilities_proton.rdata")
#load(file = "data2/data2_study_references_proton.rdata")

data = merge(data2_studies_proton, data2_designs_proton, all.x = T)
#data = merge(data, data2_countries_proton, all.x = T)
#data$Region[is.na(data$Region)] = "Other/NA"
#data = select(data, -name)
#data = merge(data, data2_conditions_proton, all.x = T)
#data = select(data, -name)
data = merge(data, data2_eligibilities_proton, all.x = T)
#data = merge(data, data2_facilities_proton, all.x = T)
#data = merge(data, data2_study_references_proton, all.x = T)

data_proton = data
rm(data)


#load(file = "data2/data2_conditions_carbon.rdata")
load(file = "data2/data2_designs_carbon.rdata")
load(file = "data2/data2_eligibilities_carbon.rdata")
load(file = "data2/data2_studies_carbon.rdata")
#load(file = "data2/data2_countries_carbon.rdata")
#load(file = "data2/data2_facilities_carbon.rdata")
#load(file = "data2/data2_study_references_carbon.rdata")

data = merge(data2_studies_carbon, data2_designs_carbon, all.x = T)
#data = merge(data, data2_countries_carbon, all.x = T)
#data$Region[is.na(data$Region)] = "Other/NA"
#data = select(data, -name)
#data = merge(data, data2_conditions_carbon, all.x = T)
#data = select(data, -name)
data = merge(data, data2_eligibilities_carbon, all.x = T)
#data = merge(data, data2_facilities_carbon, all.x = T)
#data = merge(data, data2_study_references_carbon, all.x = T)

data_carbon = data
rm(data)

######### 统一化列名 #########################
colnames(data_carbon) = paste0(toupper(substr(colnames(data_carbon), 1, 1)), tolower(substr(colnames(data_carbon), 2, nchar(colnames(data_carbon)))))
colnames(data_proton) = paste0(toupper(substr(colnames(data_proton), 1, 1)), tolower(substr(colnames(data_proton), 2, nchar(colnames(data_proton)))))

colnames(data2_carbon_who) = paste0(toupper(substr(colnames(data2_carbon_who), 1, 1)), tolower(substr(colnames(data2_carbon_who), 2, nchar(colnames(data2_carbon_who)))))
colnames(data2_proton_who) = paste0(toupper(substr(colnames(data2_proton_who), 1, 1)), tolower(substr(colnames(data2_proton_who), 2, nchar(colnames(data2_proton_who)))))


colnames(data2_carbon_who)[colnames(data2_carbon_who) == "Trialid"] <- "Id"
colnames(data2_carbon_who)[colnames(data2_carbon_who) == "Inclusion_gender"] <- "Gender"
colnames(data2_carbon_who)[colnames(data2_carbon_who) == "Phase"] <- "Phase_done"

colnames(data2_proton_who)[colnames(data2_proton_who) == "Trialid"] <- "Id"
colnames(data2_proton_who)[colnames(data2_proton_who) == "Inclusion_gender"] <- "Gender"
colnames(data2_proton_who)[colnames(data2_proton_who) == "Phase"] <- "Phase_done"



colnames(data_carbon)[colnames(data_carbon) == "Nct_id"] <- "Id"
colnames(data_carbon)[colnames(data_carbon) == "Submitted_year"] = "Year_registration"

colnames(data_proton)[colnames(data_proton) == "Nct_id"] <- "Id"
colnames(data_proton)[colnames(data_proton) == "Submitted_year"] = "Year_registration"



data_carbon$Ct_type <- "carbon"
data_proton$Ct_type <- "proton"
data_both_ct = rbind(data_carbon, data_proton)
data_both_ct$Platform <- "CT"

save(data_carbon, file = "data3/data_carbon_ct.rdata")
save(data_proton, file = "data3/data_proton_ct.rdata")
save(data_both_ct, file = "data3/data_both_ct.rdata")

data2_carbon_who$Ct_type <- "carbon"
data2_proton_who$Ct_type <- "proton"
data_both_who = rbind(data2_carbon_who, data2_proton_who)
data_both_who$Platform <- "WHO"

save(data2_carbon_who, file = "data3/data_carbon_who.rdata")
save(data2_proton_who, file = "data3/data_proton_who.rdata")
save(data_both_who, file = "data3/data_both_who.rdata")

#### 合并两个platform数据
who_coln = colnames(data_both_who)
ct_coln = colnames(data_both_ct)
who_ct_coln = intersect(who_coln, ct_coln)

data_who = data_both_who[,who_ct_coln]
data_ct = data_both_ct[,who_ct_coln]
data_all = rbind(data_who, data_ct)
data_all$Phase_done[data_all$Phase_done == "Not selected"] <- "Not Applicable"
data_all$Phase_done[data_all$Phase_done == "Not applicable"] <- "Not Applicable"
data_all$Phase_done[data_all$Phase_done == "Phase 1/Phase 3"] <- "Phase 1/Phase 2"

save(data_all, file = "data3/data_all.rdata")
writexl::write_xlsx(data_all, path = "data3/data_all.xlsx")
######################## Phase随年份变化图 #####################################
####### Phase_Year
load("data3/data_all.rdata")
library(dplyr)
library(ggplot2)
library(ggsci)
data = data_all[,c("Year_registration", "Phase_done")]
#data$phase_done = as.character(data$phase_done)

data = data %>%
  group_by(Year_registration,Phase_done) %>%
  summarize(count=n())
data$Year_registration = as.character(data$Year_registration)

factor_order = c("Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3", "Not Applicable")
data$Phase_done = factor(data$Phase_done, levels = factor_order)

library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:5], 1) 
#fill_colors = alpha(c("#003660", "#76A2BB", "#BFD9E6","#F8F2ED", "#D64F39"),1)
#c("#E5B58B","#E29476","#D97A5D","#D64F39","#B33020")
#c("#003660","#3C5D82","#76A2BB","#BFD9E6","#F8F2ED","#D64F39")
#fill_colors = c("#003660","#3C5D82","#76A2BB","#AFC7D7","#BFD9E6")
fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Phase_done, y = count, x = Year_registration)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Phase")

ggsave("Phase_Year_bar.pdf",device = cairo_pdf,width =5, height =3)

############ 删掉not applicable的图
data_all_not = data_all[which(data_all$Phase_done != "Not Applicable"), ]
data = data_all_not[,c("Year_registration", "Phase_done")]
#data$phase_done = as.character(data$phase_done)

data = data %>%
  group_by(Year_registration,Phase_done) %>%
  summarize(count=n())
data$Year_registration = as.character(data$Year_registration)

factor_order = c("Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3")
data$Phase_done = factor(data$Phase_done, levels = factor_order)

library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:5], 1) 
#fill_colors = alpha(c("#003660", "#76A2BB", "#BFD9E6","#F8F2ED", "#D64F39"),1)
#c("#E5B58B","#E29476","#D97A5D","#D64F39","#B33020")
#c("#003660","#3C5D82","#76A2BB","#BFD9E6","#F8F2ED","#D64F39")
#fill_colors = c("#003660","#3C5D82","#76A2BB","#AFC7D7","#BFD9E6")
#fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Phase_done, y = count, x = Year_registration)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Phase")

ggsave("Phase_Year_not_bar.pdf",device = cairo_pdf,width =5, height =3)


ggplot(data, aes(fill = Phase_done, y = count, x = Year_registration)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Phase")

ggsave("Phase_Year_not_fill_bar.pdf",device = cairo_pdf,width =5, height =3)

######################## Source随年份变化图 #####################################
load("data3/data_all.rdata")
library(dplyr)
library(ggplot2)
data = data_all[,c("Year_registration", "Source_class")]
#data$phase_done = as.character(data$phase_done)

data = data %>%
  group_by(Year_registration,Source_class) %>%
  summarize(count=n())
data$Year_registration = as.character(data$Year_registration)

factor_order = c("INDUSTRY", "GOV", "OTHER", "None/Not Report")
data$Source_class = factor(data$Source_class, levels = factor_order)

library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:3], 1) 
#fill_colors = alpha(c("#003660", "#76A2BB", "#BFD9E6","#F8F2ED", "#D64F39"),1)
#c("#E5B58B","#E29476","#D97A5D","#D64F39","#B33020")
#c("#003660","#3C5D82","#76A2BB","#BFD9E6","#F8F2ED","#D64F39")
#fill_colors = c("#003660","#3C5D82","#76A2BB","#AFC7D7","#BFD9E6")
fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Source_class, y = count, x = Year_registration)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Source Class")

ggsave("Source_Year_bar.pdf",device = cairo_pdf,width =5, height =3)

ggplot(data, aes(fill = Source_class, y = count, x = Year_registration)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Source Class")

ggsave("Source_Year_fill_bar.pdf",device = cairo_pdf,width =5, height =3)
######################## TYPE 随年份变化图 #####################################
data = data_all[,c("Year_registration", "Ct_type")]
#data$phase_done = as.character(data$phase_done)

data = data %>%
  group_by(Year_registration,Ct_type) %>%
  summarize(count=n())
data$Year_registration = as.character(data$Year_registration)

factor_order = c("carbon", "proton")
data$Ct_type = factor(data$Ct_type, levels = factor_order)

library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[2:3], 1) 
#fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Ct_type, y = count, x = Year_registration)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Type")

ggsave("Type_Year_bar.pdf",device = cairo_pdf,width =5, height =3)



######################## Phase随year_segm变化图 #####################################
####### Phase_Year
#load("data3/data_all.rdata")
#library(dplyr)
#library(ggplot2)
#library(ggsci)
data = data_all[,c("Year_segm", "Phase_done")]
#data$phase_done = as.character(data$phase_done)

data = data %>%
  group_by(Year_segm,Phase_done) %>%
  summarize(count=n())
data$Year_segm = as.character(data$Year_segm)

factor_order = c("Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3", "Not Applicable")
data$Phase_done = factor(data$Phase_done, levels = factor_order)

#library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:5], 1) 
#fill_colors = alpha(c("#003660", "#76A2BB", "#BFD9E6","#F8F2ED", "#D64F39"),1)
#c("#E5B58B","#E29476","#D97A5D","#D64F39","#B33020")
#c("#003660","#3C5D82","#76A2BB","#BFD9E6","#F8F2ED","#D64F39")
#fill_colors = c("#003660","#3C5D82","#76A2BB","#AFC7D7","#BFD9E6")
fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Phase_done, y = count, x = Year_segm)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Phase")

ggsave("Phase_Year_segm_bar.pdf",device = cairo_pdf,width =3.5, height =3)


######################## Source随Year_segm变化图 #####################################
data = data_all[,c("Year_segm", "Source_class")]
#data$phase_done = as.character(data$phase_done)

data = data %>%
  group_by(Year_segm,Source_class) %>%
  summarize(count=n())
data$Year_segm = as.character(data$Year_segm)

factor_order = c("INDUSTRY", "GOV", "OTHER", "None/Not Report")
data$Source_class = factor(data$Source_class, levels = factor_order)

#library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:3], 1) 
#fill_colors = alpha(c("#003660", "#76A2BB", "#BFD9E6","#F8F2ED", "#D64F39"),1)
#c("#E5B58B","#E29476","#D97A5D","#D64F39","#B33020")
#c("#003660","#3C5D82","#76A2BB","#BFD9E6","#F8F2ED","#D64F39")
#fill_colors = c("#003660","#3C5D82","#76A2BB","#AFC7D7","#BFD9E6")
fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Source_class, y = count, x = Year_segm)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Source Class")

ggsave("Source_Year_segm_bar.pdf",device = cairo_pdf,width =3.5, height =3)


######################## TYPE 随Year_segm变化图 #####################################
data = data_all[,c("Year_segm", "Ct_type")]
#data$phase_done = as.character(data$phase_done)

data = data %>%
  group_by(Year_segm,Ct_type) %>%
  summarize(count=n())
data$Year_segm = as.character(data$Year_segm)

factor_order = c("carbon", "proton")
data$Ct_type = factor(data$Ct_type, levels = factor_order)

library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[2:3], 1) 
#fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Ct_type, y = count, x = Year_segm)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Type")

ggsave("Type_Year_segm_bar.pdf",device = cairo_pdf,width =3.5, height =3)



############################## TABLE ONE  ALL in yearsegm #######################################
library(dplyr)
library(tableone)
load("data3/data_all.rdata")
data = data_all
## 定义需要纳入的变量
### 以year_segm作为分组变量，不加入vars中
dput(names(data))
## copy过来删减
vars = c("Ct_type", "Registration_before_participant_enrollment", 
         "Phase_done", "Recruitment_status", "Allocation", "Masking", 
         "Excludes_children", "Excludes_elderly", "Gender", "Enrollment_segm", 
         "Source_class", "Platform")

## 指定分类变量
factorvars = vars



## 创建初步表格并查看

ALL_yearsegm_tableone = CreateTableOne(vars = vars,
                                       #strata = "Year_segm",
                                       data = data,
                                       factorVars = factorvars)

exactvars = factorvars

output_ALL_yearsegm_table1 = print(x = ALL_yearsegm_tableone, #指定表格
                                   #contDigits = 1, #连续变量保留1位小数
                                   #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                   #nonnormal = nonnormalvars, #指定非正态连续变量
                                   #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                   showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                   noSpaces = TRUE, #删除用于对齐的空格
                                   printToggle = FALSE) #不展示输出结果

write.csv(output_ALL_yearsegm_table1, file = "Results/ALL_yearsegm_tableone3.csv")

library(CBCgrps)

tab1 = multigrps(data,
               gvar = "Year_segm",
               varlist = vars,
               ShowStatistic = T,
               workspace = 2e+09) # 给Fisher分配内存。

output = tab1
write.csv(output, file = "Results/ALL_yearsegm_tableone3_CBCgrps.csv")

########################## CARBON

data = data_all[which(data_all$Ct_type == "carbon"),]
## 定义需要纳入的变量
### 以year_segm作为分组变量，不加入vars中
dput(names(data))
## copy过来删减
vars = c("Registration_before_participant_enrollment", 
         "Phase_done", "Recruitment_status", "Allocation", "Masking", 
         "Excludes_children", "Excludes_elderly", "Gender", "Enrollment_segm", 
         "Source_class", "Platform")

## 指定分类变量
factorvars = vars



## 创建初步表格并查看

Carbon_yearsegm_tableone = CreateTableOne(vars = vars,
                                       strata = "Year_segm",
                                       data = data,
                                       factorVars = factorvars)

exactvars = factorvars

output_Carbon_yearsegm_table1 = print(x = Carbon_yearsegm_tableone, #指定表格
                                   #contDigits = 1, #连续变量保留1位小数
                                   #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                   #nonnormal = nonnormalvars, #指定非正态连续变量
                                   exact = exactvars,  #指定需要Fisher确切法统计的变量
                                   showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                   noSpaces = TRUE, #删除用于对齐的空格
                                   printToggle = FALSE) #不展示输出结果

write.csv(output_Carbon_yearsegm_table1, file = "Results/Carbon_yearsegm_tableone.csv")


output_Carbon_yearsegm_table1 = print(x = Carbon_yearsegm_tableone, #指定表格
                                      #contDigits = 1, #连续变量保留1位小数
                                      #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                      #nonnormal = nonnormalvars, #指定非正态连续变量
                                      #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                      showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                      noSpaces = TRUE, #删除用于对齐的空格
                                      printToggle = FALSE) #不展示输出结果

write.csv(output_Carbon_yearsegm_table1, file = "Results/Carbon_yearsegm_tableone2.csv")

########################## PROTON

data = data_all[which(data_all$Ct_type == "proton"),]
## 定义需要纳入的变量
### 以year_segm作为分组变量，不加入vars中
dput(names(data))
## copy过来删减
vars = c("Registration_before_participant_enrollment", 
         "Phase_done", "Recruitment_status", "Allocation", "Masking", 
         "Excludes_children", "Excludes_elderly", "Gender", "Enrollment_segm", 
         "Source_class", "Platform")

## 指定分类变量
factorvars = vars



## 创建初步表格并查看

Proton_yearsegm_tableone = CreateTableOne(vars = vars,
                                          strata = "Year_segm",
                                          data = data,
                                          factorVars = factorvars)

exactvars = factorvars

output_Proton_yearsegm_table1 = print(x = Proton_yearsegm_tableone, #指定表格
                                      #contDigits = 1, #连续变量保留1位小数
                                      #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                      #nonnormal = nonnormalvars, #指定非正态连续变量
                                      exact = exactvars,  #指定需要Fisher确切法统计的变量
                                      showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                      noSpaces = TRUE, #删除用于对齐的空格
                                      printToggle = FALSE) #不展示输出结果

write.csv(output_Proton_yearsegm_table1, file = "Results/Proton_yearsegm_tableone.csv")


output_Proton_yearsegm_table1 = print(x = Proton_yearsegm_tableone, #指定表格
                                      #contDigits = 1, #连续变量保留1位小数
                                      #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                      #nonnormal = nonnormalvars, #指定非正态连续变量
                                      #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                      showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                      noSpaces = TRUE, #删除用于对齐的空格
                                      printToggle = FALSE) #不展示输出结果

write.csv(output_Proton_yearsegm_table1, file = "Results/Proton_yearsegm_tableone2.csv")


############################## TABLE ONE  ALL in Ct type #######################################

vars = c("Year_segm", "Registration_before_participant_enrollment", 
         "Phase_done", "Recruitment_status", "Allocation", "Masking", 
         "Excludes_children", "Excludes_elderly", "Gender", "Enrollment_segm", 
         "Source_class", "Platform")

## 指定分类变量
factorvars = vars



## 创建初步表格并查看

ALL_ct_type_tableone = CreateTableOne(vars = vars,
                                       #strata = "Ct_type",
                                       data = data,
                                       factorVars = factorvars)

exactvars = factorvars

output_ALL_ct_type_table1 = print(x = ALL_ct_type_tableone, #指定表格
                                   #contDigits = 1, #连续变量保留1位小数
                                   #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                   #nonnormal = nonnormalvars, #指定非正态连续变量
                                   #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                   showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                   noSpaces = TRUE, #删除用于对齐的空格
                                   printToggle = FALSE) #不展示输出结果

write.csv(output_ALL_ct_type_table1, file = "Results/ALL_ct_type_tableone3.csv")

library(CBCgrps)
tab1 = twogrps(data,
               gvar = "Ct_type",
               varlist = vars,
               ShowStatistic = T,
               workspace = 2e+09) # 给fFisher分配内存。

output = tab1$Table

write.csv(output, file = "Results/ALL_ct_type_tableone3_CBCgrps.csv")


####################### Region ################################################

load("data3/data_both_who.rdata")
load(file = "data2/data2_countries_proton.rdata")
load(file = "data2/data2_countries_carbon.rdata")
data2_countries_ct = rbind(data2_countries_carbon, data2_countries_proton)
data2_countries_who = data_both_who[,c("Id", "Countries", "Region")]

colnames(data2_countries_ct)[colnames(data2_countries_ct) == "nct_id"] = "Id"
colnames(data2_countries_ct)[colnames(data2_countries_ct) == "name"] = "Countries"

data3_countries = rbind(data2_countries_ct, data2_countries_who)

data3_countries$Region = case_when(
  data3_countries$Countries %in% c("United States", "Canada") ~ "North America",
  data3_countries$Countries %in% c("Germany", "Denmark", "Sweden",
                                     "France", "Norway", "Italy", "United Kingdom", 
                                     "Spain", "Netherlands", "Switzerland", "Belgium", "Poland",
                                   "Hungary", "United Kingdom;England;Wales", "Austria") ~ "Europe",
  data3_countries$Countries %in% c("Korea, Republic of", "China", "Taiwan", "Japan",
                                   "Chian", "P.R. China", "Thailand") ~ "Asia",
)
#colnames(data3_countries)[colnames(data3_countries) == "ID"] = "Id"

save(data3_countries, file = "data3/data3_countries.rdata")

data4_countries = merge(data_all, data3_countries, all.x = T)
data4_countries = unique(data4_countries)
data4_countries$Region[is.na(data4_countries$Region)] = "Other/NA"
data4_countries$Countries = as.character(data4_countries$Countries)
data4_countries$Countries[is.na(data4_countries$Countries)] = "Other/NA"
save(data4_countries, file = "data3/data4_countries.rdata")

####################### Region 随 year变化图 ################################################
load("data3/data4_countries.rdata")
library(dplyr)
library(ggplot2)
library(ggsci)
data = data4_countries[,c("Year_registration", "Region")]

data = data %>%
  group_by(Year_registration,Region) %>%
  summarize(count=n())
data$Year_registration = as.character(data$Year_registration)

factor_order = c("Europe", "North America","Asia", "Other/NA")
data$Region = factor(data$Region, levels = factor_order)

library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:4], 1) 
#fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Region, y = count, x = Year_registration)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Region_Year_bar.pdf",device = cairo_pdf,width =5, height =3)

ggplot(data, aes(fill = Region, y = count, x = Year_registration)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Region_Year_fillbar.pdf",device = cairo_pdf,width =5, height =3)

############## CARBON 
data4_countries_carbon = data4_countries[which(data4_countries$Ct_type == "carbon"),]
data = data4_countries_carbon[,c("Year_registration", "Region")]

data = data %>%
  group_by(Year_registration,Region) %>%
  summarize(count=n())
data$Year_registration = as.character(data$Year_registration)

factor_order = c("Europe", "North America","Asia", "Other/NA")
data$Region = factor(data$Region, levels = factor_order)

library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:4], 1) 
#fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Region, y = count, x = Year_registration)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Carbon_Region_Year_bar.pdf",device = cairo_pdf,width =5, height =3)

ggplot(data, aes(fill = Region, y = count, x = Year_registration)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Carbon_Region_Year_fillbar.pdf",device = cairo_pdf,width =5, height =3)

############## PROTON 
data4_countries_proton = data4_countries[which(data4_countries$Ct_type == "proton"),]
data = data4_countries_proton[,c("Year_registration", "Region")]

data = data %>%
  group_by(Year_registration,Region) %>%
  summarize(count=n())
data$Year_registration = as.character(data$Year_registration)

factor_order = c("Europe", "North America","Asia", "Other/NA")
data$Region = factor(data$Region, levels = factor_order)

library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:4], 1) 
#fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Region, y = count, x = Year_registration)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Proton_Region_Year_bar.pdf",device = cairo_pdf,width =5, height =3)

ggplot(data, aes(fill = Region, y = count, x = Year_registration)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Proton_Region_Year_fillbar.pdf",device = cairo_pdf,width =5, height =3)



####################### Region 随 year_segm变化图 ################################################
data = data4_countries[,c("Year_segm", "Region")]

data = data %>%
  group_by(Year_segm,Region) %>%
  summarize(count=n())
data$Year_segm = as.character(data$Year_segm)

factor_order = c("Europe", "North America","Asia", "Other/NA")
data$Region = factor(data$Region, levels = factor_order)

#library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:4], 1) 
#fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Region, y = count, x = Year_segm)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Region_Year_segm_bar.pdf",device = cairo_pdf,width =3.5, height =3)

ggplot(data, aes(fill = Region, y = count, x = Year_segm)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Region_Year_segm_fillbar.pdf",device = cairo_pdf,width =3.5, height =3)

####################### Region 随 type变化图 ################################################
data = data4_countries[,c("Ct_type", "Region")]

data = data %>%
  group_by(Ct_type,Region) %>%
  summarize(count=n())
data$Ct_type = as.character(data$Ct_type)

factor_order = c("Europe", "North America","Asia", "Other/NA")
data$Region = factor(data$Region, levels = factor_order)

#library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:4], 1) 
#fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Region, y = count, x = Ct_type)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Region_Ct_type_bar.pdf",device = cairo_pdf,width =3, height =3)

ggplot(data, aes(fill = Region, y = count, x = Ct_type)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Region_Ct_type_fillbar.pdf",device = cairo_pdf,width =3, height =3)

####################### Region 随 status变化图 ################################################
data = data4_countries[,c("Recruitment_status", "Region")]

data = data %>%
  group_by(Recruitment_status,Region) %>%
  summarize(count=n())
data$Recruitment_status = as.character(data$Recruitment_status)

factor_order = c("Europe", "North America","Asia", "Other/NA")
data$Region = factor(data$Region, levels = factor_order)

#library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2")[1:4], 1) 
#fill_colors <- c(fill_colors, "#CCCCCC")

ggplot(data, aes(fill = Region, y = count, x = Recruitment_status)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Region_Recruitment_status_bar.pdf",device = cairo_pdf,width =3.5, height =3)

ggplot(data, aes(fill = Region, y = count, x = Recruitment_status)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Region") 

ggsave("Region_Recruitment_status_fillbar.pdf",device = cairo_pdf,width =3.5, height =3)



####################### TABLE ONE Region in yearsegm ###########################


load("data3/data4_countries.rdata")
library(dplyr)
library(tableone)
data = data4_countries
### 以year_segm作为分组变量，不加入vars中
dput(names(data))
## copy过来删减
vars = c("Region")

## 指定分类变量
factorvars = vars



## 创建初步表格并查看

Region_yearsegm_tableone = CreateTableOne(vars = vars,
                                       #strata = "Year_segm",
                                       data = data,
                                       factorVars = factorvars)

exactvars = factorvars

output_Region_yearsegm_table1 = print(x = Region_yearsegm_tableone, #指定表格
                                   #contDigits = 1, #连续变量保留1位小数
                                   #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                   #nonnormal = nonnormalvars, #指定非正态连续变量
                                   #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                   showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                   noSpaces = TRUE, #删除用于对齐的空格
                                   printToggle = FALSE) #不展示输出结果

write.csv(output_Region_yearsegm_table1, file = "Results/Region_yearsegm_tableone3.csv")

library(CBCgrps)
tab1 = multigrps(data,
               gvar = "Year_segm",
               varlist = vars,
               ShowStatistic = T,
               workspace = 2e+09) # 给fFisher分配内存。
output = tab1

write.csv(output, file = "Results/Region_yearsegm_tableone3_CBCgrps.csv")

####################### TABLE ONE Region in Ct type ############################
vars = c("Region")

## 指定分类变量
factorvars = vars



## 创建初步表格并查看

Region_Ct_type_tableone = CreateTableOne(vars = vars,
                                          #strata = "Ct_type",
                                          data = data,
                                          factorVars = factorvars)

exactvars = factorvars

output_Region_Ct_type_table1 = print(x = Region_Ct_type_tableone, #指定表格
                                      #contDigits = 1, #连续变量保留1位小数
                                      #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                      #nonnormal = nonnormalvars, #指定非正态连续变量
                                      #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                      showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                      noSpaces = TRUE, #删除用于对齐的空格
                                      printToggle = FALSE) #不展示输出结果

write.csv(output_Region_Ct_type_table1, file = "Results/Region_Ct_type_tableone3.csv")

library(CBCgrps)
tab1 = twogrps(data,
               gvar = "Ct_type",
               varlist = vars,
               ShowStatistic = T,
               workspace = 2e+09) # 给fFisher分配内存。
output = tab1$Table
write.csv(output, file = "Results/Region_Ct_type_tableone3_CBCgrps.csv")



####################### Condition ################################################
ct_condition_carbon = readRDS("done_conditions_carbon.rds")
ct_condition_proton = readRDS("done_conditions_proton.rds")
ct_condition = rbind(ct_condition_carbon, ct_condition_proton)

load("data3/data_both_who.rdata")
library(dplyr)


data_both_who = data_both_who %>%
  mutate(cancer_type = NA) %>%
  select(1:Condition, cancer_type, everything())
data_both_who$cancer_type = ifelse(grepl("nerve|pituitary|intracranial|acoustic|schwannoma|pineoblastoma|medulloblastoma|pineocytoma|glioma|brain|ependymoma|astrocytoma|meningioma|craniopharyngioma|cns cancer|central nervous system|glioblastoma", data_both_who$Condition, ignore.case = T),
                                            "CNS", data_both_who$cancer_type)
data_both_who$cancer_type = ifelse(grepl("parotid|Oral|schneiderian|tongue|head and neck|oropharyngeal|nasal|head-and-neck|hnscc|head cancer|neck cancer|tonsil|nasopharyngeal|pharyngeal|laryngeal|oral cavity", data_both_who$Condition, ignore.case = T),
                                            "Head_and_Neck", data_both_who$cancer_type)
data_both_who$cancer_type = ifelse(grepl("cholangiocarcinoma|anus|colorectal|esophageal|pancreatic|rectal|anal|esophagus|gi cancer|gastrointestinal|liver|hepatocellular", data_both_who$Condition, ignore.case = T),
                                            "Digestive_System", data_both_who$cancer_type)
data_both_who$cancer_type = ifelse(grepl("lung|nsclc|sclc", data_both_who$Condition, ignore.case = T),
                                            "Lung", data_both_who$cancer_type)
data_both_who$cancer_type = ifelse(grepl("breast", data_both_who$Condition, ignore.case = T),
                                            "Breast", data_both_who$cancer_type)
data_both_who$cancer_type = ifelse(grepl("sarcoma|chordoma", data_both_who$Condition, ignore.case = T), 
                                            "sarcoma", data_both_who$cancer_type)
data_both_who$cancer_type = ifelse(grepl("prostate|prostatic", data_both_who$Condition, ignore.case = T), 
                                            "Prostate", data_both_who$cancer_type)
data_both_who$cancer_type = ifelse(grepl("cervical|endometrial|uterine|gynecologic", data_both_who$Condition, ignore.case = T), 
                                            "Gynecology", data_both_who$cancer_type)
data_both_who$cancer_type = ifelse(is.na(data_both_who$cancer_type), "Others", data_both_who$cancer_type)

save(data_both_who, file = "data3/data_both_who.rdata")


data_cancertype_who = data_both_who[,c("Id", "cancer_type")]
data_cancertype_ct = ct_condition[, c("nct_id", "cancer_type")]
colnames(data_cancertype_ct)[colnames(data_cancertype_ct) == "nct_id"] = "Id"
data3_conditions = rbind(data_cancertype_ct, data_cancertype_who)
save(data3_conditions, file = "data3/data3_conditions.rdata")

load("data3/data_all.rdata")
data4_conditions = merge(data_all, data3_conditions, all.x = T)
data4_conditions = unique(data4_conditions)
save(data4_conditions, file = "data3/data4_conditions.rdata")


####################### Condition 随 year变化图 ################################################
load("data3/data4_conditions.rdata")
library(dplyr)
library(ggplot2)
library(ggsci)
data = data4_conditions[,c("Year_registration", "cancer_type")]

data = data %>%
  group_by(Year_registration,cancer_type) %>%
  summarize(count=n())
data$Year_registration = as.character(data$Year_registration)

factor_order = c("Others", "Gynecology", "sarcoma", "Breast","Lung","Head_and_Neck","CNS", "Prostate", "Digestive_System")
data$cancer_type = factor(data$cancer_type, levels = factor_order)

library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2"), 1) 
fill_colors <- c(fill_colors, "#CCCCCC")
fill_colors <- rev(fill_colors)
ggplot(data, aes(fill = cancer_type, y = count, x = Year_registration)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Cancer Type") 

ggsave("cancer_type_Year_bar.pdf",device = cairo_pdf,width =5, height =3)

ggplot(data, aes(fill = cancer_type, y = count, x = Year_registration)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Cancer Type") 

ggsave("cancer_type_Year_fillbar.pdf",device = cairo_pdf,width =5, height =3)


####################### Condition 随 year_segm变化图 ################################################
data = data4_conditions[,c("Year_segm", "cancer_type")]

data = data %>%
  group_by(Year_segm,cancer_type) %>%
  summarize(count=n())
data$Year_segm = as.character(data$Year_segm)

factor_order = c("Others", "Gynecology", "sarcoma", "Breast","Lung","Head_and_Neck","CNS", "Prostate", "Digestive_System")
data$cancer_type = factor(data$cancer_type, levels = factor_order)

#library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2"), 1) 
fill_colors <- c(fill_colors, "#CCCCCC")
fill_colors <- rev(fill_colors)

ggplot(data, aes(fill = cancer_type, y = count, x = Year_segm)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Cancer Type") 

ggsave("cancer_type_Year_segm_bar.pdf",device = cairo_pdf,width =3.5, height =3)

ggplot(data, aes(fill = cancer_type, y = count, x = Year_segm)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Cancer Type") 

ggsave("cancer_type_Year_segm_fillbar.pdf",device = cairo_pdf,width =3.5, height =3)

####################### Condition 随 type变化图 ################################################
data = data4_conditions[,c("Ct_type", "cancer_type")]

data = data %>%
  group_by(Ct_type,cancer_type) %>%
  summarize(count=n())
data$Ct_type = as.character(data$Ct_type)

factor_order = c("Others", "Gynecology", "sarcoma", "Breast","Lung","Head_and_Neck","CNS", "Prostate", "Digestive_System")
data$cancer_type = factor(data$cancer_type, levels = factor_order)

#library(RColorBrewer)
fill_colors <- alpha(brewer.pal(8, "Set2"), 1) 
fill_colors <- c(fill_colors, "#CCCCCC")
fill_colors <- rev(fill_colors)

ggplot(data, aes(fill = cancer_type, y = count, x = Ct_type)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Cancer Type") 

ggsave("cancer_type_Ct_type_bar.pdf",device = cairo_pdf,width =3, height =3)

ggplot(data, aes(fill = cancer_type, y = count, x = Ct_type)) +
  geom_bar(position = "fill", stat = "identity")+ 
  theme_classic() +
  #scale_fill_jco() +
  scale_fill_manual(values = fill_colors)+
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Cancer Type") 

ggsave("cancer_type_Ct_type_fillbar.pdf",device = cairo_pdf,width =3, height =3)



####################### TABLE ONE Condition in yearsegm ###########################
rm(list = ls())
gc()

library(dplyr)
library(tableone)
load("data3/data4_conditions.rdata")
data = data4_conditions

### 以year_segm作为分组变量，不加入vars中
dput(names(data))
## copy过来删减
vars = c("cancer_type")

## 指定分类变量
factorvars = vars



## 创建初步表格并查看

cancer_type_yearsegm_tableone = CreateTableOne(vars = vars,
                                          #strata = "Year_segm",
                                          data = data,
                                          factorVars = factorvars)

exactvars = factorvars

output_cancer_type_yearsegm_table1 = print(x = cancer_type_yearsegm_tableone, #指定表格
                                      #contDigits = 1, #连续变量保留1位小数
                                      #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                      #nonnormal = nonnormalvars, #指定非正态连续变量
                                      #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                      showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                      noSpaces = TRUE, #删除用于对齐的空格
                                      printToggle = FALSE) #不展示输出结果

write.csv(output_cancer_type_yearsegm_table1, file = "Results/cancer_type_yearsegm_tableone3.csv")

library(CBCgrps)
tab1 = multigrps(data,
               gvar = "Year_segm",
               varlist = vars,
               ShowStatistic = T,
               #workspace = 2e+9, # 给fFisher分配内存
               sim = T  #compute p-values by Monte Carlo simulation
               ) 
output = tab1
write.csv(output, file = "Results/cancer_type_yearsegm_tableone3_CBCgrps.csv")



####################### TABLE ONE Conditions in Ct type ############################
vars = c("cancer_type")

## 指定分类变量
factorvars = vars



## 创建初步表格并查看

cancer_type_Ct_type_tableone = CreateTableOne(vars = vars,
                                         #strata = "Ct_type",
                                         data = data,
                                         factorVars = factorvars)

exactvars = factorvars

output_cancer_type_Ct_type_table1 = print(x = cancer_type_Ct_type_tableone, #指定表格
                                     #contDigits = 1, #连续变量保留1位小数
                                     #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                     #nonnormal = nonnormalvars, #指定非正态连续变量
                                     #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                     showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                     noSpaces = TRUE, #删除用于对齐的空格
                                     printToggle = FALSE) #不展示输出结果

write.csv(output_cancer_type_Ct_type_table1, file = "Results/cancer_type_Ct_type_tableone3.csv")

library(CBCgrps)
tab1 = twogrps(data,
               gvar = "Ct_type",
               varlist = vars,
               ShowStatistic = T,
               workspace = 2e+9, # 给fFisher分配内存
               #sim = T  #compute p-values by Monte Carlo simulation
)
output = tab1$Table

write.csv(output, file = "Results/cancer_type_Ct_type_tableone3_CBCgrps.csv")


####################### logisticst 回归分析stopped early和completed的影响因素#########
rm(list = ls())
gc()

load("data3/data_all.rdata")
data_all$Completed = ifelse(data_all$Recruitment_status == "Completed", 1, 0)
data_all$Stopped_Early = ifelse(data_all$Recruitment_status == "Stopped Early", 1, 0)
#data_all$allocation = ifelse(data_all$Allocation == "Randomized" , "Randomized", "Non-Randomized/Not report")
# 绘制相关性热图
data_heatmap = data_all[,c("Phase_done", "Allocation",  "Masking" ,"Year_segm" ,
                             "Registration_before_participant_enrollment" , "Enrollment_segm" ,"Source_class",
                             "Ct_type")]
#data_heatmap = as.character(data_heatmap)
# 假设data是你的全部由分类变量构成的数据框
# 安装并加载vcd包和corrplot包
#install.packages("vcd")
#install.packages("corrplot")
library(vcdExtra)
library(corrplot)

# Assuming 'data' is your data frame with categorical variables
# Compute Cramér's V for the contingency tables
data = data_heatmap
cramer_matrix <- matrix(1, ncol = ncol(data), nrow = ncol(data))
colnames(cramer_matrix) <- colnames(data)  # Set column names
rownames(cramer_matrix) <- colnames(data)  # Set row names

for (i in 1:(ncol(data) - 1)) {
  for (j in (i + 1):ncol(data)) {
    table_ij <- table(data[, i], data[, j])
    cramer_matrix[i, j] <- assocstats(table_ij)$cramer
    cramer_matrix[j, i] <- cramer_matrix[i, j]
  }
}

# Draw the correlation heatmap

corrplot(cramer_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.6)

library(MASS)
data_all[] = lapply(data_all, as.character)
data_all[] = lapply(data_all, factor)

fit_completed = glm(Completed ~  Allocation  + Year_segm +
                      Registration_before_participant_enrollment + Enrollment_segm +Source_class+
                      Phase_done+Ct_type+Gender+Masking+Excludes_children+Excludes_elderly, 
                      data = data_all, family="binomial")

fit_stop = glm(Stopped_Early ~ Allocation + Year_segm + Registration_before_participant_enrollment + 
                 Ct_type + Excludes_elderly, 
                data = data_all, family="binomial")
step_model <- stepAIC(fit_stop, direction = "both")

summary(step_model)

formatFit<-function(fit){
  #取P值
  p<-summary(fit)$coefficients[,4]
  #wald值
  wald<-summary(fit)$coefficients[,3]^2
  #B值
  valueB<-coef(fit)
  #OR值
  valueOR<-exp(coef(fit))
  #OR值得95%CI
  confitOR<-exp(confint(fit))
  data.frame(
    B=round(valueB,3),
    Wald=round(wald,3),
    OR_with_CI=paste(round(valueOR,3),"(",
                     round(confitOR[,1],3),"~",round(confitOR[,2],3),")",sep=""),
    P=format.pval(p,digits = 3,eps=0.001)
  )
}

summary_stop = formatFit(fit_stop)
summary_complete = formatFit(fit_completed)

library(openxlsx)
write.xlsx(summary_complete, file = "Results/summary_complete_fit.xlsx")
write.xlsx(summary_stop, file = "Results/summary_stop_fit.xlsx")

######################### 分割线 ###############################################






########## Phase_Yearsegm
data = data2_studies_proton[,c("Year_segm", "Phase_done")]
data = data %>%
  group_by(year_segm,phase_done) %>%
  summarize(count=n())
## 转换为宽数据
library(tidyr)
data = spread(data, key = "phase_done", value = "count")
data[is.na(data)] <- 0
library(ggradar)
ggradar(
  data,
  values.radar = c("0", "25", "50"),
  grid.min = 0, grid.mid = 25, grid.max = 50,
  #gridline.max.linetype = "solid",
  group.line.width = 1,
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800", "#FC4E07"),
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  base.size = 9,
  grid.line.width = 1
)
ggsave("Phase_Yearsegm_radar.pdf",device = cairo_pdf,width =6, height =6)


##bar图
data = data2_studies_proton[,c("year_segm", "phase_done")]
data = data %>%
  group_by(year_segm,phase_done) %>%
  summarize(count=n())
factor_order = c("Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3", "Not Applicable")
data$phase_done = factor(data$phase_done, levels = factor_order)

ggplot(data, aes(fill = phase_done, y = count, x = year_segm)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  scale_fill_jco() +
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 0,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Phase")

ggsave("Phase_Yearsegm_bar.pdf",device = cairo_pdf,width =4, height =3)



########################## For Carbon ##########################################
library(ggplot2)
library(dplyr)
library(ggsci)

######################## Phase随年份变化图 #####################################
####### Phase_Year

data = data2_studies_carbon[,c("start_year", "phase_done")]
#data$phase_done = as.character(data$phase_done)

data = data %>%
  group_by(start_year,phase_done) %>%
  summarize(count=n())
data$start_year = as.character(data$start_year)

factor_order = c("Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3", "Not Applicable")
data$phase_done = factor(data$phase_done, levels = factor_order)

ggplot(data, aes(fill = phase_done, y = count, x = start_year)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  scale_fill_jco() +
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Phase")

ggsave("Carbon_Phase_Year_bar.pdf",device = cairo_pdf,width =5, height =3)


########## Phase_Yearsegm
data = data2_studies_carbon[,c("year_segm", "phase_done")]
data = data %>%
  group_by(year_segm,phase_done) %>%
  summarize(count=n())
## 转换为宽数据
library(tidyr)
data = spread(data, key = "phase_done", value = "count")
data[is.na(data)] <- 0
library(ggradar)
ggradar(
  data,
  values.radar = c("0", "7", "14"),
  grid.min = 0, grid.mid = 7, grid.max = 14,
  #gridline.max.linetype = "solid",
  group.line.width = 1,
  group.point.size = 3,
  group.colours = c("#00AFBB", "#E7B800", "#FC4E07"),
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom",
  base.size = 9,
  grid.line.width = 1
)
ggsave("Carbon_Phase_Yearsegm_radar.pdf",device = cairo_pdf,width =6, height =6)


##bar图
data = data2_studies_carbon[,c("year_segm", "phase_done")]
data = data %>%
  group_by(year_segm,phase_done) %>%
  summarize(count=n())
factor_order = c("Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3", "Not Applicable")
data$phase_done = factor(data$phase_done, levels = factor_order)

ggplot(data, aes(fill = phase_done, y = count, x = year_segm)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  scale_fill_jco() +
  # 设置坐标轴名称
  labs(x = "Year", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 0,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))+   # 使用Arial字体
  labs(fill = "Phase")

ggsave("Carbon_Phase_Yearsegm_bar.pdf",device = cairo_pdf,width =4, height =3)


######################### For condition in Carbon and Proton ###################

load(file = "data2/data2_conditions_carbon.rdata")
load(file = "data2/data2_conditions_proton.rdata")

data2_conditions_carbon$ct_type <- "Carbon"
data2_conditions_proton$ct_type <- "Proton"

data2_conditons = rbind(data2_conditions_carbon,data2_conditions_proton)

data = data2_conditons[, c("cancer_type_v1", "ct_type")]
data = data %>%
  group_by(cancer_type_v1,ct_type) %>%
  summarize(count=n())

ggplot(data, aes(fill = ct_type, y = count, x = cancer_type_v1)) +
  geom_bar(position = "stack", stat = "identity")+ 
  theme_classic() +
  scale_fill_jco() +
  # 设置坐标轴名称
  labs(x = "Cancer Type", y = "Count") +
  theme(axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))   # 使用Arial字体
  #labs(fill = "C")

ggsave("RAType_conditions_bar.pdf",device = cairo_pdf,width =5, height =3)


### pie

data = data2_conditions_carbon[, c("cancer_type_v1", "ct_type")]
data = data %>%
  group_by(cancer_type_v1,ct_type) %>%
  summarize(count=n())

ggplot(data,
       aes(x="", y = count, fill = cancer_type_v1)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0) + 
  scale_fill_jco()+
  theme(panel.background = element_blank())
ggsave("Carbon_conditions_pie.pdf",device = cairo_pdf,width =5, height =3)


data = data2_conditions_proton[, c("cancer_type_v1", "ct_type")]
data = data %>%
  group_by(cancer_type_v1,ct_type) %>%
  summarize(count=n())

ggplot(data,
       aes(x="", y = count, fill = cancer_type_v1)) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0) + 
  scale_fill_jco()+
  theme(panel.background = element_blank())
ggsave("Proton_conditions_pie.pdf",device = cairo_pdf,width =5, height =3)


############################## TABLE ONE  PROTON #######################################
library(dplyr)
load(file = "data2/data2_conditions_proton.rdata")
load(file = "data2/data2_designs_proton.rdata")
load(file = "data2/data2_eligibilities_proton.rdata")
load(file = "data2/data2_studies_proton.rdata")
load(file = "data2/data2_countries_proton.rdata")
load(file = "data2/data2_facilities_proton.rdata")
load(file = "data2/data2_study_references_proton.rdata")

data = merge(data2_studies_proton, data2_designs_proton, all.x = T)
data = merge(data, data2_countries_proton, all.x = T)
data$Region[is.na(data$Region)] = "Other/NA"
data = select(data, -name)
data = merge(data, data2_conditions_proton, all.x = T)
data = select(data, -name)
data = merge(data, data2_eligibilities_proton, all.x = T)
data = merge(data, data2_facilities_proton, all.x = T)
data = merge(data, data2_study_references_proton, all.x = T)

library(tableone)
## 定义需要纳入的变量
### 以year_segm作为分组变量，不加入vars中
dput(names(data))
## copy过来删减
vars = c("Recruitment_status",
         "phase_done", "enrollment_segm", "source_class", 
         "arms_segm", "has_dmc", 
         "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
         "excludes_children", "excludes_elderly", 
         "No_of_geographic_regions_segm", 
         "No_of_References_segm")
 
## 指定分类变量
factorvars = c("Recruitment_status",
               "phase_done", "enrollment_segm", "source_class", 
               "arms_segm", "has_dmc", 
               "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
               "excludes_children", "excludes_elderly", 
               "No_of_geographic_regions_segm", 
               "No_of_References_segm")

## 创建初步表格并查看

year_tableone = CreateTableOne(vars = vars,
                               strata = "year_segm",
                               data = data,
                               factorVars = factorvars)

exactvars = factorvars

output_year_table1 = print(x = year_tableone, #指定表格
                       #contDigits = 1, #连续变量保留1位小数
                       #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                       #nonnormal = nonnormalvars, #指定非正态连续变量
                       #exact = exactvars,  #指定需要Fisher确切法统计的变量
                       showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                       noSpaces = TRUE, #删除用于对齐的空格
                       printToggle = FALSE) #不展示输出结果

write.csv(output_year_table1, file = "Results/year_tableone_proton.csv")


############################## TABLE ONE  CARBON #######################################

library(dplyr)
load(file = "data2/data2_conditions_carbon.rdata")
load(file = "data2/data2_designs_carbon.rdata")
load(file = "data2/data2_eligibilities_carbon.rdata")
load(file = "data2/data2_studies_carbon.rdata")
load(file = "data2/data2_countries_carbon.rdata")
load(file = "data2/data2_facilities_carbon.rdata")
load(file = "data2/data2_study_references_carbon.rdata")

data = merge(data2_studies_carbon, data2_designs_carbon, all.x = T)
data = merge(data, data2_countries_carbon, all.x = T)
data$Region[is.na(data$Region)] = "Other/NA"
data = select(data, -name)
data = merge(data, data2_conditions_carbon, all.x = T)
data = select(data, -name)
data = merge(data, data2_eligibilities_carbon, all.x = T)
data = merge(data, data2_facilities_carbon, all.x = T)
data = merge(data, data2_study_references_carbon, all.x = T)

library(tableone)
## 定义需要纳入的变量
### 以year_segm作为分组变量，不加入vars中
dput(names(data))
## copy过来删减
vars = c("Recruitment_status",
         "phase_done", "enrollment_segm", "source_class", 
         "arms_segm", "has_dmc", 
         "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
         "excludes_children", "excludes_elderly", 
         "No_of_geographic_regions_segm", 
         "No_of_References_segm")

## 指定分类变量
factorvars = c("Recruitment_status",
               "phase_done", "enrollment_segm", "source_class", 
               "arms_segm", "has_dmc", 
               "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
               "excludes_children", "excludes_elderly", 
               "No_of_geographic_regions_segm", 
               "No_of_References_segm")

## 创建初步表格并查看

year_tableone = CreateTableOne(vars = vars,
                               strata = "year_segm",
                               data = data,
                               factorVars = factorvars)

exactvars = factorvars

output_year_table1 = print(x = year_tableone, #指定表格
                           #contDigits = 1, #连续变量保留1位小数
                           #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                           #nonnormal = nonnormalvars, #指定非正态连续变量
                           #exact = exactvars,  #指定需要Fisher确切法统计的变量
                           showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                           noSpaces = TRUE, #删除用于对齐的空格
                           printToggle = FALSE) #不展示输出结果

write.csv(output_year_table1, file = "Results/year_tableone_carbon.csv")


############################## TABLE ONE  CARBON VS PROTON #######################################
data_carbon = data
rm(data)

load(file = "data2/data2_conditions_proton.rdata")
load(file = "data2/data2_designs_proton.rdata")
load(file = "data2/data2_eligibilities_proton.rdata")
load(file = "data2/data2_studies_proton.rdata")
load(file = "data2/data2_countries_proton.rdata")
load(file = "data2/data2_facilities_proton.rdata")
load(file = "data2/data2_study_references_proton.rdata")

data = merge(data2_studies_proton, data2_designs_proton, all.x = T)
data = merge(data, data2_countries_proton, all.x = T)
data$Region[is.na(data$Region)] = "Other/NA"
data = select(data, -name)
data = merge(data, data2_conditions_proton, all.x = T)
data = select(data, -name)
data = merge(data, data2_eligibilities_proton, all.x = T)
data = merge(data, data2_facilities_proton, all.x = T)
data = merge(data, data2_study_references_proton, all.x = T)

data_proton = data
rm(data)

data_carbon$ct_type <- "carbon"
data_proton$ct_type <- "proton"
data_both = rbind(data_carbon, data_proton)
save(data_carbon, file = "data3/data_carbon.rdata")
save(data_proton, file = "data3/data_proton.rdata")
save(data_both, file = "data3/data_both.rdata")

data=data_both

library(tableone)
## 定义需要纳入的变量
### 以year_segm作为分组变量，不加入vars中
dput(names(data))
## copy过来删减
vars = c("year_segm", "Recruitment_status",
         "phase_done", "enrollment_segm", "source_class", 
         "arms_segm", "has_dmc", 
         "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
         "excludes_children", "excludes_elderly", 
         "No_of_geographic_regions_segm", 
         "No_of_References_segm")

## 指定分类变量
factorvars = c("year_segm", "Recruitment_status",
               "phase_done", "enrollment_segm", "source_class", 
               "arms_segm", "has_dmc", 
               "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
               "excludes_children", "excludes_elderly", 
               "No_of_geographic_regions_segm", 
               "No_of_References_segm")



## 创建初步表格并查看

ct_type_tableone = CreateTableOne(vars = vars,
                               strata = "ct_type",
                               data = data,
                               factorVars = factorvars)

exactvars = factorvars

output_ct_type_table1 = print(x = ct_type_tableone, #指定表格
                           #contDigits = 1, #连续变量保留1位小数
                           #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                           #nonnormal = nonnormalvars, #指定非正态连续变量
                           #exact = exactvars,  #指定需要Fisher确切法统计的变量
                           showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                           noSpaces = TRUE, #删除用于对齐的空格
                           printToggle = FALSE) #不展示输出结果

write.csv(output_year_table1, file = "Results/ct_type_tableone_carbon.csv")


############################## TABLE ONE  BOTH in yearsegm #######################################
library(dplyr)
library(tableone)

load("data3/data_both.rdata")


data=data_both
## 定义需要纳入的变量
### 以year_segm作为分组变量，不加入vars中
dput(names(data))
## copy过来删减
vars = c("ct_type","Recruitment_status",
         "phase_done", "enrollment_segm", "source_class", 
         "arms_segm", "has_dmc", 
         "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
         "excludes_children", "excludes_elderly", 
         "No_of_geographic_regions_segm", 
         "No_of_References_segm")

## 指定分类变量
factorvars = c("ct_type","Recruitment_status",
               "phase_done", "enrollment_segm", "source_class", 
               "arms_segm", "has_dmc", 
               "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
               "excludes_children", "excludes_elderly", 
               "No_of_geographic_regions_segm", 
               "No_of_References_segm")



## 创建初步表格并查看

ALL_yearsegm_tableone = CreateTableOne(vars = vars,
                                  strata = "year_segm",
                                  data = data,
                                  factorVars = factorvars)

#exactvars = factorvars

output_ALL_yearsegm_table1 = print(x = ALL_yearsegm_tableone, #指定表格
                              #contDigits = 1, #连续变量保留1位小数
                              #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                              #nonnormal = nonnormalvars, #指定非正态连续变量
                              #exact = exactvars,  #指定需要Fisher确切法统计的变量
                              showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                              noSpaces = TRUE, #删除用于对齐的空格
                              printToggle = FALSE) #不展示输出结果

write.csv(output_ALL_yearsegm_table1, file = "Results/ALL_yearsegm_tableone_carbon.csv")


############################## TABLE ONE  Region #######################################
load("data3/data_proton.rdata")
load("data3/data_carbon.rdata")
load("data3/data_both.rdata")

library(dplyr)
library(tableone)

### both
data=data_both
vars = c("year_segm", "ct_type","Recruitment_status",
         "phase_done", "enrollment_segm", "source_class", 
         "arms_segm", "has_dmc", 
         "allocation", "intervention_model", "masking", "cancer_type_v1", "gender", 
         "excludes_children", "excludes_elderly", 
         "No_of_geographic_regions_segm", 
         "No_of_References_segm")
factorvars = c("year_segm", "ct_type","Recruitment_status",
               "phase_done", "enrollment_segm", "source_class", 
               "arms_segm", "has_dmc", 
               "allocation", "intervention_model", "masking", "cancer_type_v1", "gender", 
               "excludes_children", "excludes_elderly", 
               "No_of_geographic_regions_segm", 
               "No_of_References_segm")

ALL_region_both_tableone = CreateTableOne(vars = vars,
                                       strata = "Region",
                                       data = data,
                                       factorVars = factorvars)

output_region_both_table1 = print(x = ALL_region_both_tableone, #指定表格
                              #contDigits = 1, #连续变量保留1位小数
                              #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                              #nonnormal = nonnormalvars, #指定非正态连续变量
                              #exact = exactvars,  #指定需要Fisher确切法统计的变量
                              showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                              noSpaces = TRUE, #删除用于对齐的空格
                              printToggle = FALSE) #不展示输出结果

write.csv(output_region_both_table1, file = "Results/region_tableone_both.csv")

### Proton
data=data_proton
vars = c("year_segm","Recruitment_status",
         "phase_done", "enrollment_segm", "source_class", 
         "arms_segm", "has_dmc", 
         "allocation", "intervention_model", "masking", "cancer_type_v1", "gender", 
         "excludes_children", "excludes_elderly", 
         "No_of_geographic_regions_segm", 
         "No_of_References_segm")
factorvars = c("year_segm","Recruitment_status",
               "phase_done", "enrollment_segm", "source_class", 
               "arms_segm", "has_dmc", 
               "allocation", "intervention_model", "masking", "cancer_type_v1", "gender", 
               "excludes_children", "excludes_elderly", 
               "No_of_geographic_regions_segm", 
               "No_of_References_segm")

ALL_region_proton_tableone = CreateTableOne(vars = vars,
                                          strata = "Region",
                                          data = data,
                                          factorVars = factorvars)

output_region_proton_table1 = print(x = ALL_region_proton_tableone, #指定表格
                                  #contDigits = 1, #连续变量保留1位小数
                                  #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                  #nonnormal = nonnormalvars, #指定非正态连续变量
                                  #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                  showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                  noSpaces = TRUE, #删除用于对齐的空格
                                  printToggle = FALSE) #不展示输出结果

write.csv(output_region_proton_table1, file = "Results/region_tableone_proton.csv")

### Carbon
data=data_carbon
vars = c("year_segm","Recruitment_status",
         "phase_done", "enrollment_segm", "source_class", 
         "arms_segm", "has_dmc", 
         "allocation", "intervention_model", "masking", "cancer_type_v1", "gender", 
         "excludes_children", "excludes_elderly", 
         "No_of_geographic_regions_segm", 
         "No_of_References_segm")
factorvars = c("year_segm","Recruitment_status",
               "phase_done", "enrollment_segm", "source_class", 
               "arms_segm", "has_dmc", 
               "allocation", "intervention_model", "masking", "cancer_type_v1", "gender", 
               "excludes_children", "excludes_elderly", 
               "No_of_geographic_regions_segm", 
               "No_of_References_segm")

ALL_region_carbon_tableone = CreateTableOne(vars = vars,
                                            strata = "Region",
                                            data = data,
                                            factorVars = factorvars)

output_region_carbon_table1 = print(x = ALL_region_carbon_tableone, #指定表格
                                    #contDigits = 1, #连续变量保留1位小数
                                    #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                    #nonnormal = nonnormalvars, #指定非正态连续变量
                                    #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                    showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                    noSpaces = TRUE, #删除用于对齐的空格
                                    printToggle = FALSE) #不展示输出结果

write.csv(output_region_carbon_table1, file = "Results/region_tableone_carbon.csv")


############################## TABLE ONE  status #######################################

### both
data=data_both
vars = c("year_segm", "ct_type",
         "phase_done", "enrollment_segm", "source_class", 
         "arms_segm", "has_dmc", 
         "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
         "excludes_children", "excludes_elderly", 
         "No_of_geographic_regions_segm", 
         "No_of_References_segm")
factorvars = c("year_segm", "ct_type",
               "phase_done", "enrollment_segm", "source_class", 
               "arms_segm", "has_dmc", 
               "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
               "excludes_children", "excludes_elderly", 
               "No_of_geographic_regions_segm", 
               "No_of_References_segm")

ALL_status_both_tableone = CreateTableOne(vars = vars,
                                          strata = "Recruitment_status",
                                          data = data,
                                          factorVars = factorvars)

output_status_both_table1 = print(x = ALL_status_both_tableone, #指定表格
                                  #contDigits = 1, #连续变量保留1位小数
                                  #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                  #nonnormal = nonnormalvars, #指定非正态连续变量
                                  #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                  showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                  noSpaces = TRUE, #删除用于对齐的空格
                                  printToggle = FALSE) #不展示输出结果

write.csv(output_status_both_table1, file = "Results/status_tableone_both.csv")

### Proton
data=data_proton
vars = c("year_segm",
         "phase_done", "enrollment_segm", "source_class", 
         "arms_segm", "has_dmc", 
         "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
         "excludes_children", "excludes_elderly", 
         "No_of_geographic_regions_segm", 
         "No_of_References_segm")
factorvars = c("year_segm",
               "phase_done", "enrollment_segm", "source_class", 
               "arms_segm", "has_dmc", 
               "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
               "excludes_children", "excludes_elderly", 
               "No_of_geographic_regions_segm", 
               "No_of_References_segm")

ALL_status_proton_tableone = CreateTableOne(vars = vars,
                                            strata = "Recruitment_status",
                                            data = data,
                                            factorVars = factorvars)

output_status_proton_table1 = print(x = ALL_status_proton_tableone, #指定表格
                                    #contDigits = 1, #连续变量保留1位小数
                                    #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                    #nonnormal = nonnormalvars, #指定非正态连续变量
                                    #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                    showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                    noSpaces = TRUE, #删除用于对齐的空格
                                    printToggle = FALSE) #不展示输出结果

write.csv(output_status_proton_table1, file = "Results/status_tableone_proton.csv")

### Carbon
data=data_carbon
vars = c("year_segm",
         "phase_done", "enrollment_segm", "source_class", 
         "arms_segm", "has_dmc", 
         "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
         "excludes_children", "excludes_elderly", 
         "No_of_geographic_regions_segm", 
         "No_of_References_segm")
factorvars = c("year_segm",
               "phase_done", "enrollment_segm", "source_class", 
               "arms_segm", "has_dmc", 
               "allocation", "intervention_model", "masking", "Region", "cancer_type_v1", "gender", 
               "excludes_children", "excludes_elderly", 
               "No_of_geographic_regions_segm", 
               "No_of_References_segm")

ALL_status_carbon_tableone = CreateTableOne(vars = vars,
                                            strata = "Recruitment_status",
                                            data = data,
                                            factorVars = factorvars)

output_status_carbon_table1 = print(x = ALL_status_carbon_tableone, #指定表格
                                    #contDigits = 1, #连续变量保留1位小数
                                    #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                    #nonnormal = nonnormalvars, #指定非正态连续变量
                                    #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                    showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                    noSpaces = TRUE, #删除用于对齐的空格
                                    printToggle = FALSE) #不展示输出结果

write.csv(output_status_carbon_table1, file = "Results/status_tableone_carbon.csv")
