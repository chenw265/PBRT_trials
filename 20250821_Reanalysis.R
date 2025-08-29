rm(list = ls())
gc()


mycolor = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
### Phase III trials statistics ####
library(dplyr)
library(tableone)
#### TABLE ONE --- all vars ####

load("data3/data_all.rdata")
load("data3/data_both_who.rdata")  # 补充详细的样本量
load("data3/data_both_ct.rdata")   # 补充详细的样本量

# 选择需要的列,并重命名
numc = data_both_ct %>%
  select("Id", "Enrollment")

nump = data_both_who %>%
  select("Id", "Target_size")%>%
  rename(Enrollment = Target_size)

num_data = rbind(numc, nump)

# 合并样本量
data_all = left_join(data_all, num_data, by = "Id")

# 去掉完全重复的行
data_all = data_all %>%
  distinct()

data_all$Enrollment = as.numeric(data_all$Enrollment)


# 增加一列变量phase_re, 将data_all中的Phase列转换为Phase_re，其中Phase I和Phase II转换为Phase I/II，Phase III和Phase II/III转换为Phase III保持不变

data_all$Phase_re <- case_when(
  data_all$Phase_done %in% c("Phase 1", "Phase 2", "Phase 1/Phase 2") ~ "Phase 1~2",
  data_all$Phase_done %in% c("Phase 3", "Phase 2/Phase 3") ~ "Phase 2~3",
  TRUE ~ data_all$Phase_done
)

data_carbon = data_all %>%
  filter(Ct_type == "carbon") %>%
  filter(Phase_re != "Not Applicable")

data_proton = data_all %>%
  filter(Ct_type == "proton") %>%
  filter(Phase_re != "Not Applicable")

##### carbon ####
data = data_carbon
dput(names(data))
vars = c("Year_segm", "Registration_before_participant_enrollment", 
         "Recruitment_status", "Allocation", "Masking", 
         "Excludes_children", "Excludes_elderly", "Gender", "Enrollment_segm", "Enrollment",
         "Source_class", "Platform")
factorvars = c("Year_segm", "Registration_before_participant_enrollment", 
               "Recruitment_status", "Allocation", "Masking", 
               "Excludes_children", "Excludes_elderly", "Gender", "Enrollment_segm",
               "Source_class", "Platform")
# shapiro.test(data_all$Enrollment) , P小于0.05,属于非正太分布
nonnormalvars = c("Enrollment")


# 创建初步表格并查看
ALL_carbon_tableone = CreateTableOne(vars = vars,
                                       strata = "Phase_re",
                                       data = data,
                                       factorVars = factorvars)
exactvars = factorvars
output = print(x = ALL_carbon_tableone, #指定表格
                                   #contDigits = 1, #连续变量保留1位小数
                                   #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                                   nonnormal = nonnormalvars, #指定非正态连续变量
                                   #exact = exactvars,  #指定需要Fisher确切法统计的变量
                                   showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
                                   noSpaces = TRUE, #删除用于对齐的空格
                                   printToggle = FALSE) #不展示输出结果

write.csv(output, file = "Results2/ALL_carbon_tableone.csv")


library(CBCgrps)
vars = c("Year_segm", "Registration_before_participant_enrollment", 
         "Recruitment_status", "Allocation", "Masking", 
         "Excludes_children", "Excludes_elderly", "Gender", "Enrollment_segm", "Enrollment",
         "Source_class", "Platform")
tab1 = twogrps(data,
               gvar = "Phase_re",
               varlist = vars,
               ShowStatistic = T)


output = tab1$Table
write.csv(output, file = "Results2/ALL_carbon_tableone_CBCgrps.csv")


##### proton ####
data = data_proton
dput(names(data))
vars = c("Year_segm", "Registration_before_participant_enrollment", 
         "Recruitment_status", "Allocation", "Masking", 
         "Excludes_children", "Excludes_elderly", "Gender", "Enrollment_segm", "Enrollment",
         "Source_class", "Platform")
factorvars = c("Year_segm", "Registration_before_participant_enrollment", 
               "Recruitment_status", "Allocation", "Masking", 
               "Excludes_children", "Excludes_elderly", "Gender", "Enrollment_segm",
               "Source_class", "Platform")
# shapiro.test(data_all$Enrollment) , P小于0.05,属于非正太分布
nonnormalvars = c("Enrollment")

# 创建初步表格并查看
ALL_proton_tableone = CreateTableOne(vars = vars,
                                     strata = "Phase_re",
                                     data = data,
                                     factorVars = factorvars)
exactvars = factorvars
output = print(x = ALL_proton_tableone, #指定表格
               #contDigits = 1, #连续变量保留1位小数
               #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
               nonnormal = nonnormalvars, #指定非正态连续变量
               #exact = exactvars,  #指定需要Fisher确切法统计的变量
               showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
               noSpaces = TRUE, #删除用于对齐的空格
               printToggle = FALSE) #不展示输出结果

write.csv(output, file = "Results2/ALL_proton_tableone.csv")


library(CBCgrps)

tab1 = twogrps(data,
               gvar = "Phase_re",
               varlist = vars,
               ShowStatistic = T)

output = tab1$Table
write.csv(output, file = "Results2/ALL_proton_tableone_CBCgrps.csv")

### TABLE ONE --- countries ####
load("data3/data4_countries.rdata")
data4_countries$Phase_re <- case_when(
  data4_countries$Phase_done %in% c("Phase 1", "Phase 2", "Phase 1/Phase 2") ~ "Phase 1~2",
  data4_countries$Phase_done %in% c("Phase 3", "Phase 2/Phase 3") ~ "Phase 2~3",
  TRUE ~ data4_countries$Phase_done
)

data_carbon = data4_countries %>%
  filter(Ct_type == "carbon") %>%
  filter(Phase_re != "Not Applicable")

data_proton = data4_countries %>%
  filter(Ct_type == "proton") %>%
  filter(Phase_re != "Not Applicable")


##### carbon ####
data = data_carbon
dput(names(data))
vars = c("Region")

factorvars = vars

# 创建初步表格并查看
ALL_carbon_tableone = CreateTableOne(vars = vars,
                                     strata = "Phase_re",
                                     data = data,
                                     factorVars = factorvars)
exactvars = factorvars
output = print(x = ALL_carbon_tableone, #指定表格
               #contDigits = 1, #连续变量保留1位小数
               #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
               #nonnormal = nonnormalvars, #指定非正态连续变量
               #exact = exactvars,  #指定需要Fisher确切法统计的变量
               showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
               noSpaces = TRUE, #删除用于对齐的空格
               printToggle = FALSE) #不展示输出结果

write.csv(output, file = "Results2/ALL_carbon_tableone_regions.csv")

library(CBCgrps)
tab1 = twogrps(data,
               gvar = "Phase_re",
               varlist = vars,
               ShowStatistic = T)
output = tab1$Table
write.csv(output, file = "Results2/ALL_carbon_tableone_regions_CBCgrps.csv")





##### proton ####
data = data_proton
dput(names(data))
vars = c("Region")
factorvars = vars

# 创建初步表格并查看
ALL_proton_tableone = CreateTableOne(vars = vars,
                                     strata = "Phase_re",
                                     data = data,
                                     factorVars = factorvars)
exactvars = factorvars
output = print(x = ALL_proton_tableone, #指定表格
               #contDigits = 1, #连续变量保留1位小数
               #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
               #nonnormal = nonnormalvars, #指定非正态连续变量
               #exact = exactvars,  #指定需要Fisher确切法统计的变量
               showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
               noSpaces = TRUE, #删除用于对齐的空格
               printToggle = FALSE) #不展示输出结果

write.csv(output, file = "Results2/ALL_proton_tableone_regions.csv")

library(CBCgrps)
tab1 = twogrps(data,
               gvar = "Phase_re",
               varlist = vars,
               ShowStatistic = T)
output = tab1$Table

write.csv(output, file = "Results2/ALL_proton_tableone_regions_CBCgrps.csv")

### TABLE ONE --- conditions ####
load("data3/data4_conditions.rdata")
data4_conditions$Phase_re <- case_when(
  data4_conditions$Phase_done %in% c("Phase 1", "Phase 2", "Phase 1/Phase 2") ~ "Phase 1~2",
  data4_conditions$Phase_done %in% c("Phase 3", "Phase 2/Phase 3") ~ "Phase 2~3",
  TRUE ~ data4_conditions$Phase_done
)

data_carbon = data4_conditions %>%
  filter(Ct_type == "carbon") %>%
  filter(Phase_re != "Not Applicable")

data_proton = data4_conditions %>%
  filter(Ct_type == "proton") %>%
  filter(Phase_re != "Not Applicable")


##### carbon ####
data = data_carbon
dput(names(data))
vars = c("cancer_type")
factorvars = vars

# 创建初步表格并查看
ALL_carbon_tableone = CreateTableOne(vars = vars,
                                     strata = "Phase_re",
                                     data = data,
                                     factorVars = factorvars)
exactvars = factorvars
output = print(x = ALL_carbon_tableone, #指定表格
               #contDigits = 1, #连续变量保留1位小数
               #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
               #nonnormal = nonnormalvars, #指定非正态连续变量
               #exact = exactvars,  #指定需要Fisher确切法统计的变量
               showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
               noSpaces = TRUE, #删除用于对齐的空格
               printToggle = FALSE) #不展示输出结果

write.csv(output, file = "Results2/ALL_carbon_tableone_conditions.csv")

library(CBCgrps)
tab1 = twogrps(data,
               gvar = "Phase_re",
               varlist = vars,
               ShowStatistic = T)
output = tab1$Table
write.csv(output, file = "Results2/ALL_carbon_tableone_conditions_CBCgrps.csv")

##### proton ####
data = data_proton
dput(names(data))
vars = c("cancer_type")
factorvars = vars

# 创建初步表格并查看
ALL_proton_tableone = CreateTableOne(vars = vars,
                                     strata = "Phase_re",
                                     data = data,
                                     factorVars = factorvars)
exactvars = factorvars
output = print(x = ALL_proton_tableone, #指定表格
               #contDigits = 1, #连续变量保留1位小数
               #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
               #nonnormal = nonnormalvars, #指定非正态连续变量
               #exact = exactvars,  #指定需要Fisher确切法统计的变量
               showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
               noSpaces = TRUE, #删除用于对齐的空格
               printToggle = FALSE) #不展示输出结果

write.csv(output, file = "Results2/ALL_proton_tableone_conditions.csv")


library(CBCgrps)
tab1 = twogrps(data,
               gvar = "Phase_re",
               varlist = vars,
               ShowStatistic = T)
output = tab1$Table

write.csv(output, file = "Results2/ALL_proton_tableone_conditions_CBCgrps.csv")

### Weighted_mean sample size ####
rm(list = ls())
gc()

library(dplyr)

load("data3/data_both_who.rdata")  # 补充详细的样本量
load("data3/data_both_ct.rdata")   # 补充详细的样本量

# 选择需要的列,并重命名
numc = data_both_ct %>%
  select("Id", "Enrollment")

numw = data_both_who %>%
  select("Id", "Target_size")%>%
  rename(Enrollment = Target_size)

num_data = rbind(numc, numw)


load("data3/data4_countries.rdata")

# 合并样本量
data4_countries = left_join(data4_countries, num_data, by = "Id")

# 去掉完全重复的行
data4_countries = data4_countries %>%
  distinct()

# 对enrollment数值化
data_carbon = data4_countries %>%
  filter(Ct_type == "carbon") %>%
  select(Enrollment, Region)

data_carbon$Enrollment = as.numeric(data_carbon$Enrollment)

# 统计各地区临床试验数量和样本量总和
region_stats_carbon = data_carbon %>%
  group_by(Region) %>%
  summarise(trial_count = n(),
            total_enrollment = sum(Enrollment, na.rm = TRUE))

data_proton = data4_countries %>%
  filter(Ct_type == "proton") %>%
  select(Enrollment, Region)
data_proton$Enrollment = as.numeric(data_proton$Enrollment)
# 统计各地区临床试验数量和样本量总和
region_stats_proton = data_proton %>%
  group_by(Region) %>%
  summarise(trial_count = n(),
            total_enrollment = sum(Enrollment, na.rm = TRUE))

# 计算加权平均样本量
region_stats_carbon <- region_stats_carbon %>%
  mutate(weighted_mean_enrollment = sum(total_enrollment) / sum(trial_count))

region_stats_proton <- region_stats_proton %>%
  mutate(weighted_mean_enrollment = sum(total_enrollment) / sum(trial_count))


### Shannon index ####
library(vegan)

# 计算Shannon指数
carbon_data = region_stats_carbon$trial_count
names(carbon_data) = region_stats_carbon$Region
shannon_index_carbon = diversity(carbon_data, index = "shannon")

proton_data = region_stats_proton$trial_count
names(proton_data) = region_stats_proton$Region
shannon_index_proton = diversity(proton_data, index = "shannon")

region_stats_carbon$Shannon_index = shannon_index_carbon
region_stats_proton$Shannon_index = shannon_index_proton

write.csv(region_stats_carbon, file = "Results2/region_stats_carbon.csv")
write.csv(region_stats_proton, file = "Results2/region_stats_proton.csv")


### 随年份的动态分布 Weighted mean sample size & Shannon index ####
rm(list = ls())
gc()

library(dplyr)
library(vegan)

load("data3/data_both_who.rdata")  # 补充详细的样本量
load("data3/data_both_ct.rdata")   # 补充详细的样本量
# 选择需要的列,并重命名
numc = data_both_ct %>%
  select("Id", "Enrollment")
numw = data_both_who %>%
  select("Id", "Target_size") %>%
  rename(Enrollment = Target_size)

num_data = rbind(numc, numw)
load("data3/data4_countries.rdata")

# 合并样本量

# 合并样本量
data4_countries = left_join(data4_countries, num_data, by = "Id")

# 去掉完全重复的行
data4_countries = data4_countries %>%
  distinct()

# 对enrollment和Year_segm数值化
data_carbon = data4_countries %>%
  filter(Ct_type == "carbon") %>%
  select(Enrollment, Year_segm, Region)
data_carbon$Enrollment = as.numeric(data_carbon$Enrollment)
# data_carbon$Year_segm = as.numeric(data_carbon$Year_segm)

# 统计各年份各地区临床试验数量和样本量总和
region_year_stats_carbon = data_carbon %>%
  group_by(Year_segm, Region) %>%
  summarise(trial_count = n(),
            total_enrollment = sum(Enrollment, na.rm = TRUE)) %>%
  ungroup()

# 每个year_segm都有4个region的记录,如果某个region在某个year_segm没有记录,则补0
all_years = unique(region_year_stats_carbon$Year_segm)
all_regions = unique(region_year_stats_carbon$Region)
all_combinations = expand.grid(Year_segm = all_years, Region = all_regions)
region_year_stats_carbon = left_join(all_combinations, region_year_stats_carbon, by = c("Year_segm", "Region"))
region_year_stats_carbon[is.na(region_year_stats_carbon)] <- 0
region_year_stats_carbon = region_year_stats_carbon %>%
  arrange(Year_segm, Region)

# 计算每年加权平均样本量和Shannon指数
yearly_stats_carbon = region_year_stats_carbon %>%
  group_by(Year_segm) %>%
  summarise(weighted_mean_enrollment = sum(total_enrollment) / sum(trial_count),
            Shannon_index = diversity(trial_count, index = "shannon")) %>%
  ungroup()
write.csv(yearly_stats_carbon, file = "Results2/yearly_region_stats_carbon.csv")

# proton
data_proton = data4_countries %>%
  filter(Ct_type == "proton") %>%
  select(Enrollment, Year_segm, Region)
data_proton$Enrollment = as.numeric(data_proton$Enrollment)
#data_proton$Year_segm = as.numeric(data_proton$Year_segm)

# 统计各年份各地区临床试验数量和样本量总和
region_year_stats_proton = data_proton %>%
  group_by(Year_segm, Region) %>%
  summarise(trial_count = n(),
            total_enrollment = sum(Enrollment, na.rm = TRUE)) %>%
  ungroup()

# 每个year_segm都有4个region的记录,如果某个region在某个year_segm没有记录,则补0
all_years = unique(region_year_stats_proton$Year_segm)
all_regions = unique(region_year_stats_proton$Region)
all_combinations = expand.grid(Year_segm = all_years, Region = all_regions)
region_year_stats_proton = left_join(all_combinations, region_year_stats_proton, by = c("Year_segm", "Region"))
region_year_stats_proton[is.na(region_year_stats_proton)] <- 0
region_year_stats_proton = region_year_stats_proton %>%
  arrange(Year_segm, Region)

# 计算每年加权平均样本量和Shannon指数
yearly_stats_proton = region_year_stats_proton %>%
  group_by(Year_segm) %>%
  summarise(weighted_mean_enrollment = sum(total_enrollment) / sum(trial_count),
            Shannon_index = diversity(trial_count, index = "shannon")) %>%
  ungroup()
write.csv(yearly_stats_proton, file = "Results2/yearly_region_stats_proton.csv")

#### 绘图-carbon和proton shannon index的这些图在同一幅图 ####
library(ggplot2)

ggplot()+
  geom_path(data = yearly_stats_carbon, aes(x = Year_segm, y = Shannon_index, color = "carbon", group = 1), linewidth = 1.2) +
  geom_point(data = yearly_stats_carbon, aes(x = Year_segm, y = Shannon_index, color = "carbon"), size = 3) +
  geom_path(data = yearly_stats_proton, aes(x = Year_segm, y = Shannon_index, color = "proton", group = 1), linewidth = 1.2) +
  geom_point(data = yearly_stats_proton, aes(x = Year_segm, y = Shannon_index, color = "proton"), size = 3) +
  geom_bar(data = yearly_stats_carbon, aes(x = Year_segm, y = Shannon_index), stat = "identity", fill = "#FC8D62", alpha = 0.25) +
  geom_bar(data = yearly_stats_proton, aes(x = Year_segm, y = Shannon_index), stat = "identity", fill = "#8DA0CB", alpha = 0.25) +
  scale_color_manual(values = c("carbon" = "#FC8D62", "proton" = "#8DA0CB"))+
  theme_bw()+
  theme(
        axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(#angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))   # 使用Arial字体

ggsave("Results2/yearly_shannon_index_carbon_proton.pdf", width = 4, height = 4)


ggplot()+
  geom_path(data = yearly_stats_carbon, aes(x = Year_segm, y = weighted_mean_enrollment, color = "carbon", group = 1), linewidth = 1.2) +
  geom_point(data = yearly_stats_carbon, aes(x = Year_segm, y = weighted_mean_enrollment, color = "carbon"), size = 3) +
  geom_path(data = yearly_stats_proton, aes(x = Year_segm, y = weighted_mean_enrollment, color = "proton", group = 1), linewidth = 1.2) +
  geom_point(data = yearly_stats_proton, aes(x = Year_segm, y = weighted_mean_enrollment, color = "proton"), size = 3) +
  geom_bar(data = yearly_stats_carbon, aes(x = Year_segm, y = weighted_mean_enrollment), stat = "identity", fill = "#FC8D62", alpha = 0.25) +
  geom_bar(data = yearly_stats_proton, aes(x = Year_segm, y = weighted_mean_enrollment), stat = "identity", fill = "#8DA0CB", alpha = 0.25) +
  scale_color_manual(values = c("carbon" = "#FC8D62", "proton" = "#8DA0CB"))+
  theme_bw()+
  theme(
        axis.title = element_text(size = 9, face = "bold", family = "sans"),
        axis.text.x = element_text(#angle = 90,
                                   size = 9,
                                   colour = "black",
                                   family = "sans",
                                   face = "bold"),
        axis.text.y = element_text(face = "bold",
                                   size = 9,
                                   colour = "black",
                                   family = "sans"))   # 使用Arial字体
ggsave("Results2/yearly_weighted_mean_enrollment_carbon_proton.pdf", width = 4, height = 4)


### 计算各region的试验数量的年增长率 ####
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
mycolor = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")

load("data3/data_both_who.rdata")  # 补充详细的样本量
load("data3/data_both_ct.rdata")   # 补充详细的样本量
# 选择需要的列,并重命名
numc = data_both_ct %>%
  select("Id", "Enrollment")
numw = data_both_who %>%
  select("Id", "Target_size") %>%
  rename(Enrollment = Target_size)

num_data = rbind(numc, numw)
load("data3/data4_countries.rdata")

# 合并样本量

# 合并样本量
data4_countries = left_join(data4_countries, num_data, by = "Id")

# 去掉完全重复的行
data4_countries = data4_countries %>%
  distinct()

# 对enrollment和Year_segm数值化
data_carbon = data4_countries %>%
  filter(Ct_type == "carbon") %>%
  select(Enrollment, Year_registration, Region)
data_carbon$Enrollment = as.numeric(data_carbon$Enrollment)
data_carbon$Year_registration = as.numeric(data_carbon$Year_registration)

# 统计各年份各地区临床试验数量
data_carbon = data_carbon %>%
  group_by(Year_registration, Region) %>%
  summarise(trial_count = n()) %>%
  ungroup()

# 每个year_segm都有4个region的记录,如果某个region在某个year_segm没有记录,则补0
all_years = unique(data_carbon$Year_registration)
all_regions = unique(data_carbon$Region)
all_combinations = expand.grid(Year_registration = all_years, Region = all_regions)
data_carbon = left_join(all_combinations, data_carbon, by = c("Year_registration", "Region"))
data_carbon[is.na(data_carbon)] <- 0

data_carbon = data_carbon %>%
  arrange(Region, Year_registration)

# 计算年增长率
data_carbon_growth = data_carbon %>%
  group_by(Region) %>%
  mutate(growth_rate = (trial_count - lag(trial_count)) / lag(trial_count) * 100) %>%
  ungroup()

# 处理缺失值和无限值,分布赋予0和NA
data_carbon_growth$growth_rate[is.infinite(data_carbon_growth$growth_rate)] <- NA
data_carbon_growth$growth_rate[is.na(data_carbon_growth$growth_rate)] <- 0

# 计算绝对增量
data_carbon_growth = data_carbon_growth %>%
  group_by(Region) %>%
  mutate(cumulative_growth = cumsum(trial_count)) %>%
  ungroup()

# 计算复合年增长率(CAGR)
data_carbon_cagr = data_carbon_growth %>%
  group_by(Region) %>%
  summarise(
    cagr = (last(trial_count) / first(trial_count))^(1 / (last(Year_registration) - first(Year_registration))) - 1) %>%
  ungroup()

factor_order = c("Europe", "North America","Asia", "Other/NA")
data_carbon_growth$Region = factor(data_carbon_growth$Region, levels = factor_order)

# 绘制年增长率图
ggplot(data_carbon_growth, aes(x = Year_registration, y = growth_rate, color = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual Growth Rate of Clinical Trials by Region (Carbon)",
       x = "Year",
       y = "Growth Rate (%)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 9, face = "bold", family = "sans"),
    axis.text.x = element_text(size = 9, colour = "black", family = "sans", face = "bold"),
    axis.text.y = element_text(face = "bold", size = 9, colour = "black", family = "sans")
  ) +
  scale_color_manual(values = mycolor)
ggsave("Results2/carbon_region_growth_rate.pdf", width = 6, height = 4)

# 绘制累积增量图
ggplot(data_carbon_growth, aes(x = Year_registration, y = cumulative_growth, color = Region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Year",
       y = "Cumulative Trials") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 9, face = "bold", family = "sans"),
    axis.text.x = element_text(size = 9, colour = "black", family = "sans", face = "bold"),
    axis.text.y = element_text(face = "bold", size = 9, colour = "black", family = "sans")
  ) +
  scale_color_manual(values = mycolor)

ggsave("Results2/carbon_region_cumulative_growth.pdf", width = 4, height = 3)


# # 绘制CAGR图
# ggplot(data_carbon_cagr, aes(x = Region, y = cagr * 100, fill = Region)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Compound Annual Growth Rate (CAGR) of Clinical Trials by Region (Carbon)",
#        x = "Region",
#        y = "CAGR (%)") +
#   theme_minimal() +
#   theme(
#     axis.title = element_text(size = 9, face = "bold", family = "sans"),
#     axis.text.x = element_text(size = 9, colour = "black", family = "sans", face = "bold"),
#     axis.text.y = element_text(face = "bold", size = 9, colour = "black", family = "sans")
#   ) +
#   scale_fill_brewer(palette = "Set1")
# ggsave("Results2/carbon_region_cagr.pdf", width = 6, height = 4)

# proton
data_proton = data4_countries %>%
  filter(Ct_type == "proton") %>%
  select(Enrollment, Year_registration, Region)
data_proton$Enrollment = as.numeric(data_proton$Enrollment)
data_proton$Year_registration = as.numeric(data_proton$Year_registration)
# 统计各年份各地区临床试验数量
data_proton = data_proton %>%
  group_by(Year_registration, Region) %>%
  summarise(trial_count = n()) %>%
  ungroup()
# 每个year_segm都有4个region的记录,如果某个region在某个year_segm没有记录,则补0
all_years = unique(data_proton$Year_registration)
all_regions = unique(data_proton$Region)
all_combinations = expand.grid(Year_registration = all_years, Region = all_regions)
data_proton = left_join(all_combinations, data_proton, by = c("Year_registration", "Region"))
data_proton[is.na(data_proton)] <- 0
data_proton = data_proton %>%
  arrange(Region, Year_registration)

# 计算年增长率
data_proton_growth = data_proton %>%
  group_by(Region) %>%
  mutate(growth_rate = (trial_count - lag(trial_count)) / lag(trial_count) * 100) %>%
  ungroup()

# 处理缺失值和无限值,分布赋予0和NA
data_proton_growth$growth_rate[is.infinite(data_proton_growth$growth_rate)] <- NA
data_proton_growth$growth_rate[is.na(data_proton_growth$growth_rate)] <- 0

# 计算绝对增量
data_proton_growth = data_proton_growth %>%
  group_by(Region) %>%
  mutate(cumulative_growth = cumsum(trial_count)) %>%
  ungroup()


# 计算复合年增长率(CAGR)
data_proton_cagr = data_proton_growth %>%
  group_by(Region) %>%
  summarise(
    cagr = (last(trial_count) / first(trial_count))^(1 / (last(Year_registration) - first(Year_registration))) - 1) %>%
  ungroup()

factor_order = c("Europe", "North America","Asia", "Other/NA")
data_proton_growth$Region = factor(data_proton_growth$Region, levels = factor_order)

# 绘制年增长率图
ggplot(data_proton_growth, aes(x = Year_registration, y = growth_rate, color = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Annual Growth Rate of Clinical Trials by Region (Proton)",
       x = "Year",
       y = "Growth Rate (%)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 9, face = "bold", family = "sans"),
    axis.text.x = element_text(size = 9, colour = "black", family = "sans", face = "bold"),
    axis.text.y = element_text(face = "bold", size = 9, colour = "black", family = "sans")
  ) +
  scale_color_manual(values = mycolor)

ggsave("Results2/proton_region_growth_rate.pdf", width = 6, height = 4)

# 绘制累积增量图
ggplot(data_proton_growth, aes(x = Year_registration, y = cumulative_growth, color = Region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Year",
       y = "Cumulative Trials") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 9, face = "bold", family = "sans"),
    axis.text.x = element_text(size = 9, colour = "black", family = "sans", face = "bold"),
    axis.text.y = element_text(face = "bold", size = 9, colour = "black", family = "sans")
  ) +
  scale_color_manual(values = mycolor)
ggsave("Results2/proton_region_cumulative_growth.pdf", width = 4, height = 3)


##### 不区分carbon 和proton的region增长率 ####
data_all = data4_countries %>%
  select(Enrollment, Year_registration, Region)

data_all$Enrollment = as.numeric(data_all$Enrollment)
data_all$Year_registration = as.numeric(data_all$Year_registration)

# 统计各年份各地区临床试验数量
data_all = data_all %>%
  group_by(Year_registration, Region) %>%
  summarise(trial_count = n()) %>%
  ungroup()

# 每个year_segm都有4个region的记录,如果某个region在某个year_segm没有记录,则补0
all_years = unique(data_all$Year_registration)
all_regions = unique(data_all$Region)
all_combinations = expand.grid(Year_registration = all_years, Region = all_regions)
data_all = left_join(all_combinations, data_all, by = c("Year_registration", "Region"))
data_all[is.na(data_all)] <- 0
data_all = data_all %>%
  arrange(Region, Year_registration)

# 计算年增长率
data_all_growth = data_all %>%
  group_by(Region) %>%
  mutate(growth_rate = (trial_count - lag(trial_count)) / lag(trial_count) * 100) %>%
  ungroup()

# 处理缺失值和无限值,分布赋予0和NA
data_all_growth$growth_rate[is.infinite(data_all_growth$growth_rate)] <- NA
data_all_growth$growth_rate[is.na(data_all_growth$growth_rate)] <- 0

# 计算绝对增量
data_all_growth = data_all_growth %>%
  group_by(Region) %>%
  mutate(cumulative_growth = cumsum(trial_count)) %>%
  ungroup()

# 计算复合年增长率(CAGR)
data_all_cagr = data_all_growth %>%
  group_by(Region) %>%
  summarise(
    cagr = (last(trial_count) / first(trial_count))^(1 / (last(Year_registration) - first(Year_registration))) - 1) %>%
  ungroup()

factor_order = c("Europe", "North America","Asia", "Other/NA")
data_all_growth$Region = factor(data_all_growth$Region, levels = factor_order)

# 绘制年增长率图
ggplot(data_all_growth, aes(x = Year_registration, y = growth_rate, color = Region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Annual Growth Rate of Clinical Trials by Region (All)",
       x = "Year",
       y = "Growth Rate (%)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 9, face = "bold", family = "sans"),
    axis.text.x = element_text(size = 9, colour = "black", family = "sans", face = "bold"),
    axis.text.y = element_text(face = "bold", size = 9, colour = "black", family = "sans")
  ) +
  scale_color_manual(values = mycolor)

ggsave("Results2/all_region_growth_rate.pdf", width = 6, height = 4)

# 绘制累积增量图
ggplot(data_all_growth, aes(x = Year_registration, y = cumulative_growth, color = Region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Year",
       y = "Cumulative Trials") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 9, face = "bold", family = "sans"),
    axis.text.x = element_text(size = 9, colour = "black", family = "sans", face = "bold"),
    axis.text.y = element_text(face = "bold", size = 9, colour = "black", family = "sans")
  ) +
  scale_color_manual(values = mycolor)

ggsave("Results2/all_region_cumulative_growth.pdf", width = 4, height = 3)


### 总结临床试验提前终止的原因 ####
rm(list = ls())
gc()

library(dplyr)

load("data3/data_both_ct.rdata")

data = data_both_ct %>%
  filter(Recruitment_status == "Stopped Early") %>%
  select(Id, Ct_type, Year_registration, Overall_status, Recruitment_status, Why_stopped) %>%
  arrange(Ct_type, Why_stopped)

writexl::write_xlsx(data, "Results2/all_why_stopped.xlsx")


### Logistic Regression 回归对比 PBRT和XRT ####
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(broom)

#### 整合PBRT和PRT的cancer type数据 ####
data_merged = read.csv("data3/data_merged.csv")
load("data3/prt_condition.rdata")
load("data3/data4_conditions.rdata")

# 将prt_condition中的cancer_type中的Gastrointestinal更换为Digestive_System
prt_condition = prt_condition %>%
  mutate(cancer_type = ifelse(cancer_type == "Gastrointestinal", "Digestive_System", cancer_type))

# 将data_merged中的source_class中的Other更换为OTHER
data_merged = data_merged %>%
  mutate(Source_class = ifelse(Source_class == "Other", "OTHER", Source_class))
 

# 对data_merged进行去重复
## ！！！ 注意，Proton和Carbon之间的重复被删除了
data_merged = data_merged %>%
  distinct()

# 处理pbrt_condition数据,从data4将data_merged出现的id筛选出来
pbrt_condition = data4_conditions %>%
  filter(Id %in% data_merged$Id[data_merged$Type == "PBRT"]) %>%
  select(Id, cancer_type)
length(unique(pbrt_condition$Id))

# pbrt_condition中如有重复的Id,则将cancer_type设置为multi-cancer
pbrt_condition = pbrt_condition %>%
  group_by(Id) %>%
  mutate(cancer_type = ifelse(n() > 1, "multi-cancer", cancer_type)) %>%
  distinct(Id, cancer_type, .keep_all = TRUE) %>%
  ungroup()
length(unique(pbrt_condition$Id))

prt_condition = prt_condition %>%
  select(Id, cancer_type)
length(unique(prt_condition$Id))

pbrt_data = data_merged %>%
  filter(Type == "PBRT") %>%
  left_join(pbrt_condition, by = "Id") %>%
  distinct()

prt_data =  data_merged %>%
  filter(Type == "Photon") %>%
  left_join(prt_condition, by = "Id") %>%
  distinct()

data_merged = rbind(pbrt_data, prt_data)

data_merged = data_merged %>%
  mutate(Allocation = ifelse(Allocation == "N/A", "None/Not Report", Allocation))

data_merged = data_merged %>%
  mutate(Masking = ifelse(Masking == "N/A", "None (Open Label)", Masking))

data_merged = data_merged %>%
  mutate(Masking = ifelse(Masking == "Open Label", "None (Open Label)", Masking))

data_merged = data_merged %>%
  mutate(cancer_type = ifelse(cancer_type == "sarcoma", "Sarcoma", cancer_type))

lapply(data_merged[-11],table)

#### 准备logistic回归分析 ####

# 将因变量Type转换为二分类因子（假设Photon为参考组）
data_merged$Type <- factor(data_merged$Type, levels = c("Photon", "PBRT"))

# 检查并转换其他分类变量为因子（根据实际数据类型调整）
data_merged = data_merged %>%
  mutate(
    # 二分类变量：通常以"No", "False", "Male"为参考
    Excludes_children = factor(Excludes_children, levels = c("f", "t")),
    Excludes_elderly = factor(Excludes_elderly, levels = c("f", "t")),
    Allocation = factor(Allocation, levels = c("None/Not Report", "Non-Randomized", "Randomized")), # 假设Non-Randomized为参考组
    Gender = factor(Gender, levels = c("All", "Female", "Male")), # 假设All为参考组
    # 多分类变量
    Phase_done = factor(Phase_done, levels = c("Not Applicable", "Phase 1","Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3","Phase 3", "Phase 4")),
    Recruitment_status = factor(Recruitment_status, levels = c("Unknown status", "Completed","Ongoing", "Stopped Early")),
    Masking = factor(Masking, levels = c("None (Open Label)", "Single", "Double", "Triple/Quadruple")),
    cancer_type = factor(cancer_type, levels = c("Others", "Digestive_System","Head_and_Neck", "CNS","Breast", "Lung", "Prostate", "Gynecology","Sarcoma","multi-cancer")), # 假设multi-cancer为参考组
    Source_class = factor(Source_class, levels = c("None/Not Report", "OTHER", "GOV", "INDUSTRY")),
    Enrollment_segm = factor(Enrollment_segm, levels = c("<50", "50-100", ">100"))
  )

# 构建模型
model <- glm(
  Type ~ Phase_done + Recruitment_status + Allocation + Masking + 
    Excludes_children + Excludes_elderly + Gender + Enrollment_segm + 
    Source_class + cancer_type,
  data = data_merged, 
  family = binomial
)
summary(model)
# 提取回归结果
model_results <- tidy(model)
# 计算OR和95%CI
model_results <- model_results %>%
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error),
    p_value = p.value
  )
# 保存结果到CSV文件
write.csv(model_results, "Results2/logistic_regression_results.csv", row.names = FALSE)


library(logistf)

firth_model <- logistf(
  Type ~ Phase_done + Recruitment_status + Allocation + Masking + 
    Excludes_children + Excludes_elderly + Gender + Enrollment_segm + 
    Source_class + cancer_type,
  data = data_merged
)
summary(firth_model)

# 提取回归结果
model_summary <- summary(firth_model)

# 获取系数、P值、置信区间等关键指标
model_results <- data.frame(
  Variable = names(firth_model$coefficients),
  Coefficient = firth_model$coefficients,
  Odds_Ratio = exp(firth_model$coefficients),
  P_Value = firth_model$prob,
  Lower_CI = exp(firth_model$ci.lower),
  Upper_CI = exp(firth_model$ci.upper)
)

# 保存结果到CSV文件
write.csv(model_results, "Results2/logistic_regression_firth_results.csv", row.names = FALSE)



# library(ggplot2)
# 
# # 准备数据（排除截距项）
# plot_data <- model_results[-1, ] # 去除(Intercept)
# plot_data$Variable <- factor(plot_data$Variable,
#                              levels = plot_data$Variable[order(plot_data$Odds_Ratio)])
# 
# # 创建森林图
# ggplot(plot_data, aes(x = Odds_Ratio, y = Variable)) +
#   geom_point(size = 3, shape = 18) +
#   geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.2) +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
#   scale_x_log10() +  # 对数刻度使图形更对称
#   labs(title = "影响因素森林图 (PBRT vs Photon)",
#        x = "优势比 (Odds Ratio, 对数尺度)",
#        y = "变量",
#        caption = "误差线表示95%置信区间") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# # 添加显著性标记
# plot_data$Significance <- ifelse(plot_data$P_Value < 0.001, "***",
#                                  ifelse(plot_data$P_Value < 0.01, "**",
#                                         ifelse(plot_data$P_Value < 0.05, "*", "")))
# 
# ggplot(plot_data, aes(x = reorder(Variable, Odds_Ratio), y = log10(Odds_Ratio),
#                       fill = P_Value < 0.05)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = Significance), vjust = 0.5, hjust = 0.5, size = 5) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
#   coord_flip() +
#   labs(title = "各变量优势比及显著性",
#        x = "变量",
#        y = "优势比",
#        fill = "显著 (p < 0.05)") +
#   scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "steelblue")) +
#   theme_minimal()


### 常见1个Cancer Type 的试验设计差异分析 ####
rm(list = ls())
gc()

library(dplyr)

data_merged = read.csv("data3/data_merged.csv")
load("data3/prt_condition.rdata")
load("data3/data4_conditions.rdata")

# 将prt_condition中的cancer_type中的Gastrointestinal更换为Digestive_System
prt_condition = prt_condition %>%
  mutate(cancer_type = ifelse(cancer_type == "Gastrointestinal", "Digestive_System", cancer_type))

# 将data_merged中的source_class中的Other更换为OTHER
data_merged = data_merged %>%
  mutate(Source_class = ifelse(Source_class == "Other", "OTHER", Source_class))


# 对data_merged进行去重复
## ！！！ 注意，Proton和Carbon之间的重复被删除了
data_merged = data_merged %>%
  distinct()

# 处理pbrt_condition数据,从data4将data_merged出现的id筛选出来
pbrt_condition = data4_conditions %>%
  filter(Id %in% data_merged$Id[data_merged$Type == "PBRT"]) %>%
  select(Id, cancer_type)
length(unique(pbrt_condition$Id))

digestive_pbrt_condition = pbrt_condition %>%
  filter(cancer_type == "Digestive_System")
length(unique(digestive_pbrt_condition$Id))


prt_condition = prt_condition %>%
  select(Id, cancer_type)
length(unique(prt_condition$Id))

digestive_prt_condition = prt_condition %>%
  filter(cancer_type == "Digestive_System")
length(unique(digestive_prt_condition$Id))

pbrt_data = data_merged %>%
  filter(Type == "PBRT") %>%
  left_join(digestive_pbrt_condition, by = "Id") %>%
  distinct()

prt_data =  data_merged %>%
  filter(Type == "Photon") %>%
  left_join(digestive_prt_condition, by = "Id") %>%
  distinct()

data_merged = rbind(pbrt_data, prt_data)

data_merged = data_merged %>%
  filter(!is.na(cancer_type))


data_merged = data_merged %>%
  mutate(Masking = ifelse(Masking == "Open Label", "None (Open Label)", Masking))

lapply(data_merged[-11],table)



library(tableone)
data = data_merged
dput(names(data))

vars = c("Phase_done","Recruitment_status", "Allocation", "Masking", 
         "Excludes_children", "Excludes_elderly", "Gender", "Enrollment_segm", 
         "Source_class")
factorvars = vars

all_tableone = CreateTableOne(vars = vars,
                              strata = "Type",
                              data = data,
                              factorVars = factorvars)
output = print(x = all_tableone, #指定表格
               #contDigits = 1, #连续变量保留1位小数
               #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
               #nonnormal = nonnormalvars, #指定非正态连续变量
               #exact = exactvars,  #指定需要Fisher确切法统计的变量
               
               showAllLevels = T,  #TRUE则显示所有分类变量水平的频数和百分比
               noSpaces = F, #删除用于对齐的空格
               printToggle = FALSE) #不展示输出结果

write.csv(output, file = "Results2/digestive_tableone.csv")



library(CBCgrps)

tab1 = twogrps(data,
               gvar = "Type",
               varlist = vars,
               ShowStatistic = T,
               tabNA = "ifany")


output = tab1$Table
write.csv(output, file = "Results2/digestive_tableone_CBCgrps.csv")











