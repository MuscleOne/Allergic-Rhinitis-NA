##### 1. read the data of demography and clinical information ####
df_demography = read.csv("data/cleaned/demography.csv")
df_clinical_info = read.csv("data/cleaned/clinical_info.csv")

#### 2. compare the study group and the non-study group ####
library(CBCgrps)
# CBCgrps::twogrps

compare_demography = twogrps(df_demography[,-1], "flag")
compare_clinical = twogrps(df_clinical_info[,-1], "flag")

# the tables
compare_demography
compare_clinical

# save the table as dataframe
df_demo_comparison = compare_demography$Table
df_clinical_comparison = compare_clinical$Table

# save the comparison results as csv in the outputs folder
write.csv(
  df_demo_comparison, file = "outputs/demo_comparison.csv", row.names = F
)
write.csv(
  df_clinical_comparison, file = "outputs/clinical_comparison.csv", row.names = F
)

