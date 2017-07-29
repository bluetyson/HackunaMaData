# LIBRARIES
library (plyr)
library (tidyr)

##############################################################################################################
#
# DATAFRAME : WEEKLY RENT
#
##############################################################################################################

census.rent.weekly = read.csv (file = "data/census/2016Census_G36_SA_POA.csv")

# Keep only total for each amount (don't care about property type)
census.rent.weekly.tot = census.rent.weekly [, grepl ("_Tot$|POA_CODE_2016", names (census.rent.weekly))]
census.rent.weekly.tot = subset (census.rent.weekly.tot, select = -c(Rent_ns_Tot, Tot_Tot))
# remove POA from postcode
census.rent.weekly.tot$POA_CODE_2016 = sapply(census.rent.weekly.tot$POA_CODE_2016, substring, 4, 99)

# write as csv
write.csv (x = census.rent.weekly.tot, file = "data/census/2016Census_G36_SA_POA2.csv")

# changes as per 2016Census_G36_SA_POA_CALCULATIONS

census.rent.weekly.avg = read.csv (file = "data/census/2016Census_G36_SA_POA2.csv")

# classify based on quartiles
# create quartiles
census.rent.weekly.avg = within(census.rent.weekly.avg, 
                                         quartile <- cut(avg.rent.per.week, quantile(avg.rent.per.week, probs=0:4/4), include.lowest=FALSE, labels=FALSE))

# relabel as factor
census.rent.weekly.avg$quartile = as.factor (census.rent.weekly.avg$quartile)

# assign names to factors
census.rent.weekly.avg$quartile = revalue(census.rent.weekly.avg$quartile, c("1"="low",
                                                                             "2"="medium",
                                                                             "3"="high",
                                                                             "4"="very high"))


##############################################################################################################
#
# DATAFRAME : FAMILY COMPOSITION
#
##############################################################################################################

census.family.composition = read.csv (file = "data/census/2016Census_G25_SA_POA.csv")

# remove POA from postcode
census.family.composition$POA_CODE_2016 = sapply(census.family.composition$POA_CODE_2016, substring, 4, 99)

# CF_no_children_F	Couple family with no children
# CF_no_children_P	
# CF_ChU15_a_Total_F	Couple family with no children under 15
# CF_ChU15_a_Total_P	
# OPF_ChU15_a_Total_F	One parent family with children under 15
# OPF_ChU15_a_Total_P	

census.family.composition.subset = subset (census.family.composition, select = c ("POA_CODE_2016", "CF_no_children_F", "CF_no_children_P", 
                                                                                  "CF_ChU15_a_Total_F", "CF_ChU15_a_Total_P", 
                                                                                  "OPF_ChU15_a_Total_F", "OPF_ChU15_a_Total_P"))
# reshape data using gather
census.family.composition.reshaped = 
  census.family.composition %>%
  gather(class, total, -POA_CODE_2016)

##############################################################################################################
#
# DATAFRAME : FAMILY WEEKLY INCOME
#
##############################################################################################################

census.family.weekly.income = read.csv (file = "data/census/2016Census_G28_SA_POA.csv")

# remove no weekly income
census.family.weekly.income.subset = census.family.weekly.income [, !grepl ("Neg", names (census.family.weekly.income))]

# remove everything that doesn't have a numeric range
census.family.weekly.income.subset =
  census.family.weekly.income.subset[, !names(census.family.weekly.income.subset) %in% c ("Prt_inc_std_cpl_fam_no_child", 
                                                                                          "Part_inc_std_cpl_fam_wth_chld", 
                                                                                          "Part_inc_statd_One_parent_fam", 
                                                                                          "Partial_income_statd_Othr_fam", 
                                                                                          "Partial_income_stated_Tot", 
                                                                                          "All_inc_ns_cpl_fam_no_child", 
                                                                                          "All_inc_ns_cpl_fam_wth_chld", 
                                                                                          "All_incomes_ns_One_parent_fam", 
                                                                                          "All_incomes_ns_Other_fam", 
                                                                                          "All_incomes_ns_Tot", 
                                                                                          "Tot_cpl_fam_no_child", 
                                                                                          "Tot_cpl_fam_wth_chld", 
                                                                                          "Tot_One_parent_fam", 
                                                                                          "Tot_Other_fam", "Tot_Tot"   )]

# keep only total per range 
census.family.weekly.income.subset = census.family.weekly.income.subset [, grepl ("POA_CODE_2016|_Tot$", names (census.family.weekly.income.subset))]

# write as csv
write.csv (x = census.family.weekly.income.subset, file = "data/census/2016Census_G28_SA_POA2.csv")

# changes as per 2016Census_G28_SA_POA_CALCULATIONS

# read average weekly income for families in SA
census.family.weekly.income.avg = read.csv (file = "data/census/2016Census_G28_SA_POA2.csv")

# classify based on quartiles
# create quartiles
census.family.weekly.income.avg = within(census.family.weekly.income.avg, 
                                               quartile <- cut(avg.total.per.week, quantile(avg.total.per.week, probs=0:4/4), include.lowest=FALSE, labels=FALSE))

# relabel as factor
census.family.weekly.income.avg$quartile = as.factor (census.family.weekly.income.avg$quartile)

# assign names to factors
census.family.weekly.income.avg$quartile = revalue(census.family.weekly.income.avg$quartile, c("1"="low",
                                                                                               "2"="medium",
                                                                                               "3"="high",
                                                                                               "4"="very high"))

# remove POA from postcode
census.family.weekly.income.avg$POA_CODE_2016 = sapply(census.family.weekly.income.avg$POA_CODE_2016, substring, 4, 99)

##############################################################################################################
#
# DATAFRAME : FAMILY LOANS
#
##############################################################################################################

census.family.mortgage = read.csv (file = "data/census/2016Census_G35_SA_POA.csv")

# remove POA from postcode
census.family.mortgage$POA_CODE_2016 = sapply (census.family.mortgage$POA_CODE_2016, substring, 4, 99)

# reshape data using gather
census.family.reshaped = 
                      census.family.mortgage %>%
                        gather(class, total, -POA_CODE_2016)








