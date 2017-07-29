#
# Selected Person Characteristics by Sex
#

# Keep only total population, total population by age and sex
census.person.char.by.sex = read.csv(file = "data/census/2016Census_P01_SA_POA.csv")
census.person.char.by.sex.tot = census.person.char.by.sex [, grepl ("POA_CODE_2016|Tot_|Age_\\d", names (census.person.char.by.sex))]
census.person.char.by.sex.tot = subset (census.person.char.by.sex.tot, select = -c(Indigenous_persons_Tot_M, Indigenous_persons_Tot_F, Indigenous_persons_Tot_P))
# remove POA from postcode
census.person.char.by.sex.tot$POA_CODE_2016 =  sapply(census.person.char.by.sex.tot$POA_CODE_2016, substring, 4, 99)

#
# Zip Codes Median information
#

# all columns usefull
census.medians.averages = read.csv(file = "data/census/2016Census_G02_SA_POA.csv")
census.medians.averages = sapply(census.medians.averages$POA_CODE_2016, substring, 4, 99)


#
# Marital Status
#

# Keep only the total columns for married, never married, etc.
census.marital.status = read.csv(file = "data/census/2016Census_P05_SA_POA.csv")
census.marital.status.tot = census.person.char.by.sex [, grepl ("POA_CODE_2016|P_Tot_", names (census.marital.status))]
# remove POA from postcode
census.marital.status.tot$POA_CODE_2016 =  sapply(census.marital.status.tot$POA_CODE_2016, substring, 4, 99)

#
# Social Marital Status
#

# Keep only the total columns for de facto marriage, reg marriage, etc.
cenus.social.marital.status = read.csv(file = "data/census/2016Census_P06_SA_POA.csv")
census.social.marital.status.tot = cenus.social.marital.status [, grepl ("POA_CODE_2016|P_Tot_", names (cenus.social.marital.status))]
# remove POA from postcode
census.social.marital.status.tot$POA_CODE_2016 =  sapply(census.social.marital.status.tot$POA_CODE_2016, substring, 4, 99)

#
# Types of education institutions
#


# Keep only the total columns for the types of education institutions
type.of.educational.institution = read.csv(file = "data/census/2016Census_P15_SA_POA.csv")
type.of.educational.institution.tot = type.of.educational.institution [, grepl ("POA_CODE_2016|Tot_P", names (type.of.educational.institution))]
# remove POA from postcode
type.of.educational.institution.tot$POA_CODE_2016 =  sapply(type.of.educational.institution.tot$POA_CODE_2016, substring, 4, 99)


#Keep only total of people by highest year of school
highest.year.school = read.csv(file = "data/census/2016Census_P16B_SA_POA.csv")
highest.year.school.tot = highest.year.school [, grepl ("POA_CODE_2016|_Tot$", names (highest.year.school))]
# remove POA from postcode
highest.year.school.tot$POA_CODE_2016 =  sapply(highest.year.school.tot$POA_CODE_2016, substring, 4, 99)


#keep the personal weekly input by band
census.weekly.income = read.csv(file = "data/census/2016Census_P17_SA_POA.csv")
census.weekly.income.tot = census.weekly.income [, grepl ("POA_CODE_2016|_Tot$", names (census.weekly.income))]
# remove POA from postcode
census.weekly.income.tot$POA_CODE_2016 =  sapply(census.weekly.income.tot$POA_CODE_2016, substring, 4, 99)


