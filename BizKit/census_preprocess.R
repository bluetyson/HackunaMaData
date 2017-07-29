

#
# WEEKLY RENT
#

census.rent.weekly = read.csv (file = "data/census/2016Census_G36_SA_POA.csv")

# Keep only total for each amount (don't care about property type)
census.rent.weekly.tot = census.rent.weekly [, grepl ("_Tot$|POA_CODE_2016", names (census.rent.weekly))]
census.rent.weekly.tot = subset (census.rent.weekly.tot, select = -c(Rent_ns_Tot, Tot_Tot))
# remove POA from postcode
census.rent.weekly.tot$POA_CODE_2016 = sapply(census.rent.weekly.tot$POA_CODE_2016, substring, 4, 99)


