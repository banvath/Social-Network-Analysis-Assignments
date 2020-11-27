rm(list = ls(all = TRUE))
setwd("C:/Users/dplewi3/Dropbox")
setwd("~/Dropbox")

library(data.table)
library(igraph)
library(reshape)
library(MASS)
library(plm)
library(pglm)
library(ggplot2)
library(zoo)
library(panelAR)

############## Data prep

# key insight -- lists are helpful for creating mini-enviroments to work with for each district. reshape can help out with getting the characteristics of neighbors all at once by transforming the data from long to wide

districts = fread("district_information.csv", header = TRUE)
border = fread("border_information.csv", header = TRUE)
rain = fread("rain_information.csv", header = TRUE)

# first want to make border edge list undirected
border = rbindlist(list(border, data.table(cbind(border$district, border$focal_district))), use.names = FALSE)

districts[year == 1985 & district == "Amritsar", dup := 1]
districts[year == 1985 & district == "Amritsar", year := 1984]

cols = colnames(districts)[-c(1:3,ncol(districts))]
districts[year == 1984 & district == "Amritsar", (cols) := lapply(.SD, function(x) max(x)), .SDcols = cols]

districts = districts[is.na(dup)]
districts[,dup := NULL]

districts[year == 1985, year := 1984]

#districts[, dup := .N, by = c("district", "year")]

# next set up droughts and floods variables from rain info using spi
# sum over election years, accounting for non-regular intervals in between elections
years = sort(unique(districts$year)) # each election year
periods = c(list(seq(1946, 1951)), lapply(seq_along(years)[-1], function(i) seq(years[i -1] + 1, years[i]))) # list with years in each interval


# can set up a few with something like
rain[, moderately_dry := as.numeric(spi < -1)]
rain[, severely_dry := as.numeric(spi < -1.5)]
rain[, extremely_dry := as.numeric(spi < -2)]
rain[, moderately_wet := as.numeric(spi > 1)]
rain[, severely_wet := as.numeric(spi > 1.5)]
rain[, extremely_wet := as.numeric(spi > 2)]
rain[, moderately_abnormal := as.numeric(spi > 1 | spi < -1)]
rain[, severely_abnormal := as.numeric(spi > 1.5 | spi < -1.5)]
rain[, extremely_abnormal := as.numeric(spi > 2 | spi < -2)]



# creating a list with just rain in each interval
rain_elections = lapply(seq_along(periods), function(i) rain[year %in% periods[[i]]])

spicols = colnames(rain)[(ncol(rain)-8):ncol(rain)]

rain_elections = lapply(seq_along(periods), function(i) rain_elections[[i]][, (spicols) := lapply(.SD, function(x) sum(x)), .SDcols = spicols, by = district])
rain_elections = lapply(seq_along(periods), function(i) rain_elections[[i]][, spi := mean(spi), by = district])
rain_elections = lapply(seq_along(periods), function(i) rain_elections[[i]][, year := as.integer(max(periods[[i]])), by = district])
rain_elections = lapply(seq_along(periods), function(i) rain_elections[[i]][, rain := sum(rain), by = district])

spi_elections = lapply(seq_along(periods), function(i) unique(rain_elections[[i]][, list(moderately_dry = moderately_dry, severely_dry = severely_dry, extremely_dry = extremely_dry, moderately_wet = moderately_wet, severely_wet = severely_wet, extremely_wet = extremely_wet, moderately_abnormal = moderately_abnormal, severely_abnormal = severely_abnormal, extremely_abnormal = extremely_abnormal, spi = spi, year = year, rain = rain, district = district,  period = i)]))

spi_elections = rbindlist(spi_elections)


setkeyv(spi_elections, c("district", "year"))
setkeyv(districts, c("district", "year"))
districts = merge(districts, spi_elections)

dwide = reshape( districts, idvar="district", timevar="year", direction="wide" )

years = unique(districts$year)
focal_vars =  c(sapply(seq_along(years), function(i) paste("spi.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("moderately_dry.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("severely_dry.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("extremely_dry.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("moderately_wet.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("severely_wet.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("extremely_wet.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("moderately_abnormal.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("severely_abnormal.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("extremely_abnormal.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("new_parties.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("rain.",years[i],sep ="")),
		sapply(seq_along(years), function(i) paste("total_parties.",years[i],sep =""))
		)

alter_table = setNames(data.table(matrix(data = rep(0, nrow(dwide)*length(years)), nrow = nrow(dwide), ncol = length(focal_vars))), paste("adj_",focal_vars,sep =""))
adjs = lapply(seq_len(nrow(dwide)), function(j) border$district[ border$focal_district == dwide$district[j]])

dwide_df = data.frame(dwide)
# using unlist to return a vector to call mean on -- .SD with .SDcols to refer to the target columns also works
# for(i in seq_along(focal_vars)){
# 	for(j in seq_len(nrow(dwide))){
# 		alter_table[j,i] = mean(unlist(dwide[district %in% adjs[[j]],colnames(dwide)==focal_vars[i],with = FALSE]), na.rm = TRUE)
# 	}
# }

# could also convert to a data frame, which is a bit quicker in this case
for(i in seq_along(focal_vars)){
	for(j in seq_len(nrow(dwide))){
		alter_table[j,i] = mean(dwide_df[dwide_df$district %in% adjs[[j]],colnames(dwide_df)==focal_vars[i]], na.rm = TRUE)
	}
}

dwide = cbind(dwide, alter_table)


districts_adjrain = reshape(dwide, idvar= "district", varying = colnames(dwide)[2:ncol(dwide)], sep = ".", timevar = "year", direction = "long")	

########### Question 1

##### Part A
# lagging variables

# can also do this in four lines, but cols/sdcols/lapply from the data table example saves a bit of space
# it's also a bit more modular in case we want to add columns in later on
cols = c("moderately_dry", "severely_dry", "extremely_dry", "moderately_wet", "severely_wet", "extremely_wet", "moderately_abnormal", "severely_abnormal", "extremely_abnormal", "rain", "spi", "new_parties", "adj_moderately_dry", "adj_severely_dry", "adj_extremely_dry", "adj_moderately_wet", "adj_severely_wet", "adj_extremely_wet", "adj_moderately_abnormal", "adj_severely_abnormal", "adj_extremely_abnormal", "adj_new_parties", "adj_rain", "adj_spi", "total_parties", "adj_total_parties"
	)
lags = paste("l_",cols,sep="")

setorderv(districts_adjrain, c("district", "year"))
districts_adjrain[, (lags) := lapply(.SD, function(x) rollapplyr(x, list(-1), mean, partial = TRUE, fill = NA)), .SDcols = cols, by = district]	


# can use loess or any other kind of scatter plot
library(ggplot2)

lowess_rain = ggplot(districts_adjrain, aes(rain, new_parties)) + geom_smooth(method = "loess", se = F) + labs(x = "Rainfall, in ml", y = "New organization") + coord_cartesian(ylim = c(1.75, 2.75)) + scale_y_continuous(breaks=seq(1.75,2.75,.25))
ggsave("lowess_rain.pdf", width = 7, height = 7, units = "in")

lowess_spi = ggplot(districts_adjrain, aes(spi, new_parties)) + geom_smooth(method = "loess", se = F) + labs(x = "Standardized Precipitation Index", y = "") + coord_cartesian(ylim = c(1.75, 2.75), xlim = c(-1.5, 1.5))
ggsave("lowess_spi.pdf", width = 7, height = 7, units = "in")


##### Part B

# is rain related to rain from previous period
summary(plm(rain ~ l_rain + l_adj_rain, data = districts_adjrain, effect = "twoways", model = "within", index = "district"))

# and spi
summary(plm(spi ~ l_spi + l_adj_spi, data = districts_adjrain, effect = "twoways", model = "within", index = "district"))

# both related 

##### Part C

# now try with drought/flood measure
summary(pglm(moderately_abnormal ~ l_moderately_abnormal + l_adj_moderately_abnormal, data = districts_adjrain, effect = "twoways", model = "within", index = "district", family = "poisson"))

# not related, so can use for exogenous variation


########### Question 2
years = sort(years)
for(i in seq_along(years)[-1]){
	districts_adjrain[year == years[i], interval := year - years[i - 1]]
}

districts_adjrain[year == 1951, interval := 6]

# in case of issues with plm/pglm
#districts_nonmissing = districts_adjrain[!is.na(spi)]
#fwrite(districts_adjrain, "districts_adjrain.csv")
#districts_adjrain = read.csv("districts_adjrain.csv", head = TRUE)


# can compare between pglm/panelAR model setup
summary(pglm(new_parties ~ moderately_abnormal + interval, data = districts_adjrain, effect = "twoways", model = "within", index = "district", family = "poisson"))

districts_adjrain_df = as.data.frame(districts_adjrain) # needed for panelAR

summary(panelAR(new_parties ~ moderately_abnormal + interval + year, data = districts_adjrain_df, panelVar = "district", timeVar = "year", rho.na.rm = TRUE, autoCorr = "psar1", panelCorrMethod = "phet"))


# new parties moderately more likely when droughts or floods

# one type of new parties that are more likely to be formed when there are droughts and floods
summary(panelAR(new_parties_socialist ~ moderately_abnormal + interval + year, data = districts_adjrain_df, panelVar = "district", timeVar = "year", rho.na.rm = TRUE, autoCorr = "psar1", panelCorrMethod = "phet"))

########## Question 3

# add in neighbor effect
summary(panelAR(new_parties ~ new_parties_caste + l_adj_moderately_abnormal + interval + year, data = districts_adjrain_df, panelVar = "district", timeVar = "year", rho.na.rm = TRUE, autoCorr = "psar1", panelCorrMethod = "phet"))


# neighbors' experiencing extreme weather in the prior period is related to increased new political parties in the current period -- provides evidence for diffusion effect

########## Question 4

##### Part A

summary(panelAR(new_parties_national_scope ~ moderately_abnormal + l_adj_moderately_abnormal + interval + year, data = districts_adjrain_df, panelVar = "district", timeVar = "year", rho.na.rm = TRUE, autoCorr = "psar1", panelCorrMethod = "phet"))

summary(panelAR(new_parties_state_scope ~ moderately_abnormal + l_adj_moderately_abnormal + interval + year, data = districts_adjrain_df, panelVar = "district", timeVar = "year", rho.na.rm = TRUE, autoCorr = "psar1", panelCorrMethod = "phet"))

summary(panelAR(new_parties_regional_scope ~ moderately_abnormal + l_adj_moderately_abnormal + interval + year, data = districts_adjrain_df, panelVar = "district", timeVar = "year", rho.na.rm = TRUE, autoCorr = "psar1", panelCorrMethod = "phet"))

##### Part B
summary(panelAR(political_concentration ~ moderately_abnormal + l_adj_moderately_abnormal + interval + year, data = districts_adjrain_df, panelVar = "district", timeVar = "year", rho.na.rm = TRUE, autoCorr = "psar1", panelCorrMethod = "phet"))

# results suggest national parties may try to capitalize on disruption and contest new elections in affected districts, and smaller regional parties are more likely to diffuse to neighboring districts

# at the same time, extreme rainfall is also related to lower levels of concentration, suggesting that droughts and floods may upset the entrenched political structure, which likely favors smaller regional parties in elections 

########## Question 5

# candidate-level info
districts_parties = fread("new_parties_in_each_district_by_candidate.csv", header = TRUE)

# get unique parties that contest in each year
districts_parties = unique(districts_parties[,c("district", "year","party_name")])
colnames(districts_parties)[colnames(districts_parties) == "party_name"] = "party"

# subset to districts with rain data
districts_parties = districts_parties[district %in% dwide$district]

setorderv(districts_parties, c("district", "year"))

# split by district so we can make mini-tables to work on independently
dp_district = split(districts_parties, f = districts_parties$district)

# create mini-tables for each district's neighbors
dp_district_neighbor = lapply(seq_along(dp_district), function(i) districts_parties[district %in% adjs[[i]]])

# initialize a list to put parties that have appeared in neighbors into
dp_district_neighbor_any_year = list()

# each ij element will contain the parties that have appeared in district i's neighbors, before year j
for(i in seq_along(dp_district)){
	years_local = unique(dp_district[[i]]$year) # lets us flexibly iterate depending on how many years we have for each district
	dp_district_neighbor_any_year[[i]] = list() # re-initializing second level
	for(j in seq_along(years_local)){
		dp_district_neighbor_any_year[[i]][[j]] = dp_district_neighbor[[i]][year < years_local[j]] # subset to parties that have been introduced before year j
	}
}

# create an object to make the comparison with the focal district
dp_district_any_year = lapply(seq_along(dp_district), function(i) split(dp_district[[i]], f = dp_district[[i]]$year))

# checking if the parties in this year have appeared in the neighbor before
for(i in seq_along(dp_district)){
	for(j in seq_along(dp_district_any_year[[i]])){
		dp_district_any_year[[i]][[j]][, same_party_any_year := party %in% dp_district_neighbor_any_year[[i]][[j]]$party]
	}
}

dp_diffused_any_year = rbindlist(lapply(seq_along(dp_district), function(i) rbindlist(dp_district_any_year[[i]])))

# sum diffused parties for each year
dp_diffused_any_year_panel = unique(dp_diffused_any_year[, list(diffused_any_year = sum(same_party_any_year)), by = c("district", "year")])

setkeyv(dp_diffused_any_year_panel, c("district", "year"))
setkeyv(districts_adjrain, c("district", "year"))

# merge back to main data
districts_adjrain = merge(districts_adjrain, dp_diffused_any_year_panel, all.x = TRUE)

# subtract from new parties to get parties that have not diffused from neighbors
districts_adjrain[, non_diffused_any_year := new_parties - diffused_any_year]

districts_adjrain_df = data.frame(districts_adjrain)

# set up similar regression as before
summary(panelAR(diffused_any_year ~  moderately_abnormal + l_adj_moderately_abnormal + interval + year, data = districts_adjrain_df, panelVar = "district", timeVar = "year", rho.na.rm = TRUE, autoCorr = "psar1", panelCorrMethod = "phet"))
# no relationship to parties that have contested elections in neighboring districts in the past

summary(panelAR(non_diffused_any_year ~  moderately_abnormal + l_adj_moderately_abnormal + interval + year, data = districts_adjrain_df, panelVar = "district", timeVar = "year", rho.na.rm = TRUE, autoCorr = "psar1", panelCorrMethod = "phet"))
# but positive relationship to parties that have are brand new, that have not contested elections in neighboring districts in the poast

districts_adjrain[, diffused_prop := diffused_any_year/new_parties]
summary(panelAR(diffused_prop ~  moderately_abnormal + l_adj_moderately_abnormal + interval + year, data = districts_adjrain_df, panelVar = "district", timeVar = "year", rho.na.rm = TRUE, autoCorr = "psar1", panelCorrMethod = "phet"))
# and neighbors' extreme weather seems to reduce proportion of parties that have contested elections in neighboring districts in the past versus brand new parties

# results suggest that the diffusion channel seems to stimulate the entry of brand new political parties, rather than the entry of the same parties that have entered in neighboring districts
# so, people seem to be stimulated to organize more broadly and to organize according to their own political interests, as opposed to co-opting the specific interests of their neighbors and their political parties
