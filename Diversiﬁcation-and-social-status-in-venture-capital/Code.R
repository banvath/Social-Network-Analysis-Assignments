rm(list = ls(all = TRUE))
setwd("C:/Users/dplewi3/Dropbox")
setwd("~/Dropbox")

library(data.table)
library(igraph)
library(Matrix)
library(ggpubr)
library(miceadds)
library(plot3D)
library(rgl)
library(matlab)
library(proxy)
library(ggplot2)
library(hhi)
library(plm)
library(taRifx)
library(ggplot2)
library(zoo)
library(pglm)

############## Data prep

# key insight -- similar to the previous assignment, there are three relevant dimensions to work along: (1) investor firms, (2) companies, and (3) deals. in the beginning we'll set up a mini-"cube" that will represent all dimensions of the data, and then we can use this to produce the rest of the measures and objects that will be needed for the remainder of the questions

# two-mode edge list relating investors to deals
edges = fread("investors_and_deals.csv", header = TRUE)

# deal info
deals = fread("deal_details.csv", header = TRUE)

length(unique(edges$Deal_Id))
nrow(deals)
# there are about 120k deals that do not appear in DealInvestorRelation.csv

# harmonizing/managing field names 
colnames(deals)[1] = "Deal_Id"
colnames(deals)[-(1:2)] = paste(colnames(deals)[-(1:2)], ".deal", sep = "")


setkey(deals, "Deal_Id")
setkey(edges, "Deal_Id")


edges = merge(edges, deals)
# nrow edges is preserved

# import company info here too
companies = fread("company_details.csv", header = TRUE)
length(unique(edges$CompanyId))
nrow(companies)
# about 15k companies that do not appear in deals via DealInvestorRelation.csv

# company id doesn't provide a unique identifier
sum(duplicated(companies$CompanyID)) 

# can try to see what duplicates mean
companies[, duplicates := .N > 1, by = CompanyID]
companies[duplicates == TRUE]

# seem to come from different primary industry codes
companies[, total_industries:= length(unique(Primary_Industry_Code)), by = c("CompanyID")]
max(companies$total_industries)

# maximum of two

# retain industry info
companies[total_industries > 1, second_industry_code := unique(Primary_Industry_Code)[2], by = c("CompanyID")]

# homogenize within year
companies[total_industries > 1, Primary_Industry_Code := unique(Primary_Industry_Code)[1], by = c("CompanyID")]

# check to see if duplicates remain
companies = unique(companies[, by = c("CompanyID")])

sum(duplicated(companies$CompanyID)) 
# all set


# harmonizing names again
colnames(companies)[1] = "CompanyId"
colnames(companies)[-1] = paste(colnames(companies)[-1], ".company", sep = "")


setkey(companies, "CompanyId")
setkey(edges, "CompanyId")

edges = merge(edges, companies)

# last, get the information about investors
investors = fread("investor_details.csv", header = TRUE)

length(unique(edges$Investor_Id))
nrow(investors)
# there are about 1k investors that do not appear in investors_and_deals.csv

# no duplicates this time
sum(duplicated(investors$InvestorId)) 

# harmonizing/managing field names 
colnames(investors)[1] = "Investor_Id"
colnames(investors)[-1] = paste(colnames(investors)[-1], ".investor", sep = "")


setkey(investors, "Investor_Id")
setkey(edges, "Investor_Id")


edges = merge(edges, investors)

edges = edges[Investor_Name.investor != "Individual Investor"]

# for making a yearly-updated network
edges[, date := as.Date(Deal_Date.deal, "%d %b %y")]
edges[, year := year(date)]

setkeyv(edges, c("Investor_Id", "year"))

################# Controls
# generate investor success variable and some controls

# success variable counts successful 
edges[, successful_investments := cumsum(Deal_Type_1.deal=="IPO" | Deal_Type_1.deal == "Merger/Acquistion" | Deal_Type_1.deal == "Buyout/LBO"), by = "Investor_Id"]

# prior experience
edges[, cumulative_investments := cumsum(Deal_Type_1.deal !="IPO" & Deal_Type_1.deal != "Merger/Acquistion"  & Deal_Type_1.deal != "Buyout/LBO"  & Deal_Type_1.deal != "Bankruptcy: Admin/Reorg"  & Deal_Type_1.deal != "Bankruptcy: Liquidation"), by = "Investor_Id"]

# firm age
edges[, age := as.integer(year -  year[seq_len(.N) == 1]),  by = "Investor_Id"]
edges[is.na(age), age := as.integer(year - min(year, na.rm = TRUE)),  by = "Investor_Id"]

# limit to just venture capital-type investors and period from 1990 to 2018 that have industry information 
edges = edges[Investor_Type.investor == "Venture Capital" & Primary_Industry_Sector.company != ""]


# allow strategic/risk controls to vary by year, as investor's strategy can change over time
# deals up until year t
years = sort((unique(edges$year[edges$year <= 2018])))

edges_yearly = lapply(seq_along(years), function(i) edges[year <= years[i]])

for(i in seq_along(edges_yearly)){
	edges_yearly[[i]][, max_year := max(year, na.rm = TRUE), by = Investor_Id] # get rid of artificial obs
	edges_yearly[[i]] = edges_yearly[[i]][max_year == years[i]]
	setorderv(edges_yearly[[i]], c("CompanyId", "date"))
	edges_yearly[[i]][, first_round := seq_len(.N) == 1, by = "CompanyId"]
	edges_yearly[[i]][, originate := sum(first_round)/length(unique(CompanyId)) > .5, by = c("Investor_Id")]
	setorderv(edges_yearly[[i]], c("Investor_Id", "date"))
	edges_yearly[[i]][, first_round_investor := seq_len(.N) == 1, by = c("Investor_Id", "CompanyId")]
	edges_yearly[[i]][, IT := sum((Primary_Industry_Sector.company == "Information Technology")*first_round_investor)/length(unique(CompanyId)) > .5, by = Investor_Id]
	edges_yearly[[i]][, early_stage := sum((Deal_Type_1.deal == "Early Stage VC" | Deal_Type_1.deal == "Accelerator/Incubator" | Deal_Type_1.deal == "Seed Round" | Deal_Type_1.deal == "Angel (individual)")*first_round_investor)/length(unique(CompanyId)) > .5, by = c("Investor_Id")]

	edges_yearly[[i]][, risk := IT + early_stage]

	# can generate herfindahl outcome variable here too
	edges_yearly[[i]][, herf := as.numeric(hhi(data.frame(c(prop.table(table(Primary_Industry_Code.company))*100)))), by = Investor_Id] # need to use c to get rid of table class and convert to numeric
	
	# just keep current year to re-create regular edges object
	edges_yearly[[i]] = edges_yearly[[i]][year == max_year] 

	print(paste0("year ",years[i]," finished at ",Sys.time())) 
    flush.console()
}

edges_ivs = rbindlist(edges_yearly)


# collapse into investor-year panel
investors = unique(edges_ivs[,list(
	successful_investments = max(successful_investments),
	cumulative_investments = max(cumulative_investments),
	age,
	originate,
	IT,
	early_stage,
	risk,
	herf), by = c("Investor_Id", "year")
	])


################# Generating status variable

# set up table that indicates whether an investor was the lead investor 
edges_lead = edges[Lead_Investor == 1]
edges_lead[, count := 1:.N, by = Deal_Id]

setkey(edges_lead, Deal_Id)
edges_list = split(edges_lead, f = edges_lead$count)

edges_notlead = edges[Lead_Investor == 0]
setkey(edges_notlead, Deal_Id)

# merge on deal to make the edge list
lead_directed = lapply(seq_along(edges_list), function(i) merge(edges_list[[i]], edges_notlead[,c(1,3)]))

lead_directed = rbindlist(lead_directed)

# quick test to make sure nothing lost during matching
length(unique(lead_directed$Investor_Id.x))

t = edges[, mixed := sum(Lead_Investor == 0) > 0 & sum(Lead_Investor == 0) < .N, by = Deal_Id]
t = t[mixed == TRUE]
length(unique(t$Investor_Id[t$Lead_Investor == 1]))

# collapse on edge pairs to generate # of occurrence of lead relationship as weights

# modifying this to take into account when deals occur, so that the status measure can change over time

# lead_weighted = lapply(seq_along(years), function(i) unique(lead_directed[year <= years[i], list(weight = .N, year ), by = c("Investor_Id.x", "Investor_Id.y")]))

lead_weighted = list()
for(i in seq_along(years)){
    lead_weighted[[i]] = lead_directed[years[i] - year <= 5 & years[i] - year >= 0 & year <= years[i]]
    lead_weighted[[i]] = unique(lead_weighted[[i]][, list(weight = .N, year = years[i]), by = c("Investor_Id.x", "Investor_Id.y")])
}

lead_weighted = rbindlist(lead_weighted)
colnames(lead_weighted) = c("from", "to", "lead", "year")

setkeyv(lead_weighted, c("from", "to", "year"))

# comembmership matrix to compare total occurences as lead versus total coccurences against

# subset edges in same fashion as above

edges_yearly = list()
incidence = list()
total_mat = list()
total_graph = list()
total_edge = list()

for(i in seq_along(years)){
    edges_yearly[[i]] = edges[years[i] - year <= 5 & years[i] - year >= 0 & year <= years[i]]
    
    incidence[[i]] = sparseMatrix(i = as.numeric(factor(edges_yearly[[i]]$Investor_Id)),
    j = as.numeric(factor(edges_yearly[[i]]$Deal_Id)),
    x = rep(1, nrow(edges_yearly[[i]])),
    dimnames = list(levels(factor(edges_yearly[[i]]$Investor_Id)), levels(factor(edges_yearly[[i]]$Deal_Id)))
    )

    total_mat[[i]] = tcrossprod(incidence[[i]])

    # sorting to preserve order
    total_mat[[i]] = total_mat[[i]][sort(rownames(total_mat[[i]])), sort(rownames(total_mat[[i]]))] 

    total_graph[[i]]  = graph.adjacency(total_mat[[i]], mode = "undirected", weighted = TRUE)
    total_edge[[i]] = data.table(get.data.frame(total_graph[[i]]), year = years[i])
}


total_edge = rbindlist(total_edge)

# account for both directions since comembership undirected
total_edge_double = rbindlist(list(total_edge, total_edge[,c("to", "from","weight","year")]), use.names = FALSE)
colnames(total_edge_double)[3] = "cooccurrence"
setkeyv(total_edge_double, c("from", "to", "year"))

superiority_edge = merge(lead_weighted, total_edge_double)

superiority_edge[, weight := lead/cooccurrence]
	
# now import back into igraph and compute bonacich/eigenvector for each year
superiority_graph = lapply(seq_along(years), function(i) graph.data.frame(superiority_edge[year == years[[i]],c(1,2,6)]))

superiority = lapply(seq_along(years), function(i) power_centrality(superiority_graph[[i]], exponent = .75, sparse = TRUE))

# cases where this is a non-missing status variable
index = sapply(seq_along(superiority), function(i) length(superiority[[i]]) > 0)

superiority = superiority[index]
years_nm = years[index]

superiority = lapply(seq_along(superiority), function(i) data.table(Investor_Id = names(superiority[[i]]), status = superiority[[i]], year = years_nm[i]))

superiority = rbindlist(superiority)

# can export here to use later and not have to compute again
fwrite(superiority, "yearly_status_vcs_12_5_19.csv")

# import and merge to investor-year panel
superiority = fread("yearly_status_vcs_12_5_19.csv", head= TRUE, key = c("Investor_Id", "year"))
setkeyv(investors, c("Investor_Id", "year"))

investors = merge(investors, superiority, all.x = TRUE)


################# generating niche width outcome variable

# what industry categories are used by all investors cumulative to each year
edges_codes_yearly = lapply(seq_along(years), function(i) edges[!is.na(Primary_Industry_Code.company) & year <= years[i], list(investor = Investor_Id, code = Primary_Industry_Code.company)])

# # compute distance measure based on co-occurrence of industry categories in investors' portfolios
# # just going to export each distance object for now and can go from there
for(i in seq_along(years)){
  incidence_labels = sparseMatrix(i = as.numeric(factor(edges_codes_yearly[[i]]$code)),
                                       j = as.numeric(factor(edges_codes_yearly[[i]]$investor)),
                                       x = rep(1, nrow(edges_codes_yearly[[i]])),
                                       dimnames = list(levels(factor(edges_codes_yearly[[i]]$code)), levels(factor(edges_codes_yearly[[i]]$investor))))
  
  jac_dist = dist(as.matrix(incidence_labels), method = "jaccard")
  label_names = t(combn(names(jac_dist), 2))
  jac_dist = as.numeric(jac_dist)
  jac_distances = data.table(label1 = label_names[,1], label2 = label_names[,2], distance = jac_dist, year = years[i])

  print(data.table(year = years[i], time = Sys.time())) 
  flush.console()
  fwrite(jac_distances, file = "label_distances_yearly_12_5_19.csv", append = TRUE)
  # include something like this if memory is strained to clean out the workspace after each run
  # rm(list = setdiff(ls(all = TRUE), c("incidence_labels", "years")))
  gc() # can be a good idea anyway 
}

# re-read the exported distances file
distances = fread("label_distances_yearly_12_5_19.csv", head = TRUE)

# make undirected -- stacking two of these on top of each other to account for each case in which the label order might be reversed or not when we merge to the investors
distances = rbindlist(list(distances, distances[, c("label2", "label1", "distance", "year")]), use.names = FALSE)

setkeyv(distances, c("label1", "label2"))

# worth noting
hist(distances$distance)

# merge to investors
edges_codes_yearly = lapply(seq_along(edges_codes_yearly), function(i) split(edges_codes_yearly[[i]], f = edges_codes_yearly[[i]]$investor))

# some investors only have one keyword associated with them
#min(sapply(seq_along(edges_codes), function(i) nrow(edges_codes[[i]])))

# leaving them in will either cause a problem of
# (1) no label, i.e. numeric(0), which would okay with the merge/collapse but doesn't let us set the keys in
# data.table properly since the key columns wouldn't exist
# (2) combn returns unique combinations of elements of a vector if it gets a length(input) > 1 
# but will return the unique combinations as input choose x if length(input) == 1
# max(investor) close to 10e7 so if this happens it exhausts the memory

t = edges_codes_yearly

index = lapply(seq_along(edges_codes_yearly), function(i) sapply(seq_along(edges_codes_yearly[[i]]), function(j) nrow(edges_codes_yearly[[i]][[j]]) > 1))
 
edges_codes_yearly = lapply(seq_along(edges_codes_yearly), function(i) edges_codes_yearly[[i]][index[[i]]])

# might have to loop but can try using apply
investor_labels = lapply(seq_along(edges_codes_yearly), function(i) lapply(seq_along(edges_codes_yearly[[i]]), function(j) t(combn(edges_codes_yearly[[i]][[j]]$code, 2))))
# memory pressure seems okay but may take a few minutes

# note that from 
edges_codes_yearly[[5]]
investor_labels[[5]]

# combn will create matched pairs when these are the only two passed into it, so just keep in mind merging will produce NAs since distances for these label pairs won't exist in the distances object, so we can change these NAs to 0s

# data table for merging each of the industry categories actually used by investors to the distance between the categories, from the distances object
investor_label_pairs = lapply(seq_along(edges_codes_yearly), function(i) lapply(seq_along(edges_codes_yearly[[i]]), function(j) data.table(Investor_Id = edges_codes_yearly[[i]][[j]]$investor[1], label1 = investor_labels[[i]][[j]][,1], label2 = investor_labels[[i]][[j]][,2], year = years[i])))

investor_label_pairs = lapply(seq_along(edges_codes_yearly), function(i) rbindlist(investor_label_pairs[[i]]))
investor_label_pairs = rbindlist(investor_label_pairs)

investor_label_pairs = unique(investor_label_pairs)
distances = unique(distances)

setkeyv(investor_label_pairs, c("label1", "label2", "year"))
setkeyv(distances, c("label1", "label2", "year"))

# ensuring numeric
#distances[, names(distances) := lapply(.SD, as.numeric)]
#investor_label_pairs[, names(investor_label_pairs) := lapply(.SD, as.numeric)]

investor_label_pairs = merge(investor_label_pairs, distances)

# per investor year
investor_label_pairs = unique(investor_label_pairs[, list(distance = sum(distance)), by = c("Investor_Id", "year")])

# merge back to investor-year panel
setkeyv(investor_label_pairs, c("Investor_Id", "year"))
setkeyv(investors, c("Investor_Id", "year"))

investors = merge(investors, investor_label_pairs, all.x = TRUE)

# number of codes used
investor_codes_yearly = lapply(seq_along(years), function(i) edges[!is.na(Primary_Industry_Code.company) & year <= years[i], list(Investor_Id, code = Primary_Industry_Code.company)])

investor_codes_yearly = lapply(seq_along(years), function(i) unique(investor_codes_yearly[[i]][, list(number_codes = length(unique(code)), year = years[i]), by = Investor_Id]))

investor_codes_yearly = rbindlist(investor_codes_yearly)

setkeyv(investor_codes_yearly, c("Investor_Id", "year"))

investors = merge(investors, investor_codes_yearly, all.x = TRUE)

# niche width measure from formula
investors[, niche_width_bar := 1 - (1 / (1 + distance / (number_codes - 1)))]

# can export this file for easy use with all variables created for later sessions
fwrite(investors, "investor_panel_12_5_19.csv")

########## Question 1

##### Part A

# re-import
investors_panel = fread("investor_panel_12_5_19.csv", head = TRUE)

# only consider cases for which firms are actually a part of the status hierarchy--i.e., have co-invested with other firms 

# set to 0 if only one industry category
investors_panel[number_codes == 1, niche_width_bar := 0]

# set up lagged variables
setorderv(investors_panel, c("Investor_Id", "year"))

cols = c("status", "successful_investments", "cumulative_investments", "originate", "IT", "early_stage")
lagged_cols = paste0("l_",cols)

investors_panel[, (lagged_cols) := lapply(.SD, function(x) as.double(rollapplyr(x, width = list(-1), FUN = mean, fill = NA))), by = Investor_Id, .SDcols = cols]

# cases 1990 and onward
investors_panel = investors_panel[!is.na(status) & year > 1989]

summary(plm(herf ~ l_status + I(l_status^2) + l_originate + l_IT + l_early_stage + age + year, model = "within", effect = "individual", data = investors_panel, index = c("Investor_Id")))

##### Part B

# set up firm-level time-averages each variable
cols = c("l_status", "l_originate", "l_IT", "l_early_stage", "age")
avg_cols = paste0(cols,"_avg")

investors_panel[, (avg_cols) := lapply(.SD, function(x) mean(x, na.rm = TRUE)), .SDcols = cols, by = Investor_Id ]
summary(glm(niche_width_bar ~ l_status + I(l_status^2) + l_originate + l_IT + l_early_stage + age + year + l_status_avg + l_originate_avg + l_IT_avg + l_early_stage_avg + age_avg, data = investors_panel, family = quasibinomial(link = "logit")))

##### Part C

# store model 
m2 = glm(niche_width_bar ~ l_status + I(l_status^2), data = investors_panel, family = quasibinomial(link = "logit"))

# set up data frame with range of values for status
m2yhat = data.frame(
	l_status = seq(min(investors_panel$l_status, na.rm = TRUE), max(investors_panel$l_status, na.rm = TRUE), length.out = 100))

# predict the fit and standard error
m2err = predict(m2, newdata = m2yhat, se.fit = TRUE)

# add the prediction to the data frame with the 95% confidence intervals
m2yhat$lci = m2err$fit - 1.96 * m2err$se.fit
m2yhat$fit = m2err$fit
m2yhat$uci = m2err$fit + 1.96 * m2err$se.fit

# plot with ggplot
ggplot(m2yhat, aes(x = l_status, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity")

########## Question 2

##### Part A

# set up lagged niche width variable
investors_panel[, l_niche_width_bar := as.double(rollapplyr(niche_width_bar, width = list(-1), FUN = mean, fill = NA)), by = Investor_Id]

# regress using pglm for the count variable outcome
summary(pglm(successful_investments ~ l_status + l_niche_width_bar + l_status*l_niche_width_bar + l_originate + l_IT + l_early_stage + age + year, model = "within", effect = "individual", data = investors_panel, index = c("Investor_Id"), family = "poisson"))

##### Part B

# setting up a 3d scatterplot with just the status and niche width variables and no fixed effects
m3 = glm(successful_investments ~ l_status + l_niche_width_bar + l_status:l_niche_width_bar, data = investors_panel, family = "poisson")

# set up scaled grid of (x,y) values
values = expand.grid(l_status=seq(min(investors_panel$l_status, na.rm = TRUE), max(investors_panel$l_status, na.rm = TRUE), length.out = 100), l_niche_width_bar=seq(min(investors_panel$l_niche_width_bar, na.rm = TRUE), max(investors_panel$l_niche_width_bar, na.rm = TRUE), length.out = 100))


# prediction from the model
values$successful_investments = predict(m3,newdata=values)

# new device for larger visualization
dev.new(height = 12, width = 12, units = "in")
par(family = "CMU Serif") # setting a latex-like font

# set up plot with x, y, and z variables
scatter3D(values$l_status, values$l_niche_width_bar, values$successful_investments, phi = 0, bty = "b2", xlab = expression("Lagged firm status"), ylab = "Lagged firm niche width", zlab = expression("Successful investments"), cex.lab = 1, ticktype = "detailed", colkey = list(cex.clab = 1, length = .5, font = 1), clab = c("Predicted values", "of interaction"))

# setting up a contour plot

# here have to specify font beforehand
cmodern = list(family = "CMU Serif", size = 12, color = "black")

# for times new roman
#times = list(family = "DejaVu Serif", size = 12, color = "black")

p1 = plot_ly(
	values,
	x = ~l_status,
	y = ~l_niche_width_bar,
	z = ~successful_investments,
	type = "contour",
	autocontour = FALSE,
	contours = list(
		end = max(values$successful_investments, na.rm = TRUE),
		size = abs(max(values$successful_investments,  na.rm = TRUE) - min(values$successful_investments,  na.rm = TRUE))/20,
		start = min(values$successful_investments,  na.rm = TRUE),
		showlines = FALSE
		),
	line = list(smoothing = 0.85),

	colorscale = "Greys"
	) %>%
		layout(font = cmodern) %>%
		colorbar(len = 1, nticks = 10, title = "Predicted successful \n investments") %>%
	    layout(yaxis = list(title = "Lagged niche width")) %>%
	    layout(xaxis = list(title = "Lagged status")) 


orca(p1, file = "success_rate_contour_plotly_8_30.pdf")

########## Question 3

##### Part A

# first, set up investor-investor edge list
# two-mode edge list

edges = fread("investors_and_deals.csv", head = TRUE)
colnames(edges) = make.names(colnames(edges))
colnames(edges)[1]="InvestorID"
colnames(edges)[2]="DealID"

# how often do investors usually invest
investors = fread("investor_details.csv", head = TRUE)
colnames(investors) = make.names(colnames(investors))
colnames(investors)[1] = "InvestorID"

# deal info
deals = fread("deal_details.csv", head = TRUE)
colnames(deals) = make.names(colnames(deals))
colnames(deals)[1]="DealID"
colnames(deals)[2]="CompanyID"

# harmonizing/managing field names

colnames(deals)[1] = "DealID"

setkey(investors, InvestorID)
setkey(edges, InvestorID)

edges = merge(edges, investors[, c("InvestorID", "Investor.Type")])



# remove individual investors
investors[Investor.Name == "Individual Investor"]
investors[, investor := destring(InvestorID, keep = "0-9.")]

# merge all info together
setkey(deals, DealID)
setkey(edges, DealID)

edges = merge(edges, deals)

edges[, dealkey := destring(DealID, keep = "0-9.")]
edges[, companykey := destring(CompanyID, keep = "0-9.")]

# for getting time information 
edges[, date := as.Date(Deal.Date, "%m/%d/%Y")]

edges[, year := year(date)]

edges = edges[!is.na(year)]

# separate out by deals for making an investor-investor edge list
edges = split(edges, f = edges$dealkey) # this will take a few minutes

# remove deals with only one investor
index = sapply(seq_along(edges), function(i) nrow(edges[[i]]) > 1)
edges = edges[index]

# make an edge list using t(combn(x,2)) for each deal
edges = lapply(seq_along(edges), function(i) data.table(dealkey = edges[[i]]$dealkey[1], date = edges[[i]]$date[1], year = edges[[i]]$year[1], companykey = edges[[i]]$companykey, t(combn(edges[[i]]$InvestorID, 2))))

edges = rbindlist(edges)

# set up the object to represent each investor's syndicate partnerships
lead = rbindlist(list(edges, edges[,c("dealkey","date","year", "companykey", "V2","V1")]), use.names = FALSE)

# we will measure the distance of the other co-investors in the syndicate from the "focal" investor one at a time
lead[, focal_investor1 := destring(V1, keep = "0-9.")]
lead[, nonfocal_investor2 := destring(V2, keep = "0-9.")]

# get this back in for putting lead investor info in edge list
dealIR = fread("investors_and_deals.csv", header = TRUE)
colnames(dealIR)=gsub(" ","",colnames(dealIR))
colnames(dealIR)[1]="InvestorID"
colnames(dealIR)[2]="DealID"

dealIR[, dealkey := destring(DealID, keep = "0-9.")]
dealIR[, focal_investor1 := destring(InvestorID, keep = "0-9.")]

setkeyv(lead, c("focal_investor1", "dealkey"))
setkeyv(dealIR, c("focal_investor1", "dealkey"))
lead=merge(lead, dealIR [,c("focal_investor1", "dealkey", "LeadInvestor")])

# get company industries
companies = fread("CompanyDetailed.csv", header = TRUE)
companies[, total_industries:= length(unique(Primary_Industry_Code)), by = c("CompanyID")]

# again retain industry info
companies[total_industries > 1, second_industry_code := unique(Primary_Industry_Code)[2], by = c("CompanyID")]

# homogenize within year
companies[total_industries > 1, Primary_Industry_Code := unique(Primary_Industry_Code)[1], by = c("CompanyID")]

# set up key
companies[, companykey := destring(CompanyID, keep = "0-9.")]

# merge to edge list to get industries 
setkey(companies, "companykey")
setkey(lead, "companykey")

lead = merge(lead, companies[, c("companykey", "Primary_Industry_Sector")], all.x = TRUE)

# for multidimensional scaling now set up sparse matrix for industries in each investor's portfolio cumulative through each year
# incidence = lapply(seq_along(years), function(l)
#         sparseMatrix(i = as.numeric(factor(edges_ivs[year <= years[l], Primary_Industry_Sector.company])),
#                 j = as.numeric(factor(edges_ivs[year <= yearly_status.csv[l], Investor_Id])),
#                 x = rep(1, nrow(edges_ivs[year <= years[l]])),
#                 dimnames = list(levels(factor(edges_ivs[year <= years[l], Primary_Industry_Sector.company])), levels(factor(edges_ivs[year <= years[l], Investor_Id])))
#          )
# )
 
# # setting up dist matrix and mdcoords for investors 
# jaccard_dist = list()
# mdscale_year = list()
# mdcoords = list()

# calculate multidimensional scaling from Jaccard similarity of industry categories in portfolio
# for(i in seq_along(years)[(length(years) - 1):length(years)]) {
#     jaccard_dist = dist(as.matrix(incidence[[i]]), method = "jaccard", by_rows = FALSE)
#     mdscale_year = cmdscale(jaccard_dist, k = 2)
#     mdcoords = data.table(investor = names(jaccard_dist), coord1 = mdscale_year[,1], coord2 = mdscale_year[,2], year = edges_ivs[year == years[i], year][1])

# # again, you can use this syntax to check the progress of the loop to make sure it's still running and, if there is an error, to see when and at what iteration it occurred. very helpful when a long, intensive loop is needed
#     print(data.table(year = edges_ivs[year == years[i], year][1], time = Sys.time())) 
#     flush.console()

# #     # if active memory is strained, can also export each iteration individually and then load back in, with something using append = TRUE
#     fwrite(mdcoords, file = "investor_multidimensional_coordinates_yearly_cumulative.csv", append = TRUE)
# }         

# read back in 
mdcoords = fread("investor_multidimensional_coordinates_yearly_cumulative.csv", head = TRUE)

# specify "medoid" for each industry for each year
edges_ivs[, single_industry := length(unique(Primary_Industry_Sector.company)) == 1, by = c("Investor_Id", "year")]

industries = lapply(seq_along(years), function(i) sort(unique(edges_ivs[year == years[i],Primary_Industry_Sector.company])))

# can just take first investor to represent a completely non-diversified strategy in each industry as the medoid
medoids = lapply(seq_along(years), function(i)
  sapply(seq_along(industries[[i]]), function(j) data.table(
    investor = edges_ivs[Primary_Industry_Sector.company == industries[[i]][j] & single_industry == TRUE & year == years[i], Investor_Id][1],
    Primary_Industry_Sector = industries[[i]][j],
    year = years[i]
    )
  )
)

# get back in to a table from the list
medoids = data.table(do.call(rbind, (lapply(seq_along(medoids), function(i) t(medoids[[i]])))))
# check result
medoids
# one entry is missing, meaning that no investor was completely non-diversified in it

# can fix last one manually by assigning medoid as investor most concentrated in the industry at this time
edges_ivs[Primary_Industry_Sector.company == "Materials and Resources", materials_count := .N, by = c("Investor_Id", "year")]
edges_ivs[, max_materials_count := materials_count == max(materials_count, na.rm = TRUE), by = c("year")]
medoids[is.na(investor), investor := edges_ivs[year == years[8] & max_materials_count == TRUE, Investor_Id]]

# for merging, make sure column type isn't still stored as list type
medoids[, investor := as.character(investor)]
medoids[, Primary_Industry_Sector := as.character(Primary_Industry_Sector)]
medoids[, year := as.numeric(year)]
# assign each sector a set of coordinates based on the medoid coordinates
setkeyv(medoids, c("investor", "year"))
setkeyv(mdcoords, c("investor", "year"))

# get the coordinates of each industry category based on the medoid
medoids = merge(medoids, mdcoords)

colnames(medoids)[colnames(medoids) %in% c("coord1", "coord2")] = c("industry_coord1", "industry_coord2")

# merge in the industry category medoids to the edge list
setkeyv(medoids, c("Primary_Industry_Sector", "year"))
setkeyv(lead, c("Primary_Industry_Sector", "year"))

lead = merge(lead, medoids[,-c("investor")])

# now get individual investor coordinates for the focal investor
mdcoords[, focal_investor1 := destring(investor, keep = "0-9.")]
colnames(mdcoords)[colnames(mdcoords) == "coord1"] = "coord1_focal"
colnames(mdcoords)[colnames(mdcoords) == "coord2"] = "coord2_focal"

setkeyv(mdcoords, c("focal_investor1", "year"))
setkeyv(lead, c("focal_investor1", "year"))


lead = merge(lead, mdcoords[,-c("investor")])

# and for the other nonfocal investors in the syndicate
colnames(mdcoords)[colnames(mdcoords) == "focal_investor1"] = "nonfocal_investor2"
colnames(mdcoords)[colnames(mdcoords) == "coord1_focal"] = "coord1_nonfocal"
colnames(mdcoords)[colnames(mdcoords) == "coord2_focal"] = "coord2_nonfocal"
setkeyv(mdcoords, c("nonfocal_investor2", "year"))
setkeyv(lead, c("nonfocal_investor2", "year"))

lead = merge(lead, mdcoords[,-c("investor")])


# calculate distance of portfolio from investment as Euclidean distance for each investor using dist()
# need to pass dist a matrix, so can make one based on the coordinates in each row
# each will take a couple minutes for all the rows
lead[, focal_dist := sapply(seq_len(nrow(lead)), function(i) dist(rbind(c(lead$industry_coord1[i], lead$industry_coord2[i]), c(lead$coord1_focal[i], lead$coord2_focal[i])))[1])]
lead[, nonfocal_dist := sapply(seq_len(nrow(lead)), function(i) dist(rbind(c(lead$industry_coord1[i], lead$industry_coord2[i]), c(lead$coord1_nonfocal[i], lead$coord2_nonfocal[i])))[1])]

# now, collapse into investor-deal format and merge with previous data
# while doing so, calculate average distance of the non-focal investors for each focal investor for each deal
investor_deals = unique(lead[, list(LeadInvestor, year, focal_dist, average_synd_dist = mean(nonfocal_dist, na.rm = TRUE)), by = c("focal_investor1", "dealkey")])

# merge the distance information back with the main data with the status variables and controls
investors_panel[, investor := destring(Investor_Id, keep = "0-9.")]
colnames(investor_deals)[colnames(investor_deals) == "focal_investor1"] = "investor"

setkeyv(investor_deals, c("investor", "year"))
setkeyv(investors_panel, c("investor", "year"))

investor_deals = merge(investor_deals, investors_panel)

# could export this at the deal level here
fwrite(investor_deals, "investor_deal_panel.csv")

# collapse to investor-year level
investor_dist_year = unique(investor_deals[LeadInvestor == 1, list(l_status, focal_dist = mean(focal_dist, na.rm = TRUE), average_synd_dist = average_synd_dist/.N, l_originate, l_IT, l_early_stage, age, l_niche_width_bar), by = c("investor", "year")])

# run regression with interaction and fixed effects
summary(plm(average_synd_dist ~ l_status + focal_dist + l_status:focal_dist + l_originate + l_IT + l_early_stage + age + year, model = "within", effect = "individual", data = investor_dist_year, index = c("investor")))

fwrite(investor_dist_year, "lead_investments_panel.csv")

##### Part B

# can follow same process as 2B
m4 = lm(average_synd_dist ~ l_status + focal_dist + l_status:focal_dist, data = investor_dist_year)

# set up scaled grid of (x,y) values
values = expand.grid(l_status=seq(min(investor_dist_year$l_status, na.rm = TRUE), max(investor_dist_year$l_status, na.rm = TRUE), length.out = 100), focal_dist=seq(min(investor_dist_year$focal_dist, na.rm = TRUE), max(investor_dist_year$focal_dist, na.rm = TRUE), length.out = 100))


# prediction from the model
values$average_synd_dist = predict(m4,newdata=values)

# new device for larger visualization
dev.new(height = 12, width = 12, units = "in")
par(family = "CMU Serif") # setting a latex-like font

# set up plot with x, y, and z variables
scatter3D(values$l_status, values$focal_dist, values$average_synd_dist, phi = 0, bty = "b2", xlab = expression("Lagged firm status"), ylab = "Firm distance from deal", zlab = "Syndicate distance from deal", cex.lab = 1, ticktype = "detailed", colkey = list(cex.clab = 1, length = .5, font = 1), clab = c("Predicted values", "of interaction"))

# setting up a contour plot

p2 = plot_ly(
  values,
  x = ~l_status,
  y = ~focal_dist,
  z = ~average_synd_dist,
  type = "contour",
  autocontour = FALSE,
  contours = list(
    end = max(values$average_synd_dist, na.rm = TRUE),
    size = abs(max(values$average_synd_dist,  na.rm = TRUE) - min(values$average_synd_dist,  na.rm = TRUE))/20,
    start = min(values$average_synd_dist,  na.rm = TRUE),
    showlines = FALSE
    ),
  line = list(smoothing = 0.85),

  colorscale = "Greys"
  ) %>%
    layout(font = cmodern) %>%
    colorbar(len = 1, nticks = 10, title = "Predicted syndicate \n distance") %>%
      layout(yaxis = list(title = "Firm distance from deal")) %>%
      layout(xaxis = list(title = "Lagged status")) 


orca(p2, file = "success_rate_contour_plotly_8_30.pdf")

