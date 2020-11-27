## Preqin venture deals through late July 2014
rm(list = ls(all = TRUE))
setwd("~/Dropbox")
setwd("C:/Users/Frank/Desktop/Social Network Analytics/Assignment 2")

library(data.table)
library(igraph)
library(stringr)
library(splitstackshape)
library(plotly)
library(network)
library(lubridate)

############## Data prep

# key insight -- ignore the startups, investor-investor relationships are given in investor column of affiliation edge list. from here, follow the same procedure as for the advice networks from exercise 1 to make the investor-investor edge list

# making monthly investor network
investments1 = fread(file = "Funding_events_7.14.csv", header = TRUE)
investments2 = fread(file = "Funding_events_7.14_page2.csv", header = TRUE)

investments = rbindlist(list(investments1, investments2))

# getting months
investments[, date := as.Date(investments$"Deal Date", "%m/%d/%y")]

# floor_date() from lubridate rounds down to the nearest month, pulling out the calendar month of each funding event
investments[, month := floor_date(date, "month")]

# make an indexing object to use for apply statements later on -- make sure that months without investments are included for continuity
months = seq.Date(min(investments$month), max(investments$month), by = "month")

# focus on investors column, maintain month to track network over time
investors = data.table(investors = investments$Investors, month = investments$month)

# split out investors into their own separate columns to make an edge list. can use commas to separate, but need to be careful of investors with suffixes like inc, ltd, llc, and others. use splitstackshape's cSplit to make this simpler
investors = cSplit(investors, "investors", ",")

# warnings should be okay but you can avoid by replacing any non UTF-8 by ''

investors = data.table(investors = investments$Investors, month = investments$month)
Encoding(investors$investors) = "UTF-8"
investors[, investors := iconv(investors, "UTF-8", "UTF-8",sub="")]
# now no warnings


# can set up an edge list using a similar approach as the advice network method from the in-class exercise on the classroom network
# key for this method is to combine that setup with the t(combn()) method from exercise 1 question 2 (question on checking for strong/weak ties)

investors = t(cSplit(investors, "investors", ","))

# get all pairs
possible_pairs = lapply(seq_len(ncol(investors)), function(i) investors[,i])

# note the difference in speed between the above and this:
# investors = data.table(investors = investments$Investors, month = investments$month)
# investors = cSplit(investors, "investors", ",")
# possible_pairs = lapply(seq_len(nrow(investors)), function(i) t(investors[i,]))
# helpful to avoid vectoring on rows with apply functions when possible

# get rid of blanks to make the object smaller
for(i in seq_along(possible_pairs)){
	possible_pairs[[i]] = possible_pairs[[i]][!is.na(possible_pairs[[i]])]
}

# ignore funding events where there is only one investor
# can use logical index as in outlook email network setup
index = sapply(seq_along(possible_pairs), function(i) length(possible_pairs[[i]]) > 2)
possible_pairs = possible_pairs[index]

edges = lapply(seq_along(possible_pairs), function(i) data.table(t(combn(possible_pairs[[i]][-1], 2)), month = as.Date(possible_pairs[[i]][1], "%Y-%m-%d")))

# calling data table on this just so we can refer to column names and make the rest of the code more interpetable
# main issue is that it's not possible to call $ on matrix-type objects, even if they have explicit column names
edges = rbindlist(edges)

colnames(edges) = c("from", "to", "month")

# keeping track of our string splitting from before, we want to account for when commas are included in the name
# we're worried mostly about strings of length 3 or 4
unique(c(edges$from[nchar(edges$from) < 5], edges$to[nchar(edges$to) < 5]))

# after a quick scan, let's say that the problem strings are <NA>, Ltd., Inc., LLC, Inc, LP, LLC., Ltd, L.P., S.A, Corp, a.s., llc, S.A., and LTD

# removing rows where either entry in the edge list doesn't represent an actual company
edges = edges[from != "<NA>" & from != "Ltd." & from != "Inc." & from != "LLC" & from != "Inc" & from != "LP" & from != "LLC." & from != "Ltd" & from != "L.P." & from != "S.A" & from != "Corp" & from != "a.s." & from != "llc" & from != "S.A." & from != "LTD" & to != "<NA>" & to != "Ltd." & to != "Inc." & to != "LLC" & to != "Inc" & to != "LP" & to != "LLC." & to != "Ltd" & to != "L.P." & to != "S.A" & to != "Corp" & to != "a.s." & to != "llc" & to != "S.A." & to != "LTD" ,]

# now let's make a network for each month

# just get the edges up to a given month, for each month in the data
edges_monthly = lapply(seq_along(months), function(i) edges[month <= months[i]])

# feed these into igraph -- will use the full network but simplified is also okay
vcnets = lapply(seq_along(edges_monthly), function(i) graph.data.frame(edges_monthly[[i]][,c("from","to")], directed = FALSE))
vcnets_simple = lapply(seq_along(vcnets), function(i) simplify(vcnets[[i]], remove.multiple = TRUE))

############## Question 1
 
####### Part A

# key insight -- setting up our monthly networks in a list as above make for quick calculations on the months we're interested in using the built-in function in igraph, because we avoid having to make matrices or re-generate a network for each step

# center of the network in the same terms as the Hollywood Actor example is VC firm with highest closeness
closeness(vcnets[[length(vcnets)]])[which.max(closeness(vcnets[[length(vcnets)]]))] # length(vcnets) will give the index of the last month in the data

####### Part B

# can also do part A with the "bacon" numbers, that is, lowest average shortest path distance
# all shortest paths 
distances = distances(vcnets[[length(vcnets)]])

# replace unreachable firms with just the size of the network to get a real number solution--infinity also an acceptable answer for the avearage distance
distances[distances == Inf] = nrow(distances)

# average shortest path length for each firm -- the "bacon" number
avg_dist_firm = apply(distances, 1, mean)

# firm with the smallest average shortest path length
avg_dist_firm[which.min(avg_dist_firm)]

####### Part C

# average shortest path length for all firms
mean(avg_dist_firm)

# number is so high because many firms are not reachable from one another
distances = distances(vcnets[[length(vcnets)]])
sum(distances(vcnets[[length(vcnets)]]) == Inf)/2

# tells us that there are 6 million unreachable pairs in the network, so this will inflate this shortest path calculation by a lot even if a lot of the distances are very small

# note that if we included isolates, the distances would grow even larger



############## Question 2
 
####### Part A
# key insight -- same as above, having a list of networks to work from makes the calculations quicker and can be done in igraph
# compute mean highest-degree kcore for each month
mean_kcore = lapply(seq_along(vcnets), function(i) data.table(month = months[i], avg_kcore = mean(coreness(vcnets[[i]]))))
mean_kcore = rbindlist(mean_kcore)

# plot
p = plot_ly(mean_kcore, x = ~month, y = ~avg_kcore, color = I("gray50"), type = "scatter", mode = "lines") %>%
	layout(yaxis = list(title = "Average coreness")) %>%
	layout(xaxis = list(title = "Year"))
orca(p, file = "coreness_over_time.pdf")
# exporting may require pandoc to be installed -- can also call plot to a web browser by calling the plot function (or whatever object the plot is assigned to) to the console



####### Part B
# can check part A against simplified version
mean_kcore_simple = lapply(seq_along(vcnets), function(i) data.table(month = months[i], avg_kcore = mean(coreness(vcnets_simple[[i]]))))
mean_kcore_simple = rbindlist(mean_kcore_simple)

p2 = plot_ly(mean_kcore_simple, x = ~month, y = ~avg_kcore, color = I("gray50"), type = "scatter", mode = "lines") %>%
  layout(yaxis = list(title = "Average coreness")) %>%
  layout(xaxis = list(title = "Year"))
orca(p2, file = "coreness_over_time_simple.pdf")
# sparser graph if repeated ties are not included in k-core calculation and upward trend flattens out towards the end

###### Part C
# key insight -- similar to exercise 1, edge lists are easier to work with than matrices when we need to keep track of attributes of the edges

# modify edges_monthly to remove ties if they are at least five years old and have not been renewed

# create a year column
for(i in seq_along(edges_monthly)){
  edges_monthly[[i]][, year := year(month)]
}

# also a year index
years = year(months)

edges_monthly_decay = lapply(seq_along(edges_monthly), function(i) edges_monthly[[i]][, tie_age := years[i] - year])

edges_monthly_decay = lapply(seq_along(edges_monthly), function(i) edges_monthly_decay[[i]][tie_age < 6,])

vcnets_decay = lapply(seq_along(edges_monthly), function(i) graph.data.frame(edges_monthly_decay[[i]][,c("from", "to")], directed = FALSE))

# same plot as in part A
mean_kcore_decay = lapply(seq_along(vcnets), function(i) data.table(month = months[i], avg_kcore = mean(coreness(vcnets_decay[[i]]))))

mean_kcore_decay = rbindlist(mean_kcore_decay)

# plot
p3 = plot_ly(mean_kcore_decay, x = ~month, y = ~avg_kcore, color = I("gray50"), type = "scatter", mode = "lines") %>%
  layout(yaxis = list(title = "Average coreness")) %>%
  layout(xaxis = list(title = "Year"))
orca(p3, file = "coreness_over_time_decay.pdf")
# sparser graph if repeated ties are not included in k-core calculation and upward trend flattens out towards the end

# comparing the average coreness for each of the modified networks against the unsimplified one with cumulative ties
t.test(mean_kcore$avg_kcore, mean_kcore_decay$avg_kcore)
t.test(mean_kcore$avg_kcore, mean_kcore_simple$avg_kcore)

# in both cases, average k-cores are higher for network that includes all ties and does not incorporate tie decay
# suggests that network may not be continuing to grow as dense if we are only considering unique ties and ties that are maintained over time,
# considering only ties which are maintained over time, in particular, could be a better measure of actual firm interaction and relationships in this industry because it allows firms that go out of business to drop out of the network over time


############## Question 3

###### Part A

# using the network incorporating tie decay
# get eigenvector centralities to measure global coreness
eigen_yearly = lapply(seq_along(vcnets), function(i) data.table(node = V(vcnets_decay[[i]])$name, eigcent = eigen_centrality(vcnets_decay[[i]])$vector, month = i))
eigen_yearly = rbindlist(eigen_yearly)
setorderv(eigen_yearly, c("month", "eigcent"), c(1,-1))


# calculate concentration as the correlation between the computed coreness scores
cor_eigen = lapply(seq_along(vcnets), function(i)
  sapply(seq_len(nrow(eigen_yearly[month == i])), function(j)
    cor(eigen_yearly[month == i, eigcent], c(rep(1,j), rep(0,nrow(eigen_yearly[month == i]) - j)))
    )
  )


save(cor_eigen, file = "concentration_scores.RData")


# ideal partition for each month starting from month 14
ideal_partitions_cor_eigen = lapply(seq_along(vcnets)[-(1:13)], function(i) data.table(max_concentration = max(cor_eigen[[i]], na.rm = TRUE), ideal_partition = which.max(cor_eigen[[i]]), ideal_partition_prop = which.max(cor_eigen[[i]])/length(cor_eigen[[i]]), month = months[i]))

ideal_partitions_cor_eigen = rbindlist(ideal_partitions_cor_eigen)
ideal_partitions_cor_eigen

# in general ideal partition occurs between 1 and n nodes after the very early period, so suggests presence of core-periphery structure

cor_eigen_dt = lapply(seq_along(cor_eigen), function(i) data.table(index = seq_along(cor_eigen[[i]]), concentration = cor_eigen[[i]], month = i))
cor_eigen_dt = rbindlist(cor_eigen_dt)

cor_eigen_dt[, index_prop := index/max(index), by = month]

# plot ideal partitions over time -- can get 1 per 12 months in the data using the modular division operator %%
conc_plots = lapply(seq_along(vcnets)[-(1:13)][c(seq_along(vcnets)[-(1:13)]%%12 == 2)], function(i)
  ggplot(cor_eigen_dt[month == i]) + geom_point(aes(x=index_prop, y = concentration), size = .5, color = "Grey50") + xlab("Proportion of firms in core partition") + ylab("Concentration of network") + theme_bw() + theme(text=element_text(family="CMU Serif", size=6), axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) + labs(subtitle = paste0("Year ",(seq_along(vcnets)%/%12 + 1981)[i])) + theme(plot.subtitle = element_text(hjust = 0.5)) + ylim(c(0,1))
  )
# the %/% provides integer division to get the correct year on the plot

# can plot on one device using grid.arrange from gridExtra
# this will make 7 rows with 5 columns each for 35 plots in total on the figure
dev.new(width = 8.5, height = 11, unit = "in")
grid.arrange(
  conc_plots[[1]],
  conc_plots[[2]],
  conc_plots[[3]],
  conc_plots[[4]],
  conc_plots[[5]],
  conc_plots[[6]],
  conc_plots[[7]],
  conc_plots[[8]],
  conc_plots[[9]],
  conc_plots[[10]],
  conc_plots[[11]],
  conc_plots[[12]],
  conc_plots[[13]],
  conc_plots[[14]],
  conc_plots[[15]],
  conc_plots[[16]],
  conc_plots[[17]],
  conc_plots[[18]],
  conc_plots[[19]],
  conc_plots[[20]],
  conc_plots[[21]],
  conc_plots[[22]],
  conc_plots[[23]],
  conc_plots[[24]],
  conc_plots[[25]],
  conc_plots[[26]],
  conc_plots[[27]],
  conc_plots[[28]],
  conc_plots[[29]],
  conc_plots[[30]],
  conc_plots[[31]],
  conc_plots[[32]],
  conc_plots[[33]],
  ncol = 5)

# network tends to conform to a core-periphery structure after the early period

# can also plot concentration and number of nodes in it 
dev.new(width = 6, height = 8, unit = "in")

# also using grid.arrange to get on the same figure
grid.arrange(
  ggplot(ideal_partitions_cor_eigen) + geom_point(aes(x = month, y = max_concentration), color = "Grey50", size = 2) +  geom_line(aes(x = month, y = max_concentration), color = "Grey50", size = 1) +  xlab("Network year") + ylab("Concentration of network, ideal partition") + theme_bw() + theme(text=element_text(family="CMU Serif", size=10), axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) #+ scale_x_continuous(breaks = c(1980,1985,1990,1995,2000,2005,2010,2016))
  ,

  ggplot(ideal_partitions_cor_eigen) + geom_point(aes(x = month, y = ideal_partition_prop), color = "Grey50", size = 2) +  geom_line(aes(x = month, y = ideal_partition_prop), color = "Grey50", size = 1) +  xlab("Network year") + ylab("Number of core firms in ideal partition") + theme_bw() + theme(text=element_text(family="CMU Serif", size=10), axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) #+ scale_x_continuous(breaks = c(1980,1985,1990,1995,2000,2005,2010,2016))
  ,
  ncol = 1
)

#  network concentration grows seems to growing since the 2010s began
# proportion of firms that are classified in the ideal partition falls over time after the earlier period
# comparing to the more local measurement of k-cores, the global measure of coreness suggests that the network is overall more dense in terms of group membership, but access to the global core remains only in the hands of a few firms

###### Part B

# key insight -- can use variety of measures to talk about core/periphery structure, including 

# 1. show development of core/periphery visually using a network plot
# can compare early plots to late plots using eigenvector/coreness as a color, for example through an animation like the one from the diffusion example in class

library(animation)

# delay between frames when replaying
ani.options(interval=.15)

# animation loop
saveGIF({
  
layout(matrix(c(1, rep(2, 5)), 6, 1))
  
# adjust the margins
par(mar=c(4,4,2,1) + 0.1)

for(i in seq_along(vcnets_decay)){
plot(-5, xlim = c(1,length(vcnets)), ylim = c(0, .3), axes = F, xlab = "", ylab = "", main = "Date")

abline(v=i, lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
abline(v=i-1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
abline(v=i-2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
axis(1)

colbar = 

plot(vcnets_decay[[i]], vertex.size=3, vertex.color=rainbow(length(unique(eigen_yearly[month == i, eigcent])))[eigen_yearly[month == i, as.factor(eigcent)]], vertex.frame.color=rainbow(length(unique(eigen_yearly[month == i, eigcent])))[eigen_yearly[month == i, as.factor(eigcent)]], vertex.label = NA)
}
}, movie.name = "coinvestment_over_time.gif")


# can see development of dense core over time
# plots will get slow after 15 years but will still plot if given time

# 2. develop a plot or descriptive statistics of how many nodes are included in different levels of k-cores
# can look at distribution of coreness to see if it is evenly dispersed or not
june_1982 = 12*1
june_1986 = 12*5
june_1991 = 12*10
june_1996 = 12*15
june_2001 = 12*20
june_2006 = 12*25

table(coreness(vcnets[[june_1982]]))
table(coreness(vcnets[[june_1986]]))
table(coreness(vcnets[[june_1991]]))
table(coreness(vcnets[[june_1996]]))
table(coreness(vcnets[[june_2001]]))
table(coreness(vcnets[[june_2006]]))

# firms become members of higher-degree k-cores over time -- 62 firms in k-core with degree 77 in 2006


# 3. can look at the distribution of closness, in the sense that if many firms have very high coreness and some very few firms have very low coreness then that would also suggest that many firms are in the core and then a few are scattered in the periphery
par(mfrow=c(2,3), mar = c(0,0,0,0)) 
hist(closeness(vcnets[[june_1982]]))
hist(closeness(vcnets[[june_1986]]))
hist(closeness(vcnets[[june_1991]]))
hist(closeness(vcnets[[june_1996]]))
hist(closeness(vcnets[[june_2001]]))
hist(closeness(vcnets[[june_2006]]))
dev.off()

# can see split of closeness statistics emerge over time

# 4. perform a multidimensional scaling on the distance between to see if many firms appear in the center of the plot and there are some other firms scattered, maybe in a ring, etc. outside of this
dist = distances(vcnets[[june_1982]])
dist[dist == Inf] = nrow(dist)
mdscale1 = cmdscale(dist, k = 2)
dist = distances(vcnets[[june_1986]])
dist[dist == Inf] = nrow(dist)
mdscale5 = cmdscale(dist, k = 2)
dist = distances(vcnets[[june_1991]])
dist[dist == Inf] = nrow(dist)
mdscale10 = cmdscale(dist, k = 2)
dist = distances(vcnets[[june_1996]])
dist[dist == Inf] = nrow(dist)
mdscale15 = cmdscale(dist, k = 2)

par(mfrow=c(2,2), mar = c(0,0,0,0)) 
plot(mdscale1)
plot(mdscale5)
plot(mdscale10)
plot(mdscale15)
dev.off()

# actually hard to see on the plot, but look at the coordinates from the last scaling
mdscale15

# many firms located on top of each other in same location, and a few firms scattered outside of this region

# 5. decompose the network into its largest connected component--the “giant component”--and see if this component contains most of the firms in the network
# get components and compare size of largest component to number of firms in the network
components = clusters(vcnets[[june_1991]])
max(components$csize)/vcount(vcnets[[june_1991]])

components = clusters(vcnets[[june_1996]])
max(components$csize)/vcount(vcnets[[june_1996]])
# percentage jumps over 2x from 1991 to 1996

# from the above, some of these may be more or less memory intensive, and may make sense on larger or smaller networks

############## Question 4
 
####### Data prep

# successful investments 
outcomes = fread("Venture_capital_firm_outcomes.csv", head = TRUE)

# get network info into the outcomes table

# can use this function again
getNetStats=function(net)
{
  deg = degree(net, mode = "total")
  close= closeness(net, mode = "total")
  betw = betweenness(net)
  prank = page_rank(net)$vector # page_rank creates a list object with some other items in it, we just want the actual scores, which are contained in the object called "vector"
  eigcent= eigen_centrality(net)$vector # page_raeigen_centralitynk creates a list object with some other items in it, we just want the actual scores, which are contained in the object called "vector"
  id=V(net)$name
  stats= as.data.table(list(firm_name = id, deg = deg, close = close, betw = betw, prank = prank, eigcent = eigcent))
  return(stats)
}

# note that only need yearly stats, so can just take one stat per year from 1981-2014
investments[which.min(date)]
# network begins in June 1981

index = seq_along(vcnets)[c(seq_along(vcnets)%%12 == 1)]
year_nets = vcnets[index]
vc_centralities = lapply(seq_along(year_nets), function(i) getNetStats(year_nets[[i]]))

# add the years
for(i in seq_along(vc_centralities)){
	vc_centralities[[i]][, year := seq(min(year(edges$month)), max(year(edges$month)))[i]]
}
vc_centralities = rbindlist(vc_centralities)

# merge together
setkeyv(outcomes, c("firm_name", "year"))
setkeyv(vc_centralities, c("firm_name", "year"))

outcomes = merge(outcomes, vc_centralities)

####### Part A

# successful investments is somewhat right-skewed, so can use quantile regression adopted for counts to estimate the median of successful investments, rather than the mean as in a normal regression
# the count regressions from the pscl are also a good choice, in lieu of a regular linear regression
hist(outcomes$successful_investments)

library(Qtools)

rq.counts(successful_investments ~ close +  corporate_venture_firm + monetary_size_deals_year_usdmn + early_stage_investor + year, data = outcomes, tau = .5, M = 100)
rq.counts(successful_investments ~ deg +  corporate_venture_firm + monetary_size_deals_year_usdmn + early_stage_investor + year, data = outcomes, tau = .5, M = 100)
rq.counts(successful_investments ~ betw +  corporate_venture_firm + monetary_size_deals_year_usdmn + early_stage_investor + year, data = outcomes, tau = .5, M = 100)
rq.counts(successful_investments ~ prank +  corporate_venture_firm + monetary_size_deals_year_usdmn + early_stage_investor + year, data = outcomes, tau = .5, M = 100)
rq.counts(successful_investments ~ eigcent +  corporate_venture_firm + monetary_size_deals_year_usdmn + early_stage_investor + year, data = outcomes, tau = .5, M = 100)
# if the models return errors about singular designs, can re-run until they converge

# closeness not related to succesful investments, but other types of centrality seem to be

####### Part B

# a firm can only go out of business once, and its likelihood of going out of business is also related to how long it has been in operation
# can use the set of "survival", or "hazard" models to estimate these kinds of failure events effectively

# a simple survival model is a discrete-time model where we estimate a logit on going out of business, controlling for the tenure-specific rate of failure for any firm that has been in operation for a tenure of x years
outcomes[, tenure := year - first_investment_year]

# the regressions from the survival package are also a good choice
# can also include some controls for industry and location
summary(glm(out_of_business ~ close +  corporate_venture_firm + monetary_size_deals_year_usdmn + early_stage_investor + year + factor(tenure), data = outcomes, family = "binomial"))
summary(glm(out_of_business ~ deg +  corporate_venture_firm + monetary_size_deals_year_usdmn + early_stage_investor + year + factor(venture_firm_industry) + factor(venture_firm_location) + factor(tenure), data = outcomes, family = "binomial"))
summary(glm(out_of_business ~ betw +  corporate_venture_firm + monetary_size_deals_year_usdmn + early_stage_investor + year + factor(venture_firm_industry) + factor(venture_firm_location) + factor(tenure), data = outcomes, family = "binomial"))
summary(glm(out_of_business ~ prank +  corporate_venture_firm + monetary_size_deals_year_usdmn + early_stage_investor + year + factor(venture_firm_industry) + factor(venture_firm_location) + factor(tenure), data = outcomes, family = "binomial"))
summary(glm(out_of_business ~ eigcent +  corporate_venture_firm + monetary_size_deals_year_usdmn + early_stage_investor + year + factor(venture_firm_industry) + factor(venture_firm_location) + factor(tenure), data = outcomes, family = "binomial"))

# all types of centrality are related to being less likely to go out of business, in varying degrees