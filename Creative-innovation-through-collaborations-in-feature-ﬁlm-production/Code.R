rm(list = ls(all = TRUE))
setwd("C:/Users/dplewi3/Dropbox")
setwd("~/Dropbox")


library(taRifx)
library(data.table)
library(zoo)
library(igraph)
library(plotly)
library(ggplot2)
library(Matrix)
library(proxy)
library(MASS)

############## Data prep/Question 1

# key insight -- having a plan for setting up our objects the first time will allow us to re-use them many times for the remainder of the questions, building the data objects we need as we go along. it can be helpful to keep in mind what data pieces will be needed for the future--it's also okay to reset the data to the original form, but make sure your steps are modular and can run quickly enough so that you can apply these smoothly to the reset object when you repurpose it to avoid having to do a lot of extra work. many of the steps flow together so can do a few things at the same time. it's helpful to be able to work along the three main dimensions of the data: (1) producers, (2) films, and (3) keywords, using subsetting and grouping syntax

# can start by defining film categories, using producer economies of scale and coreness in co-production network of producers

# first, generalism based on core-periphery position in the collaboration network

# make a network out of producer edge list
film_producer_history = fread("producers_and_films.csv", head = TRUE)

film_producer_history[, us := sum(country != "us" & country != "") == 0, by = pindex]
film_producer_history = film_producer_history[us == 1]

film_producer_history[, dup := .N , by = c("pcindex", "pindex")]
film_producer_history = film_producer_history[dup == 1]

film_producer_history[, num_producers := .N, by = pindex]

film_producer_history_coprods = film_producer_history[num_producers > 1]

# and expand to get the producer pairs for each film
film_producer_history_split = split(film_producer_history_coprods, f = film_producer_history_coprods$pindex)

producer_pairs_film = lapply(seq_along(film_producer_history_split), function(i) t(combn(film_producer_history_split[[i]]$pcindex, 2)))

producer_pairs_film_dt = lapply(seq_along(film_producer_history_split), function(i) data.table(producer1 = producer_pairs_film[[i]][,1], producer2 = producer_pairs_film[[i]][,2], pindex = film_producer_history_split[[i]]$pindex[1], year =film_producer_history_split[[i]]$year[1]))

producer_pairs_film_dt = rbindlist(producer_pairs_film_dt)

# ordering pcindex alphabetically by column 1 to column 2 for making the network undirected
producer_pairs_film_dt = rbindlist(list(producer_pairs_film_dt, data.table(producer_pairs_film_dt[, c("producer2", "producer1", "pindex", "year")])), use.names = FALSE)

producer_pairs_film_dt = producer_pairs_film_dt[producer1 < producer2]

# now can count unique pairs
setorderv(producer_pairs_film_dt, c("producer1", "producer2", "pindex"))
producer_pairs_film_dt[, weight := seq_len(.N), by = c("producer1", "producer2")]


years = sort(unique(producer_pairs_film_dt$year))
years = years[years <= 2016]

# cumulative yearly network 
edges_yearly = lapply(seq_along(years), function(i) producer_pairs_film_dt[year <= years[i]])

# can use a 10-year renewal period to get rid of decayed ties, no need to include this portion but helps to get rid of companies that are no longer in business
for(i in seq_along(years)){
    edges_yearly[[i]][, tie_age := years[i] - year]
    edges_yearly[[i]] = edges_yearly[[i]][tie_age <= 10]
}

producer_net = lapply(seq_along(years), function(i) graph.data.frame(edges_yearly[[i]][,c("producer1","producer2","weight")], directed = FALSE))

# use eigenvector centrality to get coreness for each year
corenesses = lapply(seq_along(years), function(i) eigen_centrality(producer_net[[i]])$vector)

corenesses = lapply(seq_along(years), function(i) data.table(pcindex = names(corenesses[[i]]), coreness = corenesses[[i]], year = years[i]))

# set up object to merge back into main data
corenesses = rbindlist(corenesses)

# sort for taking prior average within producer
setorderv(corenesses, c("pcindex", "year"))

# fill non-existent rows in the prior average with 0s
corenesses[, coreness_history := rollapplyr(coreness, list(-(10:1)), mean, partial = TRUE, fill = 0), by = pcindex]

# set threshold
corenesses[, generalist_net_upperq := coreness_history > quantile(coreness_history, .75), by = year]


# second, generalism based on producer scale, incorporating box office information along the way
box = fread("box_office_revenues.csv", header = TRUE, key = "pindex")

producers = fread("producers_and_films.csv", header = TRUE, key = "pindex")

pbox = merge(producers, box)

# only keep us producers 
pbox[, us := sum(country != "us" & country != "") == 0, by = pindex]

pbox = pbox[us == 1]

# yearly box office for each producer, as well as the screen release coverage
yearly_box = unique(pbox[, list(total_box = sum(total_box), count_films = .N, release_coverage = sum(release_coverage), budget = sum(budget)), by = c("pcindex", "year")])

# note that this way we could have defined a generalist by box office as well
yearly_box[, generalist_count := count_films > quantile(count_films, .75), by = year]
yearly_box[, generalist_box := total_box > quantile(total_box, .75), by = year]

setkeyv(yearly_box, c("pcindex", "year"))
setkeyv(pbox, c("pcindex", "year"))
film_category = merge(pbox, yearly_box)

# merge with corenesses from before
setkeyv(corenesses, c("pcindex", "year"))
film_category = merge(film_category, corenesses, all.x = TRUE)
film_category[is.na(generalist_net_upperq), generalist_net_upperq := 0]

film_category[, coproduction := .N > 1, by = pindex]

film_category = unique(film_category[, list(generalist_film_count = sum(generalist_count) == .N, specialist_film_count = sum(generalist_count) == 0, hybrid_film_count = sum(generalist_count) > 0 & sum(generalist_count) < .N, generalist_film_net = sum(generalist_net_upperq, na.rm = TRUE) == .N, specialist_film_net = sum(generalist_net_upperq, na.rm = TRUE) == 0, hybrid_film_net = sum(generalist_net_upperq, na.rm = TRUE) > 0 & sum(generalist_net_upperq, na.rm = TRUE) < .N, coproduction = coproduction), by = pindex])

# 5 solo and coproduction types for each definition of generalism
film_category[generalist_film_count == TRUE & coproduction == 0, category_count := "gsolo_count"]
film_category[specialist_film_count == TRUE & coproduction == 0, category_count := "ssolo_count"]
film_category[generalist_film_count == TRUE & coproduction == 1, category_count := "gcoprod_count"]
film_category[specialist_film_count == TRUE & coproduction == 1, category_count := "scoprod_count"]
film_category[hybrid_film_count == TRUE, category_count := "hcoprod_count"]

film_category[generalist_film_net == TRUE & coproduction == 0, category_net := "gsolo_net"]
film_category[specialist_film_net == TRUE & coproduction == 0, category_net := "ssolo_net"]
film_category[generalist_film_net == TRUE & coproduction == 1, category_net := "gcoprod_net"]
film_category[specialist_film_net == TRUE & coproduction == 1, category_net := "scoprod_net"]
film_category[hybrid_film_net == TRUE, category_net := "hcoprod_net"]

# get rid of extra columns
film_category = film_category[, c("pindex", "category_count", "category_net")]


####### Part A
# next, count the new keywords, using the original keyword data
keywords = fread("film_keywords.csv", head = TRUE)

# can exclude some non-descriptive keywords that don't give too much information about plot and style -- this step is not necessary for the assignment but may help for the analysis

# can do this based on an index and a key in data table
index = c("one-word-title", "title-spoken-by-character", "character-name-in-title", "cult-film", "independent-film", "two-word-title", "box-office-flop", "box-office-hit", "character-repeating-someone-else's-dialogue", "three-word-title", "written-by-director", "place-name-in-title", "voice-over-narration", "no-opening-credits", "number-in-title", "product-placement", "blockbuster", "subtitled-scene", "opening-action-scene", "title-directed-by-female", "scene-during-end-credits")

setkey(keywords, keyword)
keywords = keywords[!index]

# also subsetting to just us producers -- can do this by expanding keyword data by producers and then shrinking it again

producers[, producer_seq := seq_len(.N), by = pindex]
setkey(keywords, pindex)

# this is sort of a "master" object in the sense that it contains all three dimensions: (1) films, (2) producers, and (3) keywords, so it is useful to return to for various information about any of the one dimensions that relates to the other two
keywords_producers = lapply(seq_len(max(producers$producer_seq)), function(i) merge(producers[producer_seq == i], keywords))
keywords_producers = rbindlist(keywords_producers)

keywords_producers[, us := sum(country != "us" & country != "") == 0, by = pindex]
keywords_producers = keywords_producers[us == 1]


# when did a keyword first appear
keywords_producers[, first_appearance_year := min(year), by = keyword]

# can consider a keyword new if it appears in its minimum year or the 2 years after to account for the natural production time cycle
keywords_producers[, new_keyword := first_appearance_year == year | first_appearance_year + 1 == year | first_appearance_year + 2 == year]

# just keep one entry per keyword per film
keywords = keywords_producers[producer_seq == 1,]

# assigning a film category to each keyword
setkey(film_category, pindex)
setkey(keywords, pindex)

keywords_categories = merge(keywords, film_category)

# what count of a film's keywords are new
new_keywords_film = unique(keywords_categories[, list(category_count, category_net, new_keywords = sum(new_keyword), total_keywords = .N, year = year), by = pindex])

# by year
yearly_new_keywords_count = unique(new_keywords_film[, list(total_new_keywords = mean(new_keywords)), by = c("category_count", "year")])
yearly_new_keywords_net = unique(new_keywords_film[, list(total_new_keywords = mean(new_keywords)), by = c("category_net", "year")])

# note not all categories have entries for each year
# can see missing with cross-tab
table(yearly_new_keywords_count$category_count, yearly_new_keywords_count$year)
table(yearly_new_keywords_net$category_net, yearly_new_keywords_net$year)

# accounting for no general solos in 2002
yearly_new_keywords_count = rbindlist(list(yearly_new_keywords_count, list("gsolo_count", 2002, NA)))

# and no general solos, coproductions, or hybrids in 1985
yearly_new_keywords_net = rbindlist(list(yearly_new_keywords_net, list("gsolo_net", 1985, NA), list("gcoprod_net", 1985, NA), list("hcoprod_net", 1985, NA)))

setorder(yearly_new_keywords_count, year)
setorder(yearly_new_keywords_net, year)

# can also exclude the first 3 years from the figure, or leave in to see how many keywords were used in total in these years
#yearly_new_keywords = yearly_new_keywords[year > 1987,]

# plotting from plotly, ggplot is good too
font = list(family = "DejaVu Serif", size = 16, color = "black")

p1 = plot_ly(x = unique(yearly_new_keywords_count$year), y = yearly_new_keywords_count$total_new_keywords[yearly_new_keywords_count$category_count=="ssolo_count"], name = "Peripheral solo productions", type = "scatter", mode = "lines") %>%
    add_trace(y = yearly_new_keywords_count$total_new_keywords[yearly_new_keywords_count$category_count=="gsolo_count"], name = "Central solo productions", mode = "lines+markers") %>%
    add_trace(y = yearly_new_keywords_count$total_new_keywords[yearly_new_keywords_count$category_count=="gcoprod_count"], name = "Central co-productions", mode = "lines+markers", line = list(dash = "dash")) %>%
    add_trace(y = yearly_new_keywords_count$total_new_keywords[yearly_new_keywords_count$category_count=="scoprod_count"], name = 'Peripheral co-productions', line = list(dash = "dash")) %>%
    add_trace(y = yearly_new_keywords_count$total_new_keywords[yearly_new_keywords_count$category_count=="hcoprod_count"], name = 'Hybrid coproductions', line = list(dash = "dot")) %>%
    layout(yaxis = list(title = "Total number of new features used")) %>%
    layout(xaxis = list(title = "Production year")) %>%
    layout(title = "Total Number of New Features Introduced in a Film by Co-production Type, 1985-2016")

p2 = plot_ly(x = unique(yearly_new_keywords_net$year), y = yearly_new_keywords_net$total_new_keywords[yearly_new_keywords_net$category_net=="ssolo_net"], name = "Peripheral solo productions", type = "scatter", mode = "lines") %>%
    add_trace(y = yearly_new_keywords_net$total_new_keywords[yearly_new_keywords_net$category_net=="gsolo_net"], name = "Central solo productions", mode = "lines+markers") %>%
    add_trace(y = yearly_new_keywords_net$total_new_keywords[yearly_new_keywords_net$category_net=="gcoprod_net"], name = "Central co-productions", mode = "lines+markers", line = list(dash = "dash")) %>%
    add_trace(y = yearly_new_keywords_net$total_new_keywords[yearly_new_keywords_net$category_net=="scoprod_net"], name = 'Peripheral co-productions', line = list(dash = "dash")) %>%
    add_trace(y = yearly_new_keywords_net$total_new_keywords[yearly_new_keywords_net$category_net=="hcoprod_net"], name = 'Hybrid coproductions', line = list(dash = "dot")) %>%
    layout(yaxis = list(title = "Total number of new features used")) %>%
    layout(xaxis = list(title = "Production year")) %>%
    layout(title = "Total Number of New Features Introduced in a Film by Co-production Type, 1985-2016")


# this will only work with pandoc/imagemagick installed and orca command-line utility, can also view plot by just calling "p1"/"p2"
orca(p1, file = "total number of new features introduced by coproduction type scale.pdf")    
orca(p2, file = "total number of new features introduced by coproduction type collaboration network.pdf")    


# what about new combinations of existing keywords
keywords_yearly = split(keywords, f = keywords$year)
keywords_yearly = lapply(seq_along(keywords_yearly), function(i) keywords_yearly[[i]][keywords_yearly[[i]]$producer_seq == 1])

# keyword dyad list for each year
item.fac = lapply(seq_along(keywords_yearly), function(i) factor(keywords_yearly[[i]]$pindex))
target.fac = lapply(seq_along(keywords_yearly), function(i) factor(keywords_yearly[[i]]$keyword))

# sparse matrix for keywords used each year
incidence = lapply(seq_along(keywords_yearly), function(i) 
        sparseMatrix(
                                        as.numeric(item.fac[[i]]), 
                                        as.numeric(target.fac[[i]]),
                                        dimnames = list(
                                        as.numeric(unique(item.fac[[i]])), 
                                        as.character(unique(target.fac[[i]]))),
                                        x = 1)
        )

cooccurrence = lapply(seq_along(keywords_yearly), function(i) t(incidence[[i]])%*%incidence[[i]])

edges = lapply(seq_along(keywords_yearly), function(i) get.edgelist(graph.adjacency(cooccurrence[[i]], "undirected")))

edges = lapply(seq_along(keywords_yearly), function(i) cbind(edges[[i]], keywords_yearly[[i]]$year[1]))
edges = do.call(rbind, edges)

edges = edges[!edges[,1] == edges[,2],]
# allowing to check for both ways a pair might show up in a film
edges = rbind(edges, cbind(edges[,2], edges[,1], edges[,3]))

# can consider a combination new if it appears in its minimum year or the 2 years after to allow time for the production process
edges = data.table(keyword1 = edges[,1], keyword2 = edges[,2], year = as.numeric(edges[,3]))

# when did a combination first appear
edges[, first_appearance_year := min(year), by = c("keyword1", "keyword2")]

edges[, new_combination := first_appearance_year == year | first_appearance_year + 1 == year]

# just keep the pairs that are new, and we can check these against the pairs that appear in each film
new_combinations = edges[new_combination == 1]

# only considering new combinations of old/existing keywords
keyword_films = keywords[new_keyword == FALSE]
keyword_films = split(keywords, f = keywords$pindex)

# removing these with an index checking if there are at least two keywords as in prior exercises is also just fine
keyword_pairs = lapply(seq_along(keyword_films), function(i) tryCatch(cbind(t(combn(keyword_films[[i]]$keyword, 2)), keyword_films[[i]]$pindex[1], keyword_films[[i]]$year[1]), error = function(e) NULL))
keyword_pairs = do.call(rbind, keyword_pairs) # get a table ignoring the null values for only 1 keyword entries 
keyword_pairs = data.table(keyword1 = keyword_pairs[,1], keyword2 = keyword_pairs[,2] , pindex = as.numeric(keyword_pairs[,3]), year = as.numeric(keyword_pairs[,4]))

# merge the producer combinations to the film combinations
setkeyv(keyword_pairs, c("keyword1", "keyword2"))
setkeyv(new_combinations, c("keyword1", "keyword2"))

# use all.x = TRUE here and the three lines after to make sure that films that do no have at least 2 keywords are not treated as missing in the main data
keyword_pairs_new = merge(keyword_pairs, new_combinations[,-c("year")], all.x = TRUE)
setkey(keyword_pairs_new, pindex)
keyword_pairs_new = merge(keyword_pairs_new, film_category)
keyword_pairs_new[is.na(new_combination), new_combination := 0]

# how many new combinations per film
new_combinations_film = unique(keyword_pairs_new[, list(category_count, category_net, new_combinations = sum(new_combination), total_combinations = .N, year = year), by = pindex])

# by year
yearly_new_combinations_count = unique(new_combinations_film[, list(total_new_combinations = mean(new_combinations)), by = c("category_count", "year")])
yearly_new_combinations_net = unique(new_combinations_film[, list(total_new_combinations = mean(new_combinations)), by = c("category_net", "year")])

yearly_new_combinations_count = rbindlist(list(yearly_new_combinations_count, list("gsolo_count", 2002, NA)))
yearly_new_combinations_net = rbindlist(list(yearly_new_combinations_net, list("gsolo_net", 1985, NA), list("gcoprod_net", 1985, NA), list("hcoprod_net", 1985, NA)))


setorder(yearly_new_combinations_count, year)
setorder(yearly_new_combinations_net, year)

# plot again, here using plotly
p3 = plot_ly(x = unique(yearly_new_combinations_count$year), y = yearly_new_combinations_count$total_new_combinations[yearly_new_combinations_count$category_count=="ssolo_count"], name = "Peripheral solo productions", type = "scatter", mode = "lines") %>%
    add_trace(y = yearly_new_combinations_count$total_new_combinations[yearly_new_combinations_count$category_count=="gsolo_count"], name = "Central solo productions", mode = "lines+markers") %>%
    add_trace(y = yearly_new_combinations_count$total_new_combinations[yearly_new_combinations_count$category_count=="gcoprod_count"], name = "Central co-productions", mode = "lines+markers", line = list(dash = "dash")) %>%
    add_trace(y = yearly_new_combinations_count$total_new_combinations[yearly_new_combinations_count$category_count=="scoprod_count"], name = 'Peripheral co-productions', line = list(dash = "dash")) %>%
    add_trace(y = yearly_new_combinations_count$total_new_combinations[yearly_new_combinations_count$category_count=="hcoprod_count"], name = 'Hybrid co-productions', line = list(dash = "dot")) %>%
    layout(yaxis = list(title = "Number of new combinations of existing features introduced")) %>%
    layout(xaxis = list(title = "Production year")) %>%
    layout(title = "Total New Combinations of Existing Features Introduced in a Film by Co-production Type, 1985-2016", font = font)

p4 = plot_ly(x = unique(yearly_new_combinations_net$year), y = yearly_new_combinations_net$total_new_combinations[yearly_new_combinations_net$category_net=="ssolo_net"], name = "Peripheral solo productions", type = "scatter", mode = "lines") %>%
    add_trace(y = yearly_new_combinations_net$total_new_combinations[yearly_new_combinations_net$category_net=="gsolo_net"], name = "Central solo productions", mode = "lines+markers") %>%
    add_trace(y = yearly_new_combinations_net$total_new_combinations[yearly_new_combinations_net$category_net=="gcoprod_net"], name = "Central co-productions", mode = "lines+markers", line = list(dash = "dash")) %>%
    add_trace(y = yearly_new_combinations_net$total_new_combinations[yearly_new_combinations_net$category_net=="scoprod_net"], name = 'Peripheral co-productions', line = list(dash = "dash")) %>%
    add_trace(y = yearly_new_combinations_net$total_new_combinations[yearly_new_combinations_net$category_net=="hcoprod_net"], name = 'Hybrid co-productions', line = list(dash = "dot")) %>%
    layout(yaxis = list(title = "Number of new combinations of existing features introduced")) %>%
    layout(xaxis = list(title = "Production year")) %>%
    layout(title = "Total New Combinations of Existing Features Introduced in a Film by Co-production Type, 1985-2016", font = font)


orca(p3, file = "total new combinations of existing features introduced by coproduction type scale.pdf")
orca(p4, file = "total new combinations of existing features introduced by coproduction type collaboratio network.pdf")


####### Part B

# get the new keywords and film category info combined with the box office info we made before
setkey(pbox, pindex)
setkey(new_keywords_film, pindex)
setkey(new_combinations_film, pindex)

colnames(film_category)[colnames(film_category) == "category_count"] = "coproduction_type"
setkey(film_category, pindex)

pbox_new = merge(pbox, new_keywords_film[,-c("year")])
pbox_new = merge(pbox_new, new_combinations_film[,-c("year", "category_count", "category_net")])

pbox_new[, gcoprod_count := category_count == "gcoprod_count"]
pbox_new[, scoprod_count := category_count == "scoprod_count"]
pbox_new[, hcoprod_count := category_count == "hcoprod_count"]
pbox_new[, gsolo_count := category_count == "gsolo_count"]
pbox_new[, ssolo_count := category_count == "ssolo_count"]

pbox_new[, gcoprod_net := category_net == "gcoprod_net"]
pbox_new[, scoprod_net := category_net == "scoprod_net"]
pbox_new[, hcoprod_net := category_net == "hcoprod_net"]
pbox_new[, gsolo_net := category_net == "gsolo_net"]
pbox_new[, ssolo_net := category_net == "ssolo_net"]


# set up the controls for subsidiary and multidimensional scaling
subsidiary = fread("production_subsidiaries.csv", head = TRUE, key = "pcindex")
setkey(pbox_new, pcindex)

pbox_new = merge(pbox_new, subsidiary, all.x = TRUE)

pbox_new[!is.na(first_year), subsidiary := year >= first_year & year <= last_year]
pbox_new[is.na(subsidiary), subsidiary := 0]

# yearly again
yearly_box_new = unique(pbox_new[, list(total_box = sum(total_box), count_films = .N, release_coverage = sum(release_coverage), budget = sum(budget), new_keywords = sum(new_keywords), new_combinations = sum(new_combinations), gcoprod_count = sum(gcoprod_count), scoprod_count = sum(scoprod_count), hcoprod_count = sum(hcoprod_count), gsolo_count = sum(gsolo_count), ssolo_count = sum(ssolo_count), gcoprod_net = sum(gcoprod_net), scoprod_net = sum(scoprod_net), hcoprod_net = sum(hcoprod_net), gsolo_net = sum(gsolo_net), ssolo_net = sum(ssolo_net), prod_company, subsidiary), by = c("pcindex", "year")])


# multidimensional scaling

# reuse the combined keywords and producers object that we made before

# want comparison set to be from current year, one year past, or one year before that
years = sort(unique(keywords_producers$year))
keywords_producers[, as.character(years) := lapply(years, function(x) year == x | year == x -1 | year == x - 2)]


# setting up windows object
keywords_yearly = list(keywords_producers[keywords_producers$"1985" == TRUE], keywords_producers[keywords_producers$"1986" == TRUE], keywords_producers[keywords_producers$"1987" == TRUE], keywords_producers[keywords_producers$"1988" == TRUE], keywords_producers[keywords_producers$"1989" == TRUE], keywords_producers[keywords_producers$"1990" == TRUE], keywords_producers[keywords_producers$"1991" == TRUE], keywords_producers[keywords_producers$"1992" == TRUE], keywords_producers[keywords_producers$"1993" == TRUE], keywords_producers[keywords_producers$"1994" == TRUE], keywords_producers[keywords_producers$"1995" == TRUE], keywords_producers[keywords_producers$"1996" == TRUE], keywords_producers[keywords_producers$"1997" == TRUE], keywords_producers[keywords_producers$"1998" == TRUE], keywords_producers[keywords_producers$"1999" == TRUE], keywords_producers[keywords_producers$"2000" == TRUE], keywords_producers[keywords_producers$"2001" == TRUE], keywords_producers[keywords_producers$"2002" == TRUE], keywords_producers[keywords_producers$"2003" == TRUE], keywords_producers[keywords_producers$"2004" == TRUE], keywords_producers[keywords_producers$"2005" == TRUE], keywords_producers[keywords_producers$"2006" == TRUE], keywords_producers[keywords_producers$"2007" == TRUE], keywords_producers[keywords_producers$"2008" == TRUE], keywords_producers[keywords_producers$"2009" == TRUE], keywords_producers[keywords_producers$"2010" == TRUE], keywords_producers[keywords_producers$"2011" == TRUE], keywords_producers[keywords_producers$"2012" == TRUE], keywords_producers[keywords_producers$"2013" == TRUE], keywords_producers[keywords_producers$"2014" == TRUE], keywords_producers[keywords_producers$"2015" == TRUE], keywords_producers[keywords_producers$"2016" == TRUE], keywords_producers[keywords_producers$"2017" == TRUE], keywords_producers[keywords_producers$"2018" == TRUE])


# sparse matrix for keywords used by each producer in each 3-year window
# note that it's possible to just apply factors directly for making the matrix, skipping the item.fac and target.fac steps from above
incidence = lapply(seq_along(keywords_yearly), function(l)
        sparseMatrix(i = as.numeric(factor(keywords_yearly[[l]]$keyword)),
                j = as.numeric(factor(keywords_yearly[[l]]$pcindex)),
                x = rep(1, nrow(keywords_yearly[[l]])),
                dimnames = list(levels(factor(keywords_yearly[[l]]$keyword)), levels(factor(keywords_yearly[[l]]$pcindex)))
         )
)
 
# setting up dist matrix and mdcoords for producers 
jaccard_dist = list()
mdscale_year = list()
mdcoords = list()
jaccard_square = list()
prod_names = list()
distances = list()

for(i in seq_along(keywords_yearly)){
    jaccard_dist = dist(as.matrix(incidence[[i]]), method = "jaccard", by_rows = FALSE)
    mdscale_year = cmdscale(jaccard_dist, k = 2)
    # need to mess with the year here to relabel years of the lags to all represent the most recent year and not recycle them from the year vector when making the data table. this means that all producers from a grouped year will be represented by that grouped year. not their lagged year. so, comparisons can be made across coproduction types and coordinates as the producers appearing in that year and not the year of their lags
    setorder(keywords_yearly[[i]], -year)
    mdcoords[[i]] = data.table(pcindex = names(jaccard_dist), coord1 = mdscale_year[,1], coord2 = mdscale_year[,2], year = keywords_yearly[[i]]$year[1])


    # can use this loop to skip ahead to get the distances for Question 2 also
    jaccard_square = as.matrix(jaccard_dist)
    prod_names = t(combn(colnames(jaccard_square), 2))
    jaccard_dist = as.numeric(jaccard_dist)
    distances[[i]] = data.table(producer1 = prod_names[,1], producer2 = prod_names[,2], distance = jaccard_dist, year = keywords_yearly[[i]]$year[1])


    # you can use this syntax to check the progress of the loop to make sure it's still running and, if there is an error, to see when and at what iteration it occurred. very helpful when a long, intensive loop is needed
    print(data.table(year = keywords_yearly[[i]]$year[1], time = Sys.time())) 
    flush.console()

    # if active memory is strained, can also export each iteration individually and then load back in, with something using append = TRUE
    #fwrite(mdcoords[[i]], file = "producer_multidimensional_coordinates_yearly.csv", append = TRUE)
    #fwrite(distances[[i]], file = "producer_jaccard_distances_yearly.csv", append = TRUE)
}         

mdcoords = rbindlist(mdcoords)
distances = rbindlist(distances)
fwrite(mdcoords, "producer_multidimensional_coordinates_yearly_11_19.csv")
fwrite(distances, "producer_jaccard_distances_yearly_11_19.csv")
# and to bring back in 
mdcoords = fread("producer_multidimensional_coordinates_yearly_11_19.csv", head = TRUE)

# merge to main data
setkeyv(mdcoords, c("pcindex", "year"))
setkeyv(yearly_box_new, c("pcindex", "year"))
    
yearly_box_new = merge(yearly_box_new, mdcoords, all.x = TRUE)

# last control and offset
yearly_box_new[, age := seq_len(.N), by = pcindex]

count_films_keywords = unique(keywords_producers[, list(count_films_key = length(unique(pindex))), by = c("pcindex", "year")])
setkeyv(count_films_keywords, c("pcindex", "year"))

yearly_box_new = merge(yearly_box_new, count_films_keywords)

# regressions for new keywords and new combinations
summary(glm.nb(new_keywords ~ gcoprod_count + scoprod_count + hcoprod_count + coord1 + coord2 + total_box + age + subsidiary + factor(year), data = yearly_box_new[year > 1987], offset(count_films_key)))
summary(glm.nb(new_keywords ~ gcoprod_net + scoprod_net + hcoprod_net + coord1 + coord2 + total_box + age + subsidiary + factor(year), data = yearly_box_new[year > 1987], offset(count_films_key)))

summary(glm.nb(new_combinations ~ gcoprod_count + scoprod_count + hcoprod_count + coord1 + coord2 + total_box + age + subsidiary + factor(year), data = yearly_box_new[year > 1987], offset(count_films_key)))
summary(glm.nb(new_combinations ~ gcoprod_net + scoprod_net + hcoprod_net + coord1 + coord2 + total_box + age + subsidiary + factor(year), data = yearly_box_new[year > 1987], offset(count_films_key)))
# generalist collabs and hybrid collabs related to creative innovation

############## Question 2

# can use the distances we used as inputs to the multidimensional scaling from 1B from the distances object
distances = rbindlist(distances)
distances = rbindlist(list(distances, distances[, c("producer2", "producer1", "distance", "year")]), use.names =  FALSE)

avg_dist = unique(distances[,list(average_dist = mean(distance)), by = c("producer1", "year")])
colnames(avg_dist)[colnames(avg_dist) == "producer1"] = "pcindex"
setkeyv(avg_dist, c("pcindex", "year"))

yearly_box_new = merge(yearly_box_new, avg_dist, all.x = TRUE)

# plot for new keywords
ggplot(yearly_box_new, aes(average_dist, new_keywords)) + geom_smooth(method
  = "loess", se = T) + labs(x = "Average Jaccard distance", y = "New keywords")

# plot for new combinations
ggplot(yearly_box_new, aes(average_dist, new_combinations)) + geom_smooth(method
  = "loess", se = T) + labs(x = "Average Jaccard distance", y = "New combinations of existing keywords")
# downward-sloping line indicates creative innovation more likely to occur when producers can combine more distant knowledge bases

############## Question 3

# generate return and standardize within year
yearly_box_new[, return := total_box/release_coverage]
yearly_box_new[, standardized_return := (return - mean(return, na.rm = TRUE))/sd(return, na.rm = TRUE), by = year]

# regression for box office return
summary(lm(standardized_return ~ gcoprod_count + scoprod_count + hcoprod_count + coord1 + coord2 + age + subsidiary + factor(year), data = yearly_box_new))
summary(lm(standardized_return ~ gcoprod_net + scoprod_net + hcoprod_net + coord1 + coord2 + age + subsidiary + factor(year), data = yearly_box_new))

# generalist collabs related to higher box office returns

############## Question 4

####### Part A

# reuse the producers and films object to split out new keywords and combinations from different collaboration types
new_solo = unique(pbox_new[category_net == "ssolo_net" | category_net == "gsolo_net", list(new_keywords_solo = sum(new_keywords)), by = c("pcindex", "year")])
new_hybrid = unique(pbox_new[category_net == "hcoprod_net", list(new_keywords_hybrid = sum(new_keywords)), by = c("pcindex", "year")])

setkeyv(new_solo, c("pcindex", "year"))
setkeyv(new_hybrid, c("pcindex", "year"))

# add the columns onto the existing yearly producer object
yearly_box_new = merge(yearly_box_new, new_solo, all.x = TRUE)
yearly_box_new = merge(yearly_box_new, new_hybrid, all.x = TRUE)

# missing are 0s, from not engaging in collaboration type
yearly_box_new[is.na(new_keywords_solo), new_keywords_solo := 0]
yearly_box_new[is.na(new_keywords_hybrid), new_keywords_hybrid := 0]

# cumulative stock of new keywords from collaborations
yearly_box_new[, stock_new_keywords_hybrid := cumsum(new_keywords_hybrid), by = "pcindex"]

# regressions for new keywords and new combinations
summary(glm.nb(new_keywords_solo ~  stock_new_keywords_hybrid + coord1 + coord2 + total_box + age + subsidiary + factor(year), data = yearly_box_new[year > 1987], offset(count_films_key)))

# creative innovation gained through hybrid collaborations associated with introducing more new keywords

####### Part B
summary(lm(standardized_return ~ new_combinations + gcoprod_net + scoprod_net + hcoprod_net + coord1 + coord2 + age + subsidiary + factor(year), data = yearly_box_new))

# more creative innovation, through new keywords and more new combinations, is associated with higher box office returns. so, even if some collaboration types are financially risky at first, can represent investment into being more innovative and earning higher returns later on--especially important for large, old companies that are less adaptable


##### Extra Credit
# cast working history
cast = fread("film_cast_members.csv", head = TRUE)

# just getting production companies with their films and the film category
pcs_films = unique(keywords_categories[,-c("keyword","first_appearance_year","keyindex")])

setkey(pcs_films, pindex)
setkey(cast, pindex)

film_category_cast = merge(pcs_films[producer_seq == 1], cast[,c("pindex", "nconst")])

# new keywords associated with each cast member based on films they worked on 
setorderv(film_category_cast, c("nconst", "year"))
film_category_cast[, new_keyword_exp := cumsum(new_keyword), by = nconst]

film_category_cast_year = unique(film_category_cast[, list(new_keyword_exp = max(new_keyword_exp)), by = c("nconst", "year")])

# shift values one year back by leading year by 1
setorderv(film_category_cast_year, c("nconst", "year"))

film_category_cast_year[, current_year := year]
film_category_cast_year[, year := shift(current_year, 1, type = "lead"), by = "nconst"]

# allows for merging each cast member into a single data object, allowing the key to be unique within each iteration of "orderings" -- something similar with split() would also work just fine
orderings = sort(unique(cast$ordering))
pcs_cast = lapply(seq_along(orderings), function(i) merge(pcs_films, cast[ordering == ordering[i],c("pindex", "nconst")]))

pcs_cast = rbindlist(pcs_cast)

setkeyv(film_category_cast_year, c("nconst", "year"))
setkeyv(pcs_cast, c("nconst", "year"))

pcs_cast = merge(pcs_cast, film_category_cast_year)

# yearly object for each producer with cast experience/innovation information
pcs_exp_year = unique(pcs_cast[,list(new_keyword_exp = sum(new_keyword_exp)), by = c("pcindex", "year")])

# merge back to main data
setkeyv(pcs_exp_year, c("pcindex", "year"))
setkeyv(yearly_box_new, c("pcindex", "year"))

yearly_box_new = merge(yearly_box_new, pcs_exp_year, all.x = TRUE)

# set up regression for count outcome
summary(glm.nb(new_keyword_exp ~ hcoprod_count + gcoprod_count + scoprod_count + coord1 + coord2 + total_box + age + subsidiary + factor(year), data = yearly_box_new))

# appears that engaging in more hybrid coproductions helps with hiring more innovative cast members in the future