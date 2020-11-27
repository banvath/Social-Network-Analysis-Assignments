rm(list = ls(all = TRUE))
library(igraph)
library(data.table)
setwd("C:/Users/dplewi3/Dropbox")
setwd("~/Dropbox")


############## Data prep
# the data is given in the file  social_and_task_network.csv
# read in with read.csv or fread, don't need row names so fread is okay here
all_edges = fread("classroom_social_and_task_network.csv", head = TRUE)

# check data structure
all_edges
# data is in edge list format with ego sending tie and alter receiving a tie
# number under task and social is strength of tie 
# when number is 0, no tie exists

# get rid of 0s to make the network of existing ties:

# reduce to non-zero edges and build a graph object
nonzero_edges = all_edges[social_tie > 0 | task_tie > 0]
nonzero_edges
 
full_network = graph.data.frame(nonzero_edges) 
# using graph.data.frame retains the edge types as edge attributes

# check basic info about the network
full_network
# we have edge attributes indicating what type of tie each edge is, which we can use to split up the networks into social and task ties

# create separate graphs for each relationship type based on edge attributes and remove isolates
# can use delete.edges with an edge index with the E() notation to subset on the only edges with particuar attributes 

# remove edges where there is no social tie
network_social = delete.edges(full_network, E(full_network)[get.edge.attribute(full_network,name = "social_tie")==0])

# check the social ties
network_social
 
# remove edges where there is no task tie
network_task = delete.edges(full_network, E(full_network)[get.edge.attribute(full_network,name = "task_tie")==0])

network_task

# plot each network just to take a look before analyzing
plot(network_social, layout=layout.fruchterman.reingold, edge.arrow.size=.5)
 
plot(network_task, layout=layout.fruchterman.reingold, edge.arrow.size=.5)

# removing isolates before centrality calculations

# can remove with similar syntax as deleting edges with particular attributes. in this case, isolates in either network will have a degree of 0
network_social = delete.vertices(network_social, V(network_social)[degree(network_social)==0])
network_task = delete.vertices(network_task, V(network_task)[degree(network_task)==0])

############## Question 1
 
####### Part A

# can write a function to get all of the network stats at once and put into a table
getNetStats=function(net)
{
  deg_in = degree(net, mode = "in")
  deg_out = degree(net, mode = "out")
  close= closeness(net, mode = "total")
  betw = betweenness(net)
  prank = page_rank(net)$vector # page_rank creates a list object with some other items in it, we just want the actual scores, which are contained in the object called "vector"
  id=V(net)$name
  stats= as.data.table(list(id = id, deg_in = deg_in, deg_out = deg_out, close = close, betw = betw, prank = prank))
  return(stats)
}
 
netstats_social = getNetStats(network_social)
netstats_task = getNetStats(network_task)

# keep the plot handy to visually verify the stats
plot(network_social, vertex.size=10, vertex.label=V(network_social)$name, edge.arrow.size = 0.5, layout=layout.fruchterman.reingold,main='Classroom S641 Social Talk')

# examine the tables to find the most central actors  according to the different measures
 
# show table sorted by centrality scores. the order() function returns a vector in ascending order and the minus sign flips it to be descending order

# for the social ties
netstats_social[order(-deg_in)] 
netstats_social[order(-deg_out)] 
netstats_social[order(-close)] 
netstats_social[order(-betw)] 
netstats_social[order(-prank)] 

# let's make a plot or two with these summary statistics
 
# To visualize these data, we can create a barplot for each
# centrality measure. In all cases, the y-axis is the value of
# each category and the x-axis is the node number. 

# for the task ties
barplot(netstats_task$deg_in, names.arg=netstats_task$id)
barplot(netstats_task$deg_out, names.arg=netstats_task$id)
barplot(netstats_task$close, names.arg=netstats_task$id)
barplot(netstats_task$betw, names.arg=netstats_task$id)
barplot(netstats_task$prank, names.arg=netstats_task$id)

# 22 by the far the most important in the task network. task network measures are much more dispersed, i.e., concentrated in 22, which will make the network-level indices of the centrality measures tend towards 1

# what can we say about the social actors if we compare the bar plots? who seems to run the show in sociable affairs? who seems to bridge sociable conversations? 

####### Part B

# compute correlations between the columns to determine how closely these measures of centrality are interrelated 

# to do this, we need to either have no missing data or use pairwise complete observations

# one approach is to compute the correlations based only on the actors in both graphs (18 in total)
cor(netstats_social[which(netstats_social$id %in% netstats_task$id),2:6], netstats_task[which(netstats_task$id %in% netstats_social$id),2:6])

# two insights: 

# those who are important in talk on tasks also serve as bridges for talk on social issues and have many outbound ties

# the larger number of indegree and outdegree ties a node has with respect to task talk, the more they serve as a bridge on social talk


############## Question 2

####### Part A

# visual solution
# get mean and median from the strength for each tie type

# can compute these with 
mean(nonzero_edges$task_tie[nonzero_edges$task_tie > 0])
quantile(nonzero_edges$social_tie[nonzero_edges$social_tie > 0], .5)

# make an edge attribute indicating if a tie is considered strong or not
social_strong = nonzero_edges$social_tie > mean(nonzero_edges$social_tie[nonzero_edges$social_tie > 0])
task_strong = nonzero_edges$task_tie > mean(nonzero_edges$task_tie[nonzero_edges$task_tie > 0])

# add this attribute to the main network
full_network = set_edge_attr(full_network, "strong", index = E(full_network), social_strong == TRUE | task_strong == TRUE)

# set the color of the edges using the "strong" edge attribute
E(full_network)$color = c("red", "dark blue")[as.factor(E(full_network)$strong)]

# plot the network to check for strong triadic closure. strong ties are dark blue, so if two dark blue edges leave a node, strong triadic closure says that there should be a red or blue edge connecting the two other nodes the dark blue edges connect to
plot(full_network,vertex.label=V(full_network)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
# graph doesn't appear to exhibit strong triadic closure

# exactly to what extent, can see with a programmatic solution

# just get the edge list of strong ties
full_network_strong = nonzero_edges[social_strong == TRUE | task_strong == TRUE, c("ego", "alter")]

# the process below is similar to calculating triadic closure in igraph using something like transitivity(), but the triadic closure function in igraph would also include weak ties when looking for a potentially open triple, and we want only potential triples in which at least two of the ties are strong

# next, check for a strong tie existing in either direction on the edgelist and get these distinct edges
# this method effectively changes the directed network into an undirected one for the purposes of calculation
# using data.table's rbindlist with use.names = FALSE avoids having to define separate column names during rbind 
full_network_strong = rbindlist(list(full_network_strong, full_network_strong[, c("alter", "ego")]), use.names = FALSE)

# don't need duplicates (cases in which strong tie is reciprocated)
full_network_strong = unique(full_network_strong)

# split into chunks in a list for each ego so the chunks can vary in size
# chunks with at least two rows represents two strong ties leaving a node
full_network_strong[, min_two := .N > 1, by = ego]
full_network_strong = full_network_strong[min_two==TRUE]
full_network_strong = split(full_network_strong, by = "ego")

# get potential friends of friends that would be implied if triadic closure is satisfied
# this is potential pairs of people that ego is connected to, when ego has more than one strong tie
strong_friends_of_friends = lapply(seq_along(full_network_strong), function(i) t(combn(full_network_strong[[i]]$alter, 2)))
strong_friends_of_friends = unique(do.call(rbind, strong_friends_of_friends)) # do.call rbind is an efficient way of generating a data frame from a list where the columns of each list element represent the same variables

# this is very similar to the code generating the ties among members of group emails in the outlook email list script -- this syntax will be used throughout the course for generating edge lists from grouped data that represent events that tie people together, e.g., produced by split()

# to satisfy strong triadic closure, all of these ties implied if strong triadic closure is satisfied should actually be realized in the observed edgelist

# checking both directions as before
# similarly using data.table here on the second object to avoid having to use column names during rbind
real_edges = rbindlist(list(nonzero_edges[,c("ego", "alter")], nonzero_edges[,c("alter", "ego")]), use.names = FALSE)

# do the scan of potential triads closed to ones that actually are closed in the real data
fof = paste(strong_friends_of_friends[,1],strong_friends_of_friends[,2],sep=",")
real = paste(real_edges$ego, real_edges$alter, sep=",")

# shows which triples with two strong ties aren't closed
fof[!(fof %in% real)]

# and what proportion of strong triadic closure is realized
mean(fof %in% real)


####### Part B

# repeat the same steps above for the median instead of the mean
social_strong = nonzero_edges$social_tie > quantile(nonzero_edges$social_tie[nonzero_edges$social_tie > 0], .5)
task_strong = nonzero_edges$task_tie > quantile(nonzero_edges$task_tie[nonzero_edges$task_tie > 0], .5)

full_network = set_edge_attr(full_network, "strong", index = E(full_network), social_strong == TRUE | task_strong == TRUE)
E(full_network)$color = c("red", "dark blue")[as.factor(E(full_network)$strong)]

plot.igraph(full_network,vertex.label=V(full_network)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)
# don't see strong triadic closure

# repeat the programmatic approach
social_strong = nonzero_edges$social_tie > quantile(nonzero_edges$task_tie[nonzero_edges$task_tie > 0], .5)
task_strong = nonzero_edges$task_tie > quantile(nonzero_edges$task_tie[nonzero_edges$task_tie > 0], .5)

full_network_strong = nonzero_edges[social_strong == TRUE | task_strong == TRUE,1:2]

# checking for a strong tie existing both directions on the edgelist, and get the distinct edges
# using data.table here to avoid having to define separate column names during rbind 
# don't need to call to first item because it is a data frame
full_network_strong = rbindlist(list(full_network_strong, data.table(cbind(full_network_strong$alter, full_network_strong$ego))))
full_network_strong = unique(full_network_strong)

# split into chunks in a list for each ego so the chunks can vary in size
# only care about chunks with at least two rows
full_network_strong[, min_two := .N > 1, by = ego]
full_network_strong = full_network_strong[min_two==TRUE]
full_network_strong = split(full_network_strong, by = "ego")

# get potential friends of friends that would be implied by triadic closure
# this is potential pairs of people that ego is connected to, when ego has more than one strong tie
strong_friends_of_friends = lapply(seq_along(full_network_strong), function(i) t(combn(full_network_strong[[i]]$alter, 2)))
strong_friends_of_friends = unique(do.call(rbind, strong_friends_of_friends))
# this is very similar to the code generating the ties among members of group emails in the outlook email list script

# to satisfy strong triadic closure, all of these ties should be realized in the actual edgelist

# checking both directions as before
# similarly using data.table here on the second object to avoid having to use column names during rbind
real_edges = rbindlist(list(nonzero_edges[,1:2], data.table(cbind(nonzero_edges$alter, nonzero_edges$ego))))

# do the scan of potential triads closed to ones that actually are closed in the real data
fof = paste(strong_friends_of_friends[,1],strong_friends_of_friends[,2],sep=",")
real = paste(real_edges$ego, real_edges$alter, sep=",")

# shows which triples with two strong ties aren't closed
fof[!(fof %in% real)]

# and what proportion of strong triadic closure is realized
mean(fof %in% real)

# both results suggest moderate alignment to triadic closure
# result for mean > result for median also suggests that some people have really strong relationships--tie strength is distributed with a long right-tail


############## Question 3

####### Part A

# calculate the betweenness measure separately for each type of relationship

# can use the same subestting syntax as in the Data Prep step

# this time select on the nonzero values of the relationship type we are interested in
between_social = edge.betweenness(full_network, e = E(full_network)[get.edge.attribute(full_network,name = "social_tie")!=0])
between_task = edge.betweenness(full_network, e = E(full_network)[get.edge.attribute(full_network,name = "task_tie")!=0])

####### Part B

# combine the betweenness result with a list containing the tie strengths

# does high betweenness tend to be related to the presence of a strong tie?

# mean
social_strong = nonzero_edges$social_tie > mean(nonzero_edges$task_tie[nonzero_edges$task_tie > 0])
task_strong = nonzero_edges$task_tie > mean(nonzero_edges$task_tie[nonzero_edges$task_tie > 0])

cor(between_social, social_strong[get.edge.attribute(full_network,name = "social_tie")!=0])
cor(between_task, task_strong[get.edge.attribute(full_network,name = "task_tie")!=0])


# median
social_strong = nonzero_edges$social_tie > quantile(nonzero_edges$task_tie[nonzero_edges$task_tie > 0], .5)
task_strong = nonzero_edges$task_tie > quantile(nonzero_edges$task_tie[nonzero_edges$task_tie > 0], .5)

cor(between_social, social_strong[get.edge.attribute(full_network,name = "social_tie")!=0])
cor(between_task, task_strong[get.edge.attribute(full_network,name = "task_tie")!=0])

# no for social, yes for task. this may be in part due to Node 22's function as a bridge in the task network, and the large number of ties this node is a part of

# can compare to visual here
plot(full_network,vertex.label=V(full_network)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)


############## Question 4

# use the same full network as in Question 1
network_matrix = as_adjacency_matrix(full_network, sparse = FALSE)

# need to iterate over two objects to check reachability

# first object will hold the powers of the adjacency matrix
network_matrix_power = network_matrix

# second is a reachability object that will check whether or not any two nodes are reachable as the number of powers--which represent the number of steps in path from one node to another--increases 
network_pathcheck = network_matrix

# can set this up in a quick loop by using some properties of the adjacency matrix

# a couple things to note:
# 1. the loop needs to run a number of times that is equal to the number of nodes in the network minus 1
# 2. this number is equal to the longest distance a shortest path between any two nodes in the network can be
# 3. if no path exists at the last iteration, the entry in the matrix will be 0
# 4. the number of 0s in this matrix tells us how many nodes are unreachable from one another, since the loop has run enough iterations to traverse all possible shortest paths on the graph
# 5. since the first iteration of the loop is a already second power, only run a number of times equal nrow - 2 instead of nrow - 1
for(i in seq_len(nrow(network_matrix) - 2)){
	network_matrix_power = network_matrix_power%*%network_matrix # increases the power of the matrix by 1
	network_pathcheck = network_pathcheck + network_matrix_power # counts the total number of walks between two nodes of any length, up to the number of nodes in the network minus 1
}

# if there are any 0s left in the matrix at the end of the loop, these are unreachable pairs
sum(network_pathcheck == 0)

# through igraph, can confirm
sum(distances(full_network) == Inf)

# or
distance_table(full_network)$unconnected

# if we included isolates 


# the package "expm"'s %^% solution also works in the loop above, e.g., something like network_matrix_power%^%i
# but still need to keep adding to the pathcheck matrix to build the full reachability object


############## Question 5

# in order for the centralization to be at its maximum, want the numerator to equal the denominator in 
# sum((max(degree(net)) - degree(net))/((n -1)* (n-2)))

# this occurs at maxmimum dispersion, i.e., when one node is maximally connected, 
# and every other node is minimally connected 

# e.g., for a network with 5 nodes,
# (0 + 3 + 3 + 3 + 3)/(4*3)

# for a network with 6 nodes
# (0 + 4 + 4 + 4 + 4 + 4)/(5*4)

# so in each case, the addition in the numerator is equal to the # of nodes minus 2,
# repeated the number of nodes - 1
# which is the same as the multiplication in the denominator

# this is also known as the "star" graph

star = make_star(10, "undirected")

plot(star)

# but, when we verify the centralization through igraph...
centr_degree(star)
centralize(degree(star))

# even though
sum((max(degree(star)) - degree(star))/((vcount(star) -1)* (vcount(star)-2)))

# returns the right answer

# as do
centr_clo(star)
centr_betw(star)

# for degree, we can check in the sna library
library(network)
starnet = network(as_adjacency_matrix(star, sparse = FALSE))

detach("package:igraph", unload=TRUE)
library(sna)

centralization(starnet, degree)

# the opposite scenario to the star is the ring, which we can look at with 

detach("package:sna", unload=TRUE)
library(igraph)

ring = make_ring(10)

plot(ring)

centr_degree(ring)
centralize(degree(ring))

sum((max(degree(ring)) - degree(ring))/((vcount(ring) -1)* (vcount(ring)-2)))


centr_clo(ring)
centr_betw(ring)

ringnet = network(as_adjacency_matrix(ring, sparse = FALSE))

detach("package:igraph", unload=TRUE)
library(sna)

centralization(ringnet, degree)
