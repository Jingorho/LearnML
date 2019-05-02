############################################
###     Recitation 10 - Social Networks   ###
# install igraph package
# install.packages("igraph")
# install RColorBrewer to make nice colors
# install.packages("RColorBrewer")
# load in the packages
library(igraph)
library(RColorBrewer)


### Part I: visualization                ###
# read in the nodes and edges data
info = read.csv("nodes_15071_anon.csv")
links = read.csv("edges_15071_anon.csv")

# First, let's take a look:
head(info)
# the node data provides the index of the nodes
# as well as the "program" attribute (undergrad, MBA 2018, etc.)
# and the "section" attribute (A or B)
head(links)
# edges data include the indices of two nodes n1 and n2
# indicating the two nodes (in this case, students) that form an edge

# Let us now construct the graph using graph.data.frame.

# This function constructs a graph by feeding
# in the edges and nodes data frames. The first
# two columns of the edge data should be the IDs
# of the two endpoints of each edge, and the
# first column of the node data should be the
# ID of that node.

# We set directed=F for undirected graphs
G = graph.data.frame(links, directed=F, info)

subset(links, links$n1==3)
subset(links, links$n2==3)
degree(G)
modularity(G, comp$membership)
# How do we plot our graph G?
# plotting a graph is separated into two steps:
# - layout (there are many ways to do this)
# - plotting (actually draws it)
# we set the seed so the layout is the same
set.seed(144)
mylayout = layout.auto(G) # generates spatial locations for plotting the nodes
# we will use the same layout "mylayout" throughout

# First, try just plain plot function
plot(G, layout=mylayout)
# quite ugly!
# Let's fix the margins of our graph
par(mar=c(0, 0, 0, 0))
plot(G, layout=mylayout)

# Still a mess - let's try to make it nicer
# Remove the node labels (vertex.label=NA), and let the 
# size vary proportional to the square root of degree (i.e., # of neighbors) 
plot(G, layout=mylayout, vertex.label=NA, vertex.size=2*sqrt(degree(G)))
# a little nicer!

# What if we wanted to color the nodes depending on the program?
col <- ifelse(V(G)$program == "MBA 2018", "green", 
              ifelse(V(G)$program == "MBA 2019", "blue", "red"))
# V(G)$program contains the program information from "info"
# This will color MBA 2018s in green, MBA 2019s in blue, and others in red
plot(G, layout=mylayout, vertex.label=NA, vertex.size=2*sqrt(degree(G)), 
     vertex.color = col)

# We can make the color vary too, but it is a little trickier. 
# The ifelse() function works the same as in Excel. ifelse([condition],[value if true],[value if false])
# ifelse() functions can be nested inside ifelse() functions in the location the 2nd or 3rd input.


### ALERT: EXPERT ONLY ####
# get.colors is a function that we've written -- it takes
# as input two colors (the color for low values and
# the color for high values) as well as values (x) and it
# returns the color for each value.
get.colors = function(cols, x) {
  ramp = colorRamp(cols)
  apply(ramp(x/max(x)), 1, function(y) rgb(y[1]/255, y[2]/255, y[3]/255))
}

# You don't need to understand or change get.colors
# to use it when plotting:
plot(G, layout=mylayout, vertex.label=NA, vertex.size=2*sqrt(degree(G)),
     vertex.color = get.colors(c("white", "purple"), degree(G)))
### END ALERT ###



### PART II:      Network metrics           ###### 
# From now on, we are only going to work with
# students who are in the largest connected
# component of the graph
comp = components(G)
# inspect the membership
comp$membership
table(comp$membership)

attributes(comp)

# only keep those that are in the largest component (component #1)
in.max.comp = (comp$membership == 1) # this is a vector of TRUE/FALSE
sg = induced_subgraph(G, in.max.comp) # this gives us a new graph object
# get the sub layout, based on the layout used for the entire dataset
sg_mylayout = mylayout[in.max.comp,]
col <- ifelse(V(sg)$program == "MBA 2018", "green", 
              ifelse(V(sg)$program == "MBA 2019", "blue", "red"))
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=2*sqrt(degree(sg)), vertex.color = col)
mode(sg_mylayout)
# Metric 1: Degree Centrality

# This is just the number of edges coming in/out
# of each of the node. This is provided directly
# from the graph, no calculation required.
degree(sg)
# We can plot by degree (which we already did!).
# The eye measures the size of a point by its
# area but vertex.size scales the radius of the
# point, so you almost always want to apply the
# square root ("sqrt" in R) to any value that you
# want to represent with the size of nodes. You
# should multiply the square root by whatever
# value is needed to make the points visually
# appealing.
plot(sg, layout=sg_mylayout, vertex.label=NA,
     vertex.size=2*sqrt(degree(sg)))

# As a side note, we could easily include this information
# into a new dataset, if we wanted to work only with students
# in the largest component later
info_plus <- info[in.max.comp,]
info_plus$degree = degree(sg)
head(info_plus)

# Similarly, we could also look at which students 
# have the highest degree centrality
sort(degree(sg),decreasing=TRUE)
head(info_plus[order(info_plus$degree, decreasing = TRUE),])

# Metric 2: Closeness Centrality

# How "central" a node is in a network
# = 1 / sum(shortest path lengths to each of the other nodes)
# Or equivalently, = 1/(N_nodes-1) * 1/average(shortest path) 
# Recall that closeness only works on connected
# components, so we need to use the 'sg' graph!
sg_cl = closeness(sg)
summary(sg_cl)
# Basic plot
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=100*sqrt(sg_cl))
# Or with colors!
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=100*sqrt(sg_cl), 
     vertex.color = get.colors(c("white", "purple"), closeness(sg)))


# Metric 3: Betweenness Centrality
# Number of shortest paths in the network in
# which this node appears
bn = betweenness(sg)
summary(bn)
hist(bn)
# Try basic plot:
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=sqrt(bn))
# Not good! We need to shrink them a bit
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=0.2*sqrt(bn))
# Really shows the range, but is hard to look at. Try sqrt + color
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=0.2*sqrt(bn),
     vertex.color = get.colors(c("white", "purple"), bn))

# Metric 4: PageRank
# The page.rank function returns more than one thing,
# but what we want is the "$vector" after calling page.rank
pr = page.rank(sg)$vector
summary(pr)
# Plot it
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=70*sqrt(pr),
     vertex.color = get.colors(c("white", "purple"), pr))



###     PART III: Community Detection     #####

# igraph makes this pretty easy too!
# There are a few ways to cluster communities, and
# different algorithms - it's a hard problem, so some
# methods are fast and less accurate, some are more.
# We'll use the cluster_spinglass function for this graph
# (due to how the algorithm works, cluster_spinglass
#  only works if we have a single connected component;
#  if you have multiple components you might try
#  cluster_fast_greedy instead)
set.seed(144)
community = cluster_spinglass(sg, spins = 100)
# Note: spins is an integer constant, an upper limit for the number of 
# communities.

# Now we have a cluster for each node (if you set a
# different random seed you may have different clusters)
clust = community$membership
clust
table(clust)
# we can look at the breakdown by program
table(info_plus$program, clust)
# we can also look at the breakdown by section
table(info_plus$section, clust)

# And we can also look at the modularity of our community detection:
community$modularity
# pretty high!

# Finally, let's plot the clusters.
# We're doing some magic here
# to try to give everything a mix of shapes and colors.
# You don't need to understand the next lines.

# Here is a version similar to the one we used in class,
# with colors and shapes:

color = rep(brewer.pal(12, "Paired"), 8)
shape = rep(rep(c("circle", "square", "csquare", "sphere"), each=6), 2)
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=3,
     vertex.color=color[clust],
     vertex.shape=shape[clust])

# But we can also go simpler and just use colors:
color = c(brewer.pal(12, "Paired"), "black","red","blue")
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=3,
     vertex.color=color[clust])

