# load libraries
library(tidyverse)
library(igraph)

#set working directory before any analysis

# load datasets
responses <- read.csv('class-responses.csv')
names <- read.csv('class-names.csv')

##filter the responses so only connections w weight >=3 are shown
el_3 <- responses %>% filter(weight >= 3)

##change the name of the column in the names and filtered responses to the name header "rater"
names(names)[names(names)=="id"] <-"rater"

##get graph for network
g <- graph_from_data_frame(el_3)

##retain only mutual connections
g_mutual <- subgraph.edges(g, eids = E(g)[which_mutual(g)], delete.vertices = TRUE)
summary(g_mutual)

#plot network
set.seed(12)
par(mar=c(0,0,1,0)+.1)
plot(g_mutual, vertex.label = NA, vertex.size=7, edge.arrow.size = 0.3, layout = layout_nicely)

## collapse to make unweighted, undirected
ud_g <- as.undirected(g_mutual, mode = 'collapse')


## to get edgelist from subset of mutual connections
el_mutual_ud <- get.edgelist(ud_g)

#Plot the network
set.seed(2)
layout <- layout_nicely(g_mutual)
plot(ud_g, vertex.label = NA, vertex.size=7, edge.arrow.size = 0.3, layout = layout)

#Create random network

set.seed(7)

random_network <- sample_gnm(n = 48, m = 56,
                             directed = FALSE, loops = FALSE)
#transitivity
transitivity(ud_g, type = 'global')
transitivity(random_network, type = 'global')

#Average Shortest Path Length
average.path.length(ud_g)
average.path.length(random_network, directed = FALSE)



#Clustering methods

# louvain methods
set.seed(1)
cl_lv <- cluster_louvain(ud_g, weights = E(ud_g)$weight)
mod_lv <- modularity(cl_lv)

# random walker methods
set.seed(1)
cl_rw <- cluster_walktrap(ud_g, weights = E(ud_g)$weight)
mod_rw <- modularity(cl_rw)

#edge betweeness methods
set.seed(1)
cl_eb <-cluster_edge_betweenness(ud_g, weights =E(ud_g)$weight)
mod_eb <-modularity(cl_eb)

#infomap methods
set.seed(1)
cl_im <-cluster_infomap(ud_g, e.weights =E(ud_g)$weight)
mod_im<-modularity(cl_im)

#adding memberships
V(ud_g)$membership_lm <- cl_lv$membership
V(ud_g)$membership_rw <- cl_rw$membership
V(ud_g)$membership_cb <-cl_eb$membership
V(ud_g)$membership_im <-cl_im$membership

#plots
set.seed(123)

par(mfrow=c(2,2), mar=c(0,0,1,0)+.1)
plot(ud_g, vertex.color=V(ud_g)$membership_lm, 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA,
     main = "Louvain")
plot(ud_g, vertex.color=V(ud_g)$membership_rw, 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA, 
     main = "Random Walk")
plot(ud_g, vertex.color=V(ud_g)$membership_cb, 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA, 
     main = "Edge Betweeness")
plot(ud_g, vertex.color=V(ud_g)$membership_im, 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA, 
     main = "Infomap")

# making it all the same layout
layout <- layout_nicely(ud_g)

par(mfrow=c(2,2), mar=c(0,0,1,0)+.1)
for (i in 1:4) {
  plot(ud_g, 
       layout=layout,  # Use the same layout for all plots
       vertex.color=get.vertex.attribute(ud_g, paste("membership_", c("lm", "rw", "cb", "im")[i], sep="")), 
       vertex.size = 9, 
       vertex.frame.color = 'white', 
       edge.arrow.size = 0.3, 
       vertex.label = NA,
       main = c("Louvain", "Random Walk", "Edge Betweeness", "Infomap")[i])
}
# modularities

methods <- c('Louvain', 'Random Walk', 'Edge Betweeness','Infomap')
mods <- data.frame(mod_lv, mod_rw, mod_eb, mod_im)
colnames(mods) <- methods
View(mods)


## choose random walker method 

plot(ud_g, vertex.color=V(ud_g)$membership_rw, 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = V(ud_g)$name, 
     main = "Random Walk",
     layout=layout)


##view the community membership summary
membership <- V(ud_g)$membership_rw
community_summary <- data.frame(Node = V(ud_g)$name, Community = membership)

##read updated class namelist (with only members of ud_g)
class.names.cleaned <- read.csv('class-names-cleaned.csv')

##change the header for the nodes to be same as community_summary
names(class.names.cleaned)[names(class.names.cleaned) == "id"] <- "Node"

##merge the demographic info of students with the community structure
nac_df <- merge(community_summary,class.names.cleaned)

##export to CSV to change the headers
write.csv(el_mutual_ud,file='edgelist.csv', row.names=FALSE)

##once headers changed, merge so that all the data can be in one dataset and import it back
allinfo_ds <-merge(edgelist, nac_df)
View(allinfo_ds)

##make sure demographic and community information are arranged the same way as in ud_g
demo.info <- demo.info %>% arrange(factor(Node, levels = V(ud_g)$name))
community_summary <- community_summary %>% arrange(factor(Node, levels = V(ud_g)$name))

##add all the attributes
ud_g <-set_vertex_attr(ud_g, name = 'Community',value = community_summary$Community)
ud_g <-set_vertex_attr(ud_g, name= 'gender', value= demo.info$gender)
ud_g <-set_vertex_attr(ud_g, name= 'year_of_study', value= demo.info$year_of_study)
ud_g <-set_vertex_attr(ud_g, name= 'ethnicity', value= demo.info$ethnicity)
ud_g <-set_vertex_attr(ud_g, name= 'HM_or_not', value= demo.info$HM_or_not)

##Plot all the attributes
set.seed(123)
par(mfrow=c(2,3), mar=c(0,0,1,0)+.1)
plot(ud_g, vertex.color=factor(V(ud_g)$Community), 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA,
     main = "Community",
     layout= layout)
plot(ud_g, vertex.color=factor(V(ud_g)$gender), 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA,
     main = "Gender",
     layout= layout)
plot(ud_g, vertex.color=factor(V(ud_g)$year_of_study), 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA,
     main = "Year of Study",
     layout= layout)
plot(ud_g, vertex.color=factor(V(ud_g)$ethnicity), 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA,
     main = "Ethnicity",
     layout= layout)
plot(ud_g, vertex.color=factor(V(ud_g)$HM_or_not), 
     vertex.size = 9, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA,
     main = "HM_or_not",
     layout= layout)

##assortativity
by_deg <- assortativity_degree(ud_g, directed=F)
by_gen <- assortativity_nominal(ud_g, types = factor(V(ud_g)$gender), directed = F)
by_yos <- assortativity_nominal(ud_g, types = factor(V(ud_g)$year_of_study), directed = F)
by_eth <- assortativity_nominal(ud_g, types = factor(V(ud_g)$ethnicity), directed = F)
by_hm <- assortativity_nominal(ud_g, types = factor(V(ud_g)$HM_or_not), directed = F)


assort_methods <- c('Degree',  'Gender', 'Year of Study','Ethnicity', 'HM_or_not')
assort_mods <- data.frame(by_deg, by_gen, by_yos, by_eth, by_hm)
colnames(assort_mods) <- assort_methods
View(assort_mods)



##analysing the gender connections
E(ud_g)$edge_type <- 'same gender'

random_network <- sample_gnm(n = 48, m = 56,
                             directed = FALSE, loops = FALSE)

E(ud_g)$edge_type[E(ud_g)[V(ud_g)[V(ud_g)$gender == 'Male'] %--% V(ud_g)[V(ud_g)$gender == 'Female']]]<- 'different gender'
summary(ud_g)
E(ud_g)$edge_type

set.seed(123)
par(mfrow=c(1,2), mar=c(0,0,1,0)+.1)

#plot community structure showing gender homophily of connections
plot(ud_g, vertex.color=factor(V(ud_g)$Community), 
     vertex.size = 5.5, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA,
     edge.color = c('red', 'blue')[factor(E(ud_g)$edge_type)],
     main = "Community",
     layout= layout)

#plot gender and showing gender homophily of connections
plot(ud_g, vertex.color=factor(V(ud_g)$gender), 
     vertex.size = 5.5, 
     vertex.frame.color = 'white', 
     edge.arrow.size = 0.3, 
     vertex.label = NA,
     edge.color = c('red', 'blue')[factor(E(ud_g)$edge_type)],
     main = "Gender",
     layout= layout)

#count the number of same and different gender connections
same_gender_count <- length(E(ud_g)[edge_type == 'same gender'])
different_gender_count <- length(E(ud_g)[edge_type == 'different gender'])
#print the counts
cat("Number of same gender connections:", same_gender_count, "\n")
cat("Number of different gender connections:", different_gender_count, "\n")

#number of connections between different genders
E(ud_g)$etype <- 'same'
E(ud_g)$etype[
  E(ud_g)[
    V(ud_g)[V(ud_g)$gender == 'Female'] %--% V(ud_g)[V(ud_g)$gender == 'Male']
  ]
] <- 'different'


table(factor(E(ud_g)$etype))

#number of same gender connections among males
E(ud_g)$etype[
  E(ud_g)[
    V(ud_g)[V(ud_g)$gender == 'Male'] %--% V(ud_g)[V(ud_g)$gender == 'Male']
  ]
] <- 'sameM'

#number of same gender connections among females
E(ud_g)$etype[
  E(ud_g)[
    V(ud_g)[V(ud_g)$gender == 'Female'] %--% V(ud_g)[V(ud_g)$gender == 'Female']
  ]
] <- 'sameF'

#summary table
table(factor(E(ud_g)$etype))

#Conduct Fisher's exact test
fet_dat <- data.frame(
  "male" = c(9, 19),
  "female" = c(28, 19),
  row.names = c("same gender", "different gender"),
  stringsAsFactors = FALSE
)
colnames(fet_dat) <- c("male", "female")

fet_dat

test <-fisher.test(fet_dat)

