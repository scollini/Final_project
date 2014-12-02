library(igraph)
library(dplyr)
library(tidyr)
library(stringr)
library(historydata)
library(ggplot2)

#Read in the cleaned up csvs with those slaves having at least one
#kinship relation.
slaverel <- read.csv("~//Desktop/Clio-3/finalproject/slaveRelationships.csv", stringsAsFactors = FALSE)
str(slaverel) 

total_slaverel <- read.csv("~//Desktop/Clio-3/finalproject/cleanRelationships.csv", stringsAsFactors = FALSE)
total_slaverel <- total_slaverel %>%
  select(id, Gender, Birth.Year, Owner)

slave_census <- read.csv("~//Desktop/Clio-3/finalproject/cleanslavesreltotal.csv", stringsAsFactors = FALSE) %>%
  select(id, Gender, Birth.Year, Skill, Farm, Census, Owner)

#Separate out the two most important censuses: 1786 and 1799.

slave_1786 <- slave_census %>%
  filter(Census == "1786") %>%
  select(id, Census, Skill, Farm)

slave_1799 <- slave_census %>%
  filter(Census == "1799") %>%
  select(id, Census, Skill, Farm)

#Join the information in the two censuses to one dataframe.

total_slaverel <- total_slaverel %>%
  left_join(slave_1786, by = "id") %>%
  left_join(slave_1799, by = "id")

#Create a graph object

slave_graph <- graph.data.frame(slaverel, directed = "FALSE")

#Add the slaves without kinship relations to the graph as extra vertices
slave_graph <- slave_graph + vertex("Adam B", "Adam C", "Anthony A", "Austin A", "Bath A",
                                    "Bristol A", "Brunswick A", "Caesar A", "Cloe A", "Cupid A",
                                    "Daniel A", "Daphne D", "Doll A", "Doll E", "Essex A", "Frank B",
                                    "Giles A", "Hannah E", "Hercules A", "Isaac B", "Jack B", "Jack G", 
                                    "Jack H", "Jack I", "James D", "Jenny B", "Jenny E", "Joe E", 
                                    "Juba A", "Julius A", "London A", "Marcus A", "Matt A", "Milly C", 
                                    "Moll A", "Molly A", "Morris B", "Moses C", "Murria A", "Nancy I", 
                                    "Paris A", "Paul A", "Peter C", "Robin A", "Robin B", "Schomberg A", 
                                    "Spencer A", "Sue A", "Tom D", "William Lee", "Will D", "Will F", "Will H")

slave_vertex <- data_frame(name = V(slave_graph)$name) 
slave_vertex <- slave_vertex %>%
  left_join(total_slaverel, by = c("name" = "id"))

#In case I missed any people, this code would show the NAs. I finally do not
#have anyone missing!
missing <- is.na(slave_vertex$Gender)
slave_vertex$Gender[missing] <- "unknown"
slave_vertex_missing <- slave_vertex %>%
  anti_join(total_slaverel, by = c("name" = "id"))

#Graphing
igraph.options(vertex.size=3, 
               edge.arrow.size=.5)

V(slave_graph)$shape <- ifelse(slave_vertex$Gender == "Female", "circle", 
                               ifelse(slave_vertex$Gender == "Male", "square", 
                                      "rectangle"))

plot(slave_graph, vertex.label = NA)

#I would like to be able to plot multiple kinship relations on the same network
#graph. This function looks up the relationship type in the slave_graph and 
#colors the edges accordingly.

lookup_edge <- function(relationship) {
  if(relationship == "Spouse") return("red")
  if(relationship == "Child") return("orange")
  if(relationship == "Sibling") return("blue")
  return("gray")
} 

lookup_edge <- Vectorize(lookup_edge, USE.NAMES = FALSE)

E(slave_graph)$color <- lookup_edge(E(slave_graph)$relationship)

plot(slave_graph, 
     vertex.label = NA,      
     vertex.size = 3)

#Evolution of Kinship Networks in Space
#Since both slave censuses taken by Washington list the farms that each 
#slave lived and worked on, we can use that variable to locate the slave 
#families within the estate. We use a function similar to the edge coloring
#function:
lookup_color <- function(type) {
  if(is.na(type)) return("gray")
  if(type == "Mansion House") return("purple")
  if(type == "Muddy Hole") return("blue")
  if(type == "Dogue Run") return("green")
  if(type == "Union Farm") return("orange")
  if(type == "River Farm") return("yellow")
  if(type == "Mr. Lears") return("brown")
  if(type == "Mrs. Washington's") return("brown")
  if(type == "Cedar Grove") return("brown")
  return("gray")
} 

lookup_color <- Vectorize(lookup_color, USE.NAMES = FALSE)

#1786 Families Across Farms
V(slave_graph)$color <- lookup_color(slave_vertex$Farm.x)

plot(slave_graph, 
     vertex.label = NA,      
     vertex.size = 3)
title("Slave Families on Mount Vernon Farms in 1786")
legend("bottomleft", legend = c("Mansion House", "Muddy Hole", "Dogue Run", "Union Farm", "River Farm", "Other Farm", "Not Active"),
       col = c("purple", "blue", "green", "orange", "yellow", "brown", "gray"), pch = 19,   
       title = "Farm")

#1799 Families Across Farms
V(slave_graph)$color <- lookup_color(slave_vertex$Farm.y)

plot(slave_graph, 
     vertex.label = NA,      
     vertex.size = 3)
title("Slave Families on Mount Vernon Farms in 1799")
legend("bottomleft", legend = c("Mansion House", "Muddy Hole", "Dogue Run", "Union Farm", "River Farm", "Other Farm", "Not Active"),
       col = c("purple", "blue", "green", "orange", "yellow", "brown", "gray"), pch = 19,   
       title = "Farm")

#Community Detection- 
comm <- walktrap.community(slave_graph)
length(comm)

sizes(comm)

memb <- membership(comm)
names(memb[memb ==2])

plot(slave_graph, vertex.label = NA, vertex.color = memb, vertex.size = 3)
title("Families detected by walktrap.community()")

plot(comm, slave_graph, vertex.label = NA, vertex.size = 2)
title("Families detected by walktrap.community()")

induced.subgraph(slave_graph, which(membership(comm) ==2)) %>%
  plot(layout = layout.reingold.tilford, vertex.label = names(memb[memb == 2]))
title("Community 5 among Mount Vernon")
legend("bottomleft", legend = c("Spouse", "Child", "Sibling"),
       col = c("red", "orange", "blue"), pch = 19,   
       title = "Relationship")

induced.subgraph(slave_graph, which(membership(comm) == 32)) %>%
  plot(layout = layout.reingold.tilford, vertex.label = names(memb[memb == 32]))
title("Community 32 among Mount Vernon")
legend("bottomleft", legend = c("Spouse", "Child", "Sibling"),
       col = c("red", "orange", "blue"), pch = 19,   
       title = "Relationship")

#Charts- I would like to show as many meaningful visualizations as possible for this project.
#This means including charts for various demographics: total population, children, 
#mothers, gender, owner, and skills on each farm. 
#I would also like to show these visuals on the 1793 map of Mount Vernon, either through 
#geo-rectifying it or through mapping the points to the image pixels. I want to show
#how the farms were unique and related to one another through the slave community. 

#This is the general idea. I have a lot of the necessary code in other homework
#but with the messy slave data. I just need to translate it here. This would
# be in a separate Rmd file. 
slave_children_1786 <- total_slaverel %>%
  filter(Skill.x == "Child") %>%
  group_by(Farm.x) 

ggplot(data = slave_children_1786, aes(x = Farm.x, stat = "identity")) + geom_bar() + theme(axis.text.x=element_text(angle = 90, hjust = 0))

slave_children_1799 <- total_slaverel %>%
  filter(Skill.y == "Child") %>%
  group_by(Farm.y) 

ggplot(data = slave_children_1799, aes(x = Farm.y, stat = "identity")) + geom_bar() + theme(axis.text.x=element_text(angle = 90, hjust = 0))


