
#  http://www.sthda.com/english/articles/33-social-network-analysis/136-network-analysis-and-manipulation-using-r/


library(tidyverse)
library(tidygraph)
library(ggraph)
library(corrr)
library(navdata)
#library(navdata)  #this is where the data is found.  NOTE look for error msg: 
#WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:
#https://cran.rstudio.com/bin/windows/Rtools/
# tbl_graph  creates network objects  from Nodes & Edges
# as_tble_graph  conversts network data to tbl_graph network
install.packages("navdata")
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/navdata")
library(navdata)

data("phone.call2")

phone.net <- tbl_graph(
  nodes = phone.call2$nodes, 
  edges = phone.call2$edges,
  directed = TRUE)

ggraph(phone.net, layout = "graphopt") + 
  geom_edge_link(width = 1, colour = "lightgray") +
  geom_node_point(size = 4, colour = "#00AFBB") +
  geom_node_text(aes(label = label), repel = TRUE)+
  theme_graph()

# One can also use the as_tbl_graph() function
# to convert to network object

'''
In the following example, we’ll create a 
correlation matrix network graph. 
The mtcars data set will be used.
'''

# 1 use the mt cars set
#2 computate the correlation matrix
#3 convert upper triangle to NA :shave()
#4 Stretch the corelatuion to long format
#5  keep only high correlation

library(corrr)
res.cor <- mtcars [, c(1, 3:6)] %>%  # (1)
  t() %>% correlate() %>%            # (2)
  shave(upper = TRUE) %>%            # (3)
  stretch(na.rm = TRUE) %>%          # (4)
  filter(r >= 0.998)                 # (5)
res.cor

# CREATE graph
set.seed(1)
cor.graph <- as_tbl_graph(res.cor, directed = FALSE)
ggraph(cor.graph) + 
  geom_edge_link() + 
  geom_node_point() +
  geom_node_text(
    aes(label = name), size = 3, repel = TRUE
  ) +
  theme_graph()


cor.graph
# 24 niodes and 59 edges
#nodes are car names.
# edges are correlation links

'''
If you want to rearrange the rows in the 
edges tibble to list those with the highest
“r” first, you could use activate() and then 
arrange(). For example, type the following R code:

'''
cor.graph %>% 
  activate(edges) %>% 
  arrange(desc(r))


'''
Note that, to extract the current active data
as a tibble, you can use the function 
as_tibble(cor.graph).

'''

'''
 you can easily manipulate the nodes and 
 the edges data in the network graph object 
 Modify the nodes data:
Group the cars by the “cyl” variable (number of cylinders) in the original mtcars data set. We’ll color the cars by groups.
Join the group info to the nodes data
Rename the column “name”, in the nodes data, to “label”
'''

# Car groups info
cars.group <- data_frame(
  name = rownames(mtcars),
  cyl = as.factor(mtcars$cyl)
)
 cars.group

 # Modify the nodes data
 cor.graph <- cor.graph %>%
   activate(nodes) %>%
   left_join(cars.group, by = "name") %>%
   rename(label = name) 
cor.graph 


#Modify the edge data.
#Rename the column “r” to “weight”.
cor.graph <- cor.graph %>%
  activate(edges) %>%
  rename(weight = r)

cor.graph

#####GRAPH with color coded nodes
set.seed(1)
ggraph(cor.graph) + 
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  scale_edge_width(range = c(0.2, 1)) +
  geom_node_point(aes(color = cyl), size = 2) +
  geom_node_text(aes(label = label), size = 3, repel = TRUE) +
  theme_graph()


# NETWORK ANALYSIS 