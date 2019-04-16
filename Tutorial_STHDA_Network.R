
#  http://www.sthda.com/english/articles/33-social-network-analysis/136-network-analysis-and-manipulation-using-r/


library(tidyverse)
library(tidygraph)
library(ggraph)
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
