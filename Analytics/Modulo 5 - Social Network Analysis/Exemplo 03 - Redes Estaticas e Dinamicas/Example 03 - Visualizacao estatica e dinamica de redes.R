#Bibliotecas bacanas de SNA
install.packages("igraph") 
install.packages("network") 
install.packages("sna")
install.packages("ggraph")
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("ndtv")

nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

View(nodes)
View(links)

library('igraph')
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"

plot(net)

# We'll start by adding new node and edge attributes to our dataframes. 
vis.nodes <- nodes
vis.links <- links
library(visNetwork)

#Uso do pacote visNetwork() para visualizacao dinamica
vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$media # Text on click
vis.nodes$label  <- vis.nodes$type.label # Node label
vis.nodes$size   <- vis.nodes$audience.size # Node size
vis.nodes$borderWidth <- 2 # Node border width
vis.nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"
visNetwork(vis.nodes, vis.links)

#Uso do pacote visNetwork() para visualizacao dinamica
vis.links$width <- 1+links$weight/8 # line width
vis.links$color <- "gray"    # line color  
vis.links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow
visnet <- visNetwork(vis.nodes, vis.links)
visnet

#Visualizacao isolada de cada elemento
visOptions(visnet, highlightNearest = TRUE, selectedBy = "type.label")


#Forma alternativa de visualizacao da rede
nodes$group <- nodes$type.label 
visnet3 <- visNetwork(nodes, links)
visnet3 <- visGroups(visnet3, groupname = "Newspaper", shape = "square",
                     color = list(background = "gray", border="black"))
visnet3 <- visGroups(visnet3, groupname = "TV", shape = "dot",       
                     color = list(background = "tomato", border="black"))
visnet3 <- visGroups(visnet3, groupname = "Online", shape = "diamond",   
                     color = list(background = "orange", border="black"))
visLegend(visnet3, main="Legend", position="right", ncol=1) 

#Aspectos da rede
hist(links$weight)
mean(links$weight)
sd(links$weight)

cut.off <- mean(links$weight) 
net.sp <- delete_edges(net, E(net)[weight<cut.off])
plot(net.sp, layout=layout_with_kk) 


#Deteccao de comunidades
par(mfrow=c(1,2))

# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(net)
class(clp)

# Community detection returns an object of class "communities" 
# which igraph knows how to plot: 
plot(clp, net)

# We can also plot the communities without relying on their built-in plot:
V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])

dev.off()



