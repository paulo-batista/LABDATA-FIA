#Carregando biblioteca
library(igraph)

#Carregando dados
data <- read.table("socialnetworkdata.txt", sep=",",header=T)
View(data)

#Separando so os 2 primeiros campos
y <- data.frame(data$first, data$second)
View(y)

#Criando rede
net <- graph.data.frame(y, directed=T)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

#Histograma das conexoes da rede
hist(V(net)$degree,
     col = 'green',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')

#Diagrama da rede
set.seed(222)
plot(net,
     vertex.color = 'green',
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)

#Destacando a rede
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)

#Outra opcao de visualizacao
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.graphopt)

#Opcao adicional
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.kamada.kawai)

#Hubs
hs <- hub_score(net)$vector
as <- authority.score(net)$vector
set.seed(123)
plot(net,
     vertex.size=hs*30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

#Authotities
set.seed(123)
plot(net,
     vertex.size=as*30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

#Deteccao de comunidades
net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)
plot(cnet,
     net,
     vertex.size = 10,
     vertex.label.cex = 0.8)


