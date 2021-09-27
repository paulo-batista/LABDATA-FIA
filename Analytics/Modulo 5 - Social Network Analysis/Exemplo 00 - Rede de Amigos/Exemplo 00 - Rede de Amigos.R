#Simulacao inicial de uma rede
#Tonho, Benja, Bobi e Maria sao amigos de Joao
# Alice e Inacio sao amigos de Maria
#Inacio eh amigo de David

library(igraph)
library(tidyverse)

# nos 
nos <-data.frame( name = c("Tonho","Benja","Bobi","Joao","Maria","Alice","Inacio","David")
                  , gender = c("M", "M", "M", "M", "F", "F", "F", "M"), 
                  age = c( 16, 30, 42, 29, 26, 32, 18, 22) ) 

# relacoes
arestas<-data.frame( from = c("Tonho", "Benja", "Bobi", "Maria", "Alice", "Inacio", "Inacio"), 
to = c("Joao", "Joao", "Joao", "Joao","Maria", "Maria", "David") )
View(arestas)

#Construindo um objeto grafico
g <-graph.data.frame(arestas, directed=F, vertices=nos)

#Visualizacao grafica
layout1 <-g %>% layout_nicely()
g %>% plot(vertex.size = 30, layout = layout1)

## Azul para homens e rosa para mulheres
colors <-ifelse(V(g)$gender=="M", "skyblue", "pink") 
g %>% plot(vertex.size=30, vertex.color=colors, layout=layout1)

#Estatisticas da rede
V(g) #nos
E(g) #arestas

#Amigos imediatos de Joao
friends <-ego(g,order=1,nodes="Joao",mindist=1)[[1]] %>% print()

#Amigas do Joao
friends[friends$gender == "F"]

#Amigos de 1a e 2a ordem do Joao
g2 <-make_ego_graph(g, order=2, nodes="Joao")[[1]] 
g2 %>% plot(vertex.size=30, vertex.color=colors)

#Medidas de centralidade
degree<-g%>%degree()%>%print() #Qtde de conexoes
closeness<-g%>%closeness()%>%round(2)%>%print()
betweenness<-g%>%betweenness()%>%print()




