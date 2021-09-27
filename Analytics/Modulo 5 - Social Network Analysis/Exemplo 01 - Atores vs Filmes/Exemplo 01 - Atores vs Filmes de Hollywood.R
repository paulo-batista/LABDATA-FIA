#Bibliotecas necessarias
pacotes<-c("igraph", "networkD3", "sna")
install.packages (pacotes)
lapply(pacotes, library, character.only = TRUE)

#Leitura das bases de atores e de filmes
atores<- read.csv("Actors.csv",sep=",")
filmes <- read.csv("Movies.csv",sep=",")
View(atores)
View(filmes)

#Diagrama inicial da rede de atores
Rede_Atores <- graph_from_data_frame(d=filmes, vertices=atores, directed=F)
plot(Rede_Atores)


#Colorindo a conexao de acordo com o filme que os atores fizeram
E(Rede_Atores)$color <- ifelse(E(Rede_Atores)$Movie == "Forest Gump", "green", 
                                ifelse(E(Rede_Atores)$Movie == "Apollo 13", "black",
                                       "orange"))

# Plotar novamente a rede
plot(Rede_Atores)

#Colorindo os nos (atores) de acordo com sua caracteristica (ganhador ou 
#indicado ao Oscar)
V(Rede_Atores)$color <- ifelse(V(Rede_Atores)$BestActorActress == "Winner",
                                "gold",ifelse(V(Rede_Atores)$BestActorActress
                                              == "Nominated","grey","lightblue"))
#Plotar novamente a rede
plot(Rede_Atores)


#Criando legenda para apontar as cores dos nos e dos vertices
plot(Rede_Atores, vertex.frame.color="white")
legend("bottomright", c("Winner","Nominee", "Not Nominated"), pch=21,
       col="#777777", pt.bg=c("gold","grey","lightblue"), pt.cex=2, cex=.8)
legend("topleft", c("Forest Gump","Apollo 13", "The Rock"), 
       col=c("green","black","orange"), lty=1, cex=.8)

#Medidas de centralidade da rede
degree(Rede_Atores, mode="all")
betweenness(Rede_Atores, directed=F, weights=NA, normalized = T)
distances(Rede_Atores, v=V(Rede_Atores)["Kevin Bacon"], to=V(Rede_Atores), weights=NA)


