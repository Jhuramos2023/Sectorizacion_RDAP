# Install packages
install.packages(c("igraph","readr","tidyr","RColorBrewer"))
install.packages("ggthemes")
install.packages("pander")
library(ggraph)
library(ggthemes)
library(dplyr)
library(igraph)
library(pander)
library (readr)

#========================================================================#
#====================== Data Pre-procesamiento ==========================#
#========================================================================#
# En esta seccion, recuperara un archivo de datos de red en formato csv  #
# al estudio R como un marco de datos y convertirlo en el igraph         #
# objeto.                                                                #
#========================================================================#

###########=================== SECTOR 3 ======================###########

#1. Leer los archivos desde la ubicacion de una red 
nodesdat<- read.table("C:\\Users\\User\\Desktop\\TESIS JAIME\\Modelamiento RStudio\\nodesdat2_sector3.csv", header=TRUE, sep=";")
pipesdat<- read.table("C:\\Users\\User\\Desktop\\TESIS JAIME\\Modelamiento RStudio\\pipesdat2_sector3.csv", header=TRUE, sep=";")

pipesdat<-as.data.frame(pipesdat)   #convertir a dataframe las tuberias   
DJ<-pipesdat[,c(2,3,4)]   #seleccionar las columnas de la 2 a la 4
nodesdat<-as.data.frame(nodesdat)   #convertir a dataframe los vertices
DJ_meta=nodesdat
DJ_meta$Cota_1=DJ_meta[,4]   #Duplicar una columna

#2. Administrar conjunto de datos (EN CASO DE DARLE PESO CON LA FRECUENCIA DE DATOS)
B<-as.data.frame(table(DJ)) # Cree una columna de peso de borde llamada "Freq"
B1<-subset(B,Freq>0) # Eliminar todos los bordes que tengan un peso igual a 0

#3.  Crear un objeto igraph a partir de los marcos de datos.
library(igraph)
# A menos que su lista de bordes en B1 se reconozca como 'factor', producira un error
Stucont_J<-graph_from_data_frame(DJ, directed = TRUE, vertices = DJ_meta)
E(Stucont_J)$weight<-E(Stucont_J)$Length  #Aqui se reconoce a la longitud como peso
Stucont_J #grafo construido

#==================================================================#
#=================== Explore sus datos igraph =====================#
#==================================================================#
#  En esta seccion, exploraron el objeto igraph nombrado           #
# "Estudiante". Veras el resumen del objeto igraph con             #
#  su estructura de datos.                                         #
#==================================================================#

#1. igraph summary
Stucont_J
gsize(Stucont_J)    #numero de enlaces
gorder(Stucont_J)   #grado

#2. Nodelist
V(Stucont_J) #vertices

#3. Edgelist
E(Stucont_J) #enlaces

#4. Attributes  (revisar los atributos)
V(Stucont_J)$Cota_1    #para ver si valores atipicos se colan 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # AQUI EMPIEZA EL PASO REVERSIBLE # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

as.numeric(DJ_meta$Cota_1) #por siacaso lo vuelves factor numerico

################ Para la particion por intervalos ################ 
x <- DJ_meta$Cota_1
categorias <- cut(x, breaks = c(-1, 60, 70, 80, Inf),
                  labels = c("SEC1", "SEC2", "SEC3", "SEC4"))
particion <- data.frame(x, categorias)

DJ_meta_cota=group_by(particion,
                      categorias) %>%
  summarise(n=n())

#Reemplazar la columna cota_1 por las categorias
DJ_meta$Cota_1=particion$categorias

########## Volver a la creacion de grafo

Stucont_J<-graph_from_data_frame(DJ, directed = TRUE, vertices = DJ_meta)
E(Stucont_J)$weight<-E(Stucont_J)$Length  #Aqui se reconoce a la longitud como peso
Stucont_J

#5. Adjacency matrix (Matriz de adyacencia)
Stucont_J[c(1:10),c(1:10)] #el numero solo es para ver una matrix de zxz

a=Stucont_J[c(1:171),c(1:171)]
jaimito=as.data.frame.matrix(a)

setwd("C:\\Users\\User\\Desktop")

install.packages("rio")
library(rio)
export(jaimito,"jaimito.xlsx")


#==================================================================#
#=================== Midiendo la Centralidad ======================#
#==================================================================#
# En esta seccion, mediras la centralidad del igrafo                  #
# objeto, "Stucont". Podras ver como funciona la teorica              #
# El concepto de cada centralidad, como el grado, el vector propio y  #
# la centralidad de intermediacion, se mide mediante el igraph.       #
#==================================================================   #

#1. Grado de centralidad
Stucont_deg_J<-degree(Stucont_J,mode=c("All"))     #grado de las aristas            
V(Stucont_J)$degree<-Stucont_deg_J                 #agregas la columna de grados                   
V(Stucont_J)$degree                                #visualizas los grados de los nodos              
which.max(Stucont_deg_J)                          #indica que vertice tiene el mayor grado 


jaimito=as.data.frame.vector(V(Stucont_J)$degree)

setwd("C:\\Users\\User\\Desktop")
library(rio)
export(jaimito,"jaimito.xlsx")

#2. Centralidad del vector propio
Stucont_eig_J <- evcent(Stucont_J)$vector    #la centralidad se mide en funcion de la importancia de los vecinos
V(Stucont_J)$Eigen<-Stucont_eig_J
V(Stucont_J)$Eigen
which.max(Stucont_eig_J)




#3. Centralidad de intermediación
Stucont_bw_J<-betweenness(Stucont_J, directed = FALSE)
V(Stucont_J)$betweenness<-Stucont_bw_J
V(Stucont_J)$betweenness
which.max(Stucont_bw_J)

DF_J<-as_long_data_frame(Stucont_J)
Stucont_J

#==================================================================#
#============== Medicion de la estructura de la red ===============# 
#==================================================================#
# En esta seccion mediras los indicadores de la red                #
# estructura como la densidad de la red, la variedad.              #
#==================================================================#

#1. Densidad de red
edge_density(Stucont_J) # Densidad global de toda la red
SEC1<-induced_subgraph(Stucont_J, V(Stucont_J)[Cota_1=="SEC1"], impl=c("auto")) #Subgrafiar en cada clase o intervalos de cota
edge_density(SEC1) # Densidad de nivel de Cota

#2. Assortativity ----> indica que tanto se unen a vertices con similares caracteristicas
values_J <- as.numeric(factor(V(Stucont_J)$Cota_1))
assortativity_nominal(Stucont_J, types=values_J)   #si sale positivo es porque los atributos de los vertices son similares

#2.1. Calculate the observed assortativity (Calcular la variedad observada)
observed.assortativity_J <- assortativity_nominal(Stucont_J, types=values_J)
results_J <- vector('list', 1000)
for(i in 1:1000){results_J[[i]] <- assortativity_nominal(Stucont_J, sample(values_J))}  "sample para crear valores aleatorios"

#2.2.  Trace la distribucion de los valores de surtido y agregue una linea vertical roja en el valor original observado
hist(unlist(results_J), xlim = c(0,1))
abline(v = observed.assortativity_J,col = "red", lty = 3, lwd=2)

#==================================================================#
#===================== Network Visualization ======================#
#==================================================================#

#1. Trazar una red con el grado de centralidad

set.seed(1001)
library(RColorBrewer) # Esta es la biblioteca de colores.
pal<-brewer.pal(length(unique(V(Stucont_J)$Cota_1)), "Set3") # Color de vértice asignado por cada número de clase
plot(Stucont_J,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont_J, "Cota_1")))],
     vertex.size = sqrt(Stucont_deg_J)/3, edge.width=sqrt(E(Stucont_J)$weight/800),
     edge.arrow.width=0.5,
     edge.arrow.size=0.08,
     edge.curved = T,
     layout = layout.fruchterman.reingold)

#1. Trazar una red con la centralidad del vector propio

set.seed(1001)
plot(Stucont_J,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont_J, "Cota_1")))],
     vertex.size = sqrt(Stucont_eig_J)*10, edge.width=sqrt(E(Stucont_J)$weight/800),
     edge.arrow.width=0.5,
     edge.arrow.size=0.08,
     edge.curved = T,
     layout = layout.fruchterman.reingold)

#2. Trazado de una red con la centralidad de intermediacion

set.seed(1001)
plot(Stucont_J,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont_J, "Cota_1")))],
     vertex.size = sqrt(Stucont_bw_J)/3, edge.width=sqrt(E(Stucont_J)$weight/800),
     edge.arrow.width=0.5,
     edge.arrow.size=0.08,
     edge.curved = T,
     layout = layout.fruchterman.reingold)

# Camino más corto entre 1 y 5
sp <- shortest.paths(Stucont_J, v = "REP-01-5", to = "J-167-5")    
sp[]                                         # Distancia 
gsp <- get.shortest.paths(Stucont_J, from = "REP-01-5", to = "J-167-5")
V(Stucont_J)[gsp$vpath[[1]]]                           # Secuencia de vértices 

#########################################################################
#####################     Clustering Jerarquico     #####################
#########################################################################
install.packages("factoextra")
library(cluster)
library(factoextra)

nodesdat<- read.table("C:\\Users\\User\\Desktop\\TESIS JAIME\\Modelamiento RStudio\\nodesdat2_sector3.csv", header=TRUE, sep=";",row.names = 1)
DJ_meta=nodesdat
DJ_meta

df <- DJ_meta
df
#normalizar las puntuaciones
df <- scale(df)
head(df)

#calcular la matriz de distacias
m.distancia <- get_dist(df, method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#estimar el número de clústers
#Elbow, silhouette o gap_stat  method
fviz_nbclust(df, pam, method = "wss")
fviz_nbclust(df, pam, method = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_stat")

#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

#calculamos los dos clústers
k2 <- kmeans(df, centers = 5, nstart = 25)
k2
str(k2)

#plotear los cluster
fviz_cluster(k2, data = df)
fviz_cluster(k2, data = df, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k2, data = df, ellipse.type = "norm")
fviz_cluster(k2, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

res2 <- hcut(df, k = 5, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black","blue","coral","chocolate"))

#pasar los cluster a mi df inicial para trabajar con ellos

DJ_meta %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

df <- DJ_meta
df
df$clus<-as.factor(k2$cluster)
df














###########=================== SECTOR 2 ======================###########

#1. Leer los archivos desde la ubicacion de una red 
nodesdat<- read.table("C:\\Users\\User\\Desktop\\TESIS JAIME\\Modelamiento RStudio\\nodesdat2_sector2.csv", header=TRUE, sep=";")
pipesdat<- read.table("C:\\Users\\User\\Desktop\\TESIS JAIME\\Modelamiento RStudio\\pipesdat2_sector2.csv", header=TRUE, sep=";")

pipesdat<-as.data.frame(pipesdat)   #convertir a dataframe las tuberias   
DJ<-pipesdat[,c(2,3,4)]   #seleccionar las columnas de la 2 a la 4
nodesdat<-as.data.frame(nodesdat)   #convertir a dataframe los vertices
DJ_meta=nodesdat
DJ_meta$Cota_1=DJ_meta[,4]   #Duplicar una columna

#2. Administrar conjunto de datos (EN CASO DE DARLE PESO CON LA FRECUENCIA DE DATOS)
B<-as.data.frame(table(DJ)) # Cree una columna de peso de borde llamada "Freq"
B1<-subset(B,Freq>0) # Eliminar todos los bordes que tengan un peso igual a 0

#3.  Crear un objeto igraph a partir de los marcos de datos.
library(igraph)
# A menos que su lista de bordes en B1 se reconozca como 'factor', producira un error
Stucont_J<-graph_from_data_frame(DJ, directed = TRUE, vertices = DJ_meta)
E(Stucont_J)$weight<-E(Stucont_J)$Length  #Aqui se reconoce a la longitud como peso
Stucont_J #grafo construido

#==================================================================#
#1. igraph summary
Stucont_J
gsize(Stucont_J)    #344
gorder(Stucont_J)   #278

#2. Nodelist
V(Stucont_J) #vertices

#3. Edgelist
E(Stucont_J) #enlaces

#4. Attributes  (revisar los atributos)
V(Stucont_J)$Cota_1    #para ver si valores atipicos se colan 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
as.numeric(DJ_meta$Cota_1) #por siacaso lo vuelves factor numerico

################ Para la particion por intervalos ################ 
x <- DJ_meta$Cota_1
categorias <- cut(x, breaks = c(-1, 60, 70, 80, Inf),
                  labels = c("SEC1", "SEC2", "SEC3", "SEC4"))
particion <- data.frame(x, categorias)

DJ_meta_cota=group_by(particion,
                      categorias) %>%
  summarise(n=n())

#Reemplazar la columna cota_1 por las categorias
DJ_meta$Cota_1=particion$categorias

########## Volver a la creacion de grafo
Stucont_J<-graph_from_data_frame(DJ, directed = TRUE, vertices = DJ_meta)
E(Stucont_J)$weight<-E(Stucont_J)$Length  #Aqui se reconoce a la longitud como peso
Stucont_J

#5. Adjacency matrix (Matriz de adyacencia)
Stucont_J[c(1:10),c(1:10)] #el numero solo es para ver una matrix de zxz

#==================================================================#
#1. Grado de centralidad
Stucont_deg_J<-degree(Stucont_J,mode=c("All"))     #grado de las aristas            
V(Stucont_J)$degree<-Stucont_deg_J                 #agregas la columna de grados                   
V(Stucont_J)$degree                                #visualizas los grados de los nodos              
which.max(Stucont_deg_J)                          #J-98-4  #96 

#2. Centralidad del vector propio
Stucont_eig_J <- evcent(Stucont_J)$vector     #la centralidad se mide en funcion de la importancia de los vecinos
V(Stucont_J)$Eigen<-Stucont_eig_J
V(Stucont_J)$Eigen
which.max(Stucont_eig_J)                      #J-277-4    #271

#3. Centralidad de intermediación
Stucont_bw_J<-betweenness(Stucont_J, directed = FALSE)
V(Stucont_J)$betweenness<-Stucont_bw_J
V(Stucont_J)$betweenness
which.max(Stucont_bw_J)

DF_J<-as_long_data_frame(Stucont_J)
Stucont_J

#==================================================================#
#1. Densidad de red
edge_density(Stucont_J) # Densidad global de toda la red
SEC1<-induced_subgraph(Stucont_J, V(Stucont_J)[Cota_1=="SEC1"], impl=c("auto")) #Subgrafiar en cada clase o intervalos de cota
edge_density(SEC1) # Densidad de nivel de Cota

#2. Assortativity ----> indica que tanto se unen a vertices con similares caracteristicas
values_J <- as.numeric(factor(V(Stucont_J)$Cota_1))
assortativity_nominal(Stucont_J, types=values_J)   #si sale positivo es porque los atributos de los vertices son similares

#2.1. Calculate the observed assortativity (Calcular la variedad observada)
observed.assortativity_J <- assortativity_nominal(Stucont_J, types=values_J)
results_J <- vector('list', 1000)
for(i in 1:1000){results_J[[i]] <- assortativity_nominal(Stucont_J, sample(values_J))}  "sample para crear valores aleatorios"

#2.2.  Trace la distribucion de los valores de surtido y agregue una linea vertical roja en el valor original observado
hist(unlist(results_J), xlim = c(0,1))
abline(v = observed.assortativity_J,col = "red", lty = 3, lwd=2)

#==================================================================#

#1. Trazar una red con el grado de centralidad

set.seed(1001)
library(RColorBrewer) # Esta es la biblioteca de colores.
pal<-brewer.pal(length(unique(V(Stucont_J)$Cota_1)), "Set3") # Color de vértice asignado por cada número de clase
plot(Stucont_J,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont_J, "Cota_1")))],
     vertex.size = sqrt(Stucont_deg_J)/3, edge.width=sqrt(E(Stucont_J)$weight/800),
     edge.arrow.width=0.5,
     edge.arrow.size=0.06,
     edge.curved = T,
     layout = layout.fruchterman.reingold)

#1. Trazar una red con la centralidad del vector propio

set.seed(1001)
plot(Stucont_J,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont_J, "Cota_1")))],
     vertex.size = sqrt(Stucont_eig_J)*10, edge.width=sqrt(E(Stucont_J)$weight/800),
     edge.arrow.width=0.5,
     edge.arrow.size=0.06,
     edge.curved = T,
     layout = layout.fruchterman.reingold)

#2. Trazado de una red con la centralidad de intermediacion

set.seed(1001)
plot(Stucont_J,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont_J, "Cota_1")))],
     vertex.size = sqrt(Stucont_bw_J)/3, edge.width=sqrt(E(Stucont_J)$weight/800),
     edge.arrow.width=0.5,
     edge.arrow.size=0.06,
     edge.curved = T,
     layout = layout.fruchterman.reingold)

# Camino más corto entre 1 y 5
sp <- shortest.paths(Stucont_J, v = "RAP-03-4", to = "J-271-4")
sp[]                                         # Distancia 
gsp <- get.shortest.paths(Stucont_J, from = "RAP-03-4", to = "J-271-4")
V(Stucont_J)[gsp$vpath[[1]]]                           # Secuencia de vértices 

#########################################################################
#####################     Clustering Jerarquico     #####################
#########################################################################
nodesdat<- read.table("C:\\Users\\User\\Desktop\\TESIS JAIME\\Modelamiento RStudio\\nodesdat2_sector2.csv", header=TRUE, sep=";",row.names = 1)
DJ_meta=nodesdat
DJ_meta

df <- DJ_meta
df
#normalizar las puntuaciones
df <- scale(df)
head(df)

#calcular la matriz de distacias
m.distancia <- get_dist(df, method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#estimar el número de clústers
#Elbow, silhouette o gap_stat  method
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_stat")

#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

#calculamos los dos clústers
k2 <- kmeans(df, centers = 3, nstart = 25)
k2
str(k2)

#plotear los cluster
fviz_cluster(k2, data = df)
fviz_cluster(k2, data = df, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k2, data = df, ellipse.type = "norm")
fviz_cluster(k2, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

res2 <- hcut(df, k = 3, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black","blue","coral","chocolate"))

#pasar los cluster a mi df inicial para trabajar con ellos

DJ_meta %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

df <- DJ_meta
df
df$clus<-as.factor(k2$cluster)
df

















###########=================== SECTOR 1 ======================###########

#1. Leer los archivos desde la ubicacion de una red 
nodesdat<- read.table("C:\\Users\\User\\Desktop\\TESIS JAIME\\Modelamiento RStudio\\nodesdat2_sector1.csv", header=TRUE, sep=";")
pipesdat<- read.table("C:\\Users\\User\\Desktop\\TESIS JAIME\\Modelamiento RStudio\\pipesdat2_sector1.csv", header=TRUE, sep=";")

pipesdat<-as.data.frame(pipesdat)   #convertir a dataframe las tuberias   
DJ<-pipesdat[,c(2,3,4)]   #seleccionar las columnas de la 2 a la 4
nodesdat<-as.data.frame(nodesdat)   #convertir a dataframe los vertices
DJ_meta=nodesdat
DJ_meta$Cota_1=DJ_meta[,4]   #Duplicar una columna

#2. Administrar conjunto de datos (EN CASO DE DARLE PESO CON LA FRECUENCIA DE DATOS)
B<-as.data.frame(table(DJ)) # Cree una columna de peso de borde llamada "Freq"
B1<-subset(B,Freq>0) # Eliminar todos los bordes que tengan un peso igual a 0

#3.  Crear un objeto igraph a partir de los marcos de datos.
# A menos que su lista de bordes en B1 se reconozca como 'factor', producira un error
Stucont_J<-graph_from_data_frame(DJ, directed = TRUE, vertices = DJ_meta)
E(Stucont_J)$weight<-E(Stucont_J)$Length  #Aqui se reconoce a la longitud como peso
Stucont_J #grafo construido

#==================================================================#
#1. igraph summary
Stucont_J
gsize(Stucont_J)    #244
gorder(Stucont_J)   #179

#2. Nodelist
V(Stucont_J) #vertices

#3. Edgelist
E(Stucont_J) #enlaces

#4. Attributes  (revisar los atributos)
V(Stucont_J)$Cota_1    #para ver si valores atipicos se colan 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
as.numeric(DJ_meta$Cota_1) #por siacaso lo vuelves factor numerico

################ Para la particion por intervalos ################ 
x <- DJ_meta$Cota_1
categorias <- cut(x, breaks = c(-1, 40, 60, 80, Inf),
                  labels = c("SEC1", "SEC2", "SEC3", "SEC4"))
particion <- data.frame(x, categorias)

DJ_meta_cota=group_by(particion,
                      categorias) %>%
  summarise(n=n())

#Reemplazar la columna cota_1 por las categorias
DJ_meta$Cota_1=particion$categorias

########## Volver a la creacion de grafo
Stucont_J<-graph_from_data_frame(DJ, directed = TRUE, vertices = DJ_meta)
E(Stucont_J)$weight<-E(Stucont_J)$Length  #Aqui se reconoce a la longitud como peso
Stucont_J

#5. Adjacency matrix (Matriz de adyacencia)
Stucont_J[c(1:10),c(1:10)] #el numero solo es para ver una matrix de zxz

#==================================================================#
#1. Grado de centralidad
Stucont_deg_J<-degree(Stucont_J,mode=c("All"))     #grado de las aristas            
V(Stucont_J)$degree<-Stucont_deg_J                 #agregas la columna de grados                   
V(Stucont_J)$degree                                #visualizas los grados de los nodos              
which.max(Stucont_deg_J)                           #J-5-3  #144  

#2. Centralidad del vector propio
Stucont_eig_J <- evcent(Stucont_J)$vector     #la centralidad se mide en funcion de la importancia de los vecinos
V(Stucont_J)$Eigen<-Stucont_eig_J
V(Stucont_J)$Eigen
which.max(Stucont_eig_J)                      #J-34-3    #167

#3. Centralidad de intermediación
Stucont_bw_J<-betweenness(Stucont_J, directed = FALSE)
V(Stucont_J)$betweenness<-Stucont_bw_J
V(Stucont_J)$betweenness
which.max(Stucont_bw_J)                      #J-58    #87

DF_J<-as_long_data_frame(Stucont_J)
Stucont_J

#==================================================================#
#1. Densidad de red
edge_density(Stucont_J) # 0.007658025
SEC1<-induced_subgraph(Stucont_J, V(Stucont_J)[Cota_1=="SEC1"], impl=c("auto")) #Subgrafiar en cada clase o intervalos de cota
edge_density(SEC1) # 0.01306471

#2. Assortativity ----> indica que tanto se unen a vertices con similares caracteristicas
values_J <- as.numeric(factor(V(Stucont_J)$Cota_1))
assortativity_nominal(Stucont_J, types=values_J)   #0.890189

#2.1. Calculate the observed assortativity (Calcular la variedad observada)
observed.assortativity_J <- assortativity_nominal(Stucont_J, types=values_J)
results_J <- vector('list', 1000)
for(i in 1:1000){results_J[[i]] <- assortativity_nominal(Stucont_J, sample(values_J))}  "sample para crear valores aleatorios"

#2.2.  Trace la distribucion de los valores de surtido y agregue una linea vertical roja en el valor original observado
hist(unlist(results_J), xlim = c(0,1))
abline(v = observed.assortativity_J,col = "red", lty = 3, lwd=2)

#==================================================================#

#1. Trazar una red con el grado de centralidad

set.seed(1001)
library(RColorBrewer) # Esta es la biblioteca de colores.
pal<-brewer.pal(length(unique(V(Stucont_J)$Cota_1)), "Set3") # Color de vértice asignado por cada número de clase
plot(Stucont_J,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont_J, "Cota_1")))],
     vertex.size = sqrt(Stucont_deg_J)/3, edge.width=sqrt(E(Stucont_J)$weight/800),
     edge.arrow.width=0.5,
     edge.arrow.size=0.06,
     edge.curved = T,
     layout = layout.fruchterman.reingold)

#1. Trazar una red con la centralidad del vector propio

set.seed(1001)
plot(Stucont_J,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont_J, "Cota_1")))],
     vertex.size = sqrt(Stucont_eig_J)*10, edge.width=sqrt(E(Stucont_J)$weight/800),
     edge.arrow.width=0.5,
     edge.arrow.size=0.06,
     edge.curved = T,
     layout = layout.fruchterman.reingold)

#2. Trazado de una red con la centralidad de intermediacion

set.seed(1001)
plot(Stucont_J,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(Stucont_J, "Cota_1")))],
     vertex.size = sqrt(Stucont_bw_J)/3, edge.width=sqrt(E(Stucont_J)$weight/800),
     edge.arrow.width=0.5,
     edge.arrow.size=0.06,
     edge.curved = T,
     layout = layout.fruchterman.reingold)

# Camino más corto entre 1 y 5
sp1_1 <- shortest.paths(Stucont_J, v = "R-522", to = "J-57")
sp1_1[]                                         # Distancia 
gsp1_1 <- get.shortest.paths(Stucont_J, from = "R-522", to = "J-57")
V(Stucont_J)[gsp1_1$vpath[[1]]]                           # Secuencia de vértices 

sp1_2 <- shortest.paths(Stucont_J, v = "R-522", to = "J-81")
sp1_2[]                                         # Distancia 
gsp1_2 <- get.shortest.paths(Stucont_J, from = "R-522", to = "J-81")
V(Stucont_J)[gsp1_2$vpath[[1]]]   



sp2 <- shortest.paths(Stucont_J, v = "RAP-03-2", to = "J-8-2")
sp2[]                                         # Distancia 
gsp2 <- get.shortest.paths(Stucont_J, from = "RAP-03-2", to = "J-8-2")
V(Stucont_J)[gsp2$vpath[[1]]]                           # Secuencia de vértices 

sp3 <- shortest.paths(Stucont_J, v = "RAP-01-3", to = "J-28-3")
sp3[]                                         # Distancia 
gsp3 <- get.shortest.paths(Stucont_J, from = "RAP-01-3", to = "J-28-3")
V(Stucont_J)[gsp3$vpath[[1]]]                           # Secuencia de vértices 

#########################################################################
#####################     Clustering Jerarquico     #####################
#########################################################################

DJ_meta=nodesdat
DJ_meta

df <- DJ_meta
df
#normalizar las puntuaciones
df <- scale(df)
head(df)

#calcular la matriz de distacias
m.distancia <- get_dist(df, method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#estimar el número de clústers
#Elbow, silhouette o gap_stat  method
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_stat")

#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

#calculamos los dos clústers
k2 <- kmeans(df, centers = 3, nstart = 25)
k2
str(k2)

#plotear los cluster
fviz_cluster(k2, data = df)
fviz_cluster(k2, data = df, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k2, data = df, ellipse.type = "norm")
fviz_cluster(k2, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

res2 <- hcut(df, k = 3, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black","blue","coral","chocolate"))

res4 <- hcut(df, k = 4, stand = TRUE)
fviz_dend(res4, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black"))

#pasar los cluster a mi df inicial para trabajar con ellos

DJ_meta %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

df <- DJ_meta
df
df$clus<-as.factor(k2$cluster)
df

head(USArrests)
