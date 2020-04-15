#téléchargement du package arules
install.packages("arules")
install.packages("arulesViz")
install.packages("plotly")
install.packages("RColorBrewer")
install.packages("plyr")
install.packages("dplyr")
install.packages("stringi")
install.packages("magrittr")
install.packages("reshape2")
#chargement en memoire du package

library(plotly)
library(arulesViz)
library(arules)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(stringi)
library("magrittr")
library("reshape2")
data(packages="Groceries")
data(Groceries)
unique(inspect(Groceries))
#nombre d'element dans l'ensembre de donnée =43367 la dénsité=0.02609146 
#le yaourt correspond à 13.16% des transactions
summary(Groceries)
#nombre de transaction = 9835
#calcul du pourcentage que contient le yaourt
yaourt=(1372/9835)*100
yaourt
#transaction de 3 à 6
inspect(Groceries[3:6])
#nombre de transaction qui ont seulement sept article
#proportion des dix premiers éléments
summary(Groceries[1:10])
#proporttion des transaction ayant au moins 15% de support
itemFrequencyPlot(Groceries, support= 0.15)
#proportion que contient le premier element
cbind(itemFrequency(Groceries[,1])*100)
#Display the proportions of the top 10 items
cbind(itemFrequency(Groceries[,1:10])*100)
#viualisation of the sparse matrix
image(Groceries)


View(Groceries)
str(Groceries)
colnames(Groceries@itemInfo)<-c("labels","level2","level1")
rownames(Groceries)[1:10]
DataGroceries<-data(Groceries)

summary(Groceries)
summary(Groceries@itemInfo)[1:10]

inspect(Groceries[1:5])#look at the 5 first transaction
itemFrequency(Groceries[, 1:3])#examine the frequency of items
itemFrequencyPlot(Groceries, support = 0.1)#plot the frequency of items
itemFrequencyPlot(Groceries, topN = 20, col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
image(Groceries[1:5])#a visualization of the sparse matrix for the first five transactions


#################################Partie II#######################################

apriori(Groceries)#default settings result in zero rules learned
groceryrules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.10, minlen = 2))
groceryrules#affiche les règles
inspect(groceryrules[1:10])#examine les dix première règle
inspect(groceryrules[1:3]) #AFFICHER les trois première règles
inspect(sort(groceryrules, by = "lift", decreasing = TRUE)[1:10])#sorting grocery rules by lift ten best rules
chocrules<- subset(groceryrules, items %in% "chocolate")
inspect(chocrules)
inspect(groceryrules)
plot (groceryrules, method = "scatterplot", control = list (k = 5),interactive = T)#affiche les regles de façon groupé

#######################################Visualisation##################################




plot(groceryrules[1:19],method="graph",interactive = T)# permet de bien identifier les associations entre différents articles
plotly_arules(groceryrules)#affiche un graphe interactif
plot(groceryrules[1:20],measure=c("support","lift"),shading="confidence",interactive=T)



########################PARTIE TROIX#########################################



#Affiche la liste des transactions
n<-apply(Groceries@data[,1:9835],2,
      function(r) paste(Groceries@itemInfo[r, "labels"], collapse = ","))
n
write(n, file="E:/datascience/groceries.csv" )
write(Groceries@itemInfo$labels, file="E:/datascience/items.csv" )
write(Groceries, file="E:/datascience/groc.csv" )
write(Groceries@data@i, file="E:/datascience/index.csv", sep="," )

new_data <- read.csv("E:/datascience/new-groceries.csv", sep=';')

str(new_data)
new_data$item_id <- as.numeric(new_data$item_id)
new_data$user_id <- as.numeric(new_data$user_id)
new_data$user_id 

affinity<-acast(new_data, user_id~items )
affinity
dim(affinity)
rownames(affinity)=c(paste("u", 1:dim(affinity)[1], sep="")) #generer des noms en u****
#colnames(affinity)=c(paste("article", 1:dim(affinity)[2], sep=""))
affinity.matrix= as(affinity,"realRatingMatrix")
affinity.matrix = normalize(affinity.matrix) ## NORMALISER
getRatingMatrix(affinity.matrix)
Rec.model=Recommender(affinity.matrix, method = "UBCF")
user3 = predict(Rec.model, affinity.matrix["u3",], n=5) # top5 articles recommandé pour l'user1(u1)
as(user3, "list")

##################Génère les recommandation pour chaque user############
user1.affinity = predict(Rec.model, affinity.matrix["u1",], type="ratings" ) #rating de user1(u1) pour TOUS les articles meme ceux qu'il n'a pas note
as(user1.affinity,"list")





