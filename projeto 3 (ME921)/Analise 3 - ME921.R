# Packages
require(mixer)
require(network)
require(mclust)
require(RColorBrewer)
require(ggplot2)

set.seed(28)

# Importação
db <- read.csv('dados/Pilotos F1 - 2024.csv', row.names=1)
labels <- row.names(db)  # Nomes

# Colocando os dados em um formato mais interessante
db_rede <- as.matrix(db[-(1:5)])
colnames(db_rede) <- labels

# db_network <- network(db_rede)
db_network <- network(db_rede, vertex.attr=db[1:5],
                      vertex.attrnames=colnames(db)[1:5])


# Gráficos
par(oma=c(2.2,1,1,1))
heatmap(db_rede, Rowv=NA, Colv=NA, scale='none')

par(oma=c(0,0,0,0), mar=c(1,1,1,1))
layout <- network.layout.fruchtermanreingold(db_network, layout.par=NULL)
plot(db_network, label=labels, mode='fruchtermanreingold', coord=layout,
     edge.col='gray', label.cex=0.75)

# Ajustando modelo
fit <- mixer(db_rede, qmin=1, qmax=8, method='variational')
mod <- getModel(fit)
mod$q  ## 4: quantidade de clusters

# Gráfico dos clusters
z <- t(mod$Taus)
color <- (2:5)[mclust::map(z)]

par(oma=c(0,0,0,0), mar=c(2,2,2,2))
plot(db_network, label=labels, coord=layout, vertex.col=color, xlim=c(2,1),
     edge.col='gray', label.cex=0.75)
legend('right', col=2:5, pch=20, legend=paste('Cluster', 1:4), bty='n',
       cex=0.75, y.intersp=2, inset=c(0.1,0.2))


# Estimativa de Theta e tau
round(mod$alphas, 2)  # \hat\tau
round(mod$Pis, 2)  # \hat\Theta


# Tabelas
tab <- data.frame('c'=as.factor(mclust::map(z)), 
                  'titulos'=get.vertex.attribute(db_network , 'Titulos'),
                  'corridas'=get.vertex.attribute(db_network , 'Corridas'),
                  'idade'=get.vertex.attribute(db_network , 'Idade'),
                  'vitorias'=get.vertex.attribute(db_network , 'Vitorias'))

colMeans((tab[tab$c==1,])[-1])
# titulos   corridas      idade   vitorias 
# 0.0000000 62.8333333 24.5000000  0.3333333 
colMeans((tab[tab$c==2,])[-1])
# titulos corridas    idade vitorias 
# 0.6    179.0     27.2     14.0 
colMeans((tab[tab$c==3,])[-1])
# titulos   corridas      idade   vitorias 
# 0.000000 165.857143  29.428571   2.428571 
colMeans((tab[tab$c==4,])[-1])
# titulos corridas    idade vitorias 
# 4.5    356.0     40.5     67.5 


titulos <- get.vertex.attribute(db_network , 'Titulos')
ti <- table(map(z), titulos)

corridas <- get.vertex.attribute(db_network , 'Corridas')
co <- apply(table(map(z), corridas), 1, mean)

par(oma=c(1,1,1,1), mar=c(2,4,4,2))
boxplot(idade ~ c, col=2:5, pch=19, data=tab)
legend('right', col=2:5, pch=20, legend=paste('Cluster', 1:4), bty='n',
       cex=0.75, y.intersp=2)

# inset=c(-0.03,0)



