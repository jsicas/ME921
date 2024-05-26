#-------- Packages --------#
require(tidyverse)
require(GGally)
require(mclust)
require(covRobust)

#-------- Importação dos dados --------#
db <- read.csv('dados/dataset-mort-both-sexes-in-2022-all-cancers.csv', nrows=185,
               col.names=c('alpha_code', 'cancer_code', 'ISO', 'country', 'label',
                           'sex', 'number', 'ASR', 'crude_rate', 'cumulative_risk')) %>% 
  select(label, number, ASR, crude_rate, cumulative_risk)

#-------- Transformações --------#
db$number <- log(db$number)
# db[-1] <- scale(db[-1])


#------- Análises --------#
ggpairs(db[-1])

# BIC
bic1 <- mclustBIC(db[-1])
plot(bic1)

# Ajustando modelos
mod1 <- Mclust(db[-1], 4, modelNames='VVE')  # Melhor modelo, critério BIC
mod2 <- Mclust(db[-1], 2, modelNames='VVE')  # Segundo melhor modelo, critério BIC

# Plot modelos
plot(mod1, what='classification')
plot(mod2, what='classification')

# Plot incerteza
uncerPlot(mod1$z)
uncerPlot(mod2$z)


# Classificando outliers
nnve.out <- cov.nnve(db[-1])
plot(mclustBIC(db[-1], initialization=list(noise=(nnve.out$classification == 0))))
mod1 <- Mclust(db[-1], 2, modelNames='VVE', initialization=list(noise=(nnve.out$classification == 0)))
mod2 <- Mclust(db[-1], 2, modelNames='VVV', initialization=list(noise=(nnve.out$classification == 0)))
plot(mod1, what='classification')
plot(mod2, what='classification')

sum(ifelse(nnve.out$classification==0, 1, 0))/length(nnve.out$classification)

ggpairs(db)




############
## Testes ##
############

par(new=T)
densityMclust(db$cumulative_risk, axes=F, xlim=c(-4,4), ylim=c(0,0.6))
densityMclust(db$number, axes=F, xlim=c(-4,4), ylim=c(0,0.6))
densityMclust(db[-1], G=10, xlim=c(-4,4), ylim=c(0,0.6), initialization=list(noise=(nnve.out$classification == 0)))



a <- densityMclust(db$crude_rate)
plot(a$data, type='l')





