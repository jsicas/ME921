#-------- Packages e configurações --------#
require(tidyverse)
require(GGally)
require(mclust)
require(covRobust)
require(patchwork)

source('plot_modified.R')  # Importando função extra

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
mod1 <- Mclust(db[-1], 2, modelNames='VVV')  # Melhor modelo, critério BIC
mod2 <- Mclust(db[-1], 2, modelNames='VVE')  # Segundo melhor modelo, critério BIC
mod3 <- Mclust(db[-1], 4, modelNames='VVE')  # Terceiro melhor modelo, critério BIC

# Quantidade de parâmetros
## VVV: Cp(p + 1)/2
## VVE: C + (p + 2C)(p - 1)/2
## onde temos C clusters e p dimensões
p <- ncol(db[-1])
C1 <- 2
C2 <- 2
C3 <- 4
param_mod1 <- C1 * p * (p + 1)/2  # 20 (VVV)
param_mod2 <- C2 + (p + 2*C2)*(p - 1)/2  # 14 (VVE)
param_mod3 <- C3 + (p + 2*C3)*(p - 1)/2  # 22 (VVE)

# Plot modelos
plot(mod1, what='classification')
plot(mod2, what='classification')

# Plot incerteza
uncerPlot(mod1$z, ylab='Incerteza')
uncerPlot(mod2$z, ylab='Incerteza')


#-------- Outliers --------#
nnve.out <- cov.nnve(db[-1])  # Classificando outliers
bic2 <- mclustBIC(db[-1], initialization=list(noise=(nnve.out$classification == 0)))
plot(bic2)

# Ajustando modelos
mod1_noise <- Mclust(db[-1], 3, modelNames='VEV', initialization=list(noise=(nnve.out$classification == 0)))
mod2_noise <- Mclust(db[-1], 2, modelNames='VVV', initialization=list(noise=(nnve.out$classification == 0)))
mod3_noise <- Mclust(db[-1], 2, modelNames='VVE', initialization=list(noise=(nnve.out$classification == 0)))

# Quantidade de parâmetros
## VEV: C + (p - 1) + Cp(p - 1)/2
## VVV: Cp(p + 1)/2
## VVE: C + (p + 2C)(p - 1)/2
## onde temos C clusters e p dimensões
p <- ncol(db[-1])
C1_noise <- 3
C2_noise <- 2
C3_noise <- 2
param_mod1_noise <- C1_noise + (p - 1) + C1_noise*p*(p - 1)/2  # 24 (VEV)
param_mod2_noise <- C2_noise * p*(p + 1)/2  # 20 (VVV)
param_mod3_noise <- C3_noise + (p + 2*C3_noise)*(p - 1)/2  # 14 (VVE)

# Plot modelos
plot(mod1_noise, what='classification')
plot(mod2_noise, what='classification')
plot(mod3_noise, what='classification')

sum(nnve.out$classification == 0)  # qtd. outliers 23
sum(nnve.out$classification == 0)/length(nnve.out$classification)  # % de outliers 12.43%


########################
## Gráficos e Tabelas ##
########################

graf1 <- db %>%
  ggplot(aes(x=exp(number), y=ASR)) +
  geom_point() +
  labs(title='A - Não transformado', x='number') +
  theme_bw()
graf2 <- db %>%
  ggplot(aes(x=exp(number), y=crude_rate)) +
  geom_point() +
  labs(title='C - Não transformado', x='number') +
  theme_bw()
# graf3 <- db %>%
#   ggplot(aes(x=exp(cumulative_risk), y=crude_rate)) +
#   geom_point() +
#   labs(title='C - Não transformado', x='number') +
#   theme_bw()

graf_log1 <- db %>%
  ggplot(aes(x=number, y=ASR)) +
  geom_point() +
  labs(title='B - transformado') +
  theme_bw()
graf_log2 <- db %>%
  ggplot(aes(x=number, y=crude_rate)) +
  geom_point() +
  labs(title='D - transformado') +
  theme_bw()
# graf_log3 <- db %>%
#   ggplot(aes(x=number, y=cumulative_risk)) +
#   geom_point() +
#   labs(title='F - transformado') +
#   theme_bw()

(graf1 | graf_log1)/(graf2 | graf_log2)

plot_modified(mod2, mod3_noise, what='classification', cex=0.8)

# BIC 
par(mfrow=c(1,2))
plot(bic1, legendArgs=list(), xlab='Número de Clusters')
par(new=T)
plot(0, axes=F, type='n', xlab='', ylab='', main='A')

# BIC com Nearest Neighbors
plot(bic2, legendArgs=list(), xlab='Número de Clusters')
par(new=T)
plot(0, axes=F, type='n', xlab='', ylab='', main='B')

# Análise de perfil
## mod2
graf_perfil1 <- db[-1] %>%
  scale() %>% as.data.frame() %>% 
  mutate('cluster'=factor(mod2$classification, labels=c('C1', 'C2'))) %>% 
  pivot_longer(-cluster, names_to='Atributo', values_to='valor') %>% 
  ggplot(aes(x=Atributo, y=valor, fill=cluster)) +
  geom_boxplot() +
  scale_colour_manual(values=c('#1c86ee', '#cd0000')) +
  coord_flip() +
  labs(title='Modelo A - Mclust') +
  theme_bw()

## mod3_noise
graf_perfil2 <- db[-1] %>%
  scale() %>% as.data.frame() %>% 
  mutate('cluster'=factor(mod3_noise$classification, labels=c('Outliers', 'C1', 'C2'))) %>% 
  pivot_longer(-cluster, names_to='Atributo', values_to='valor') %>% 
  ggplot(aes(x=Atributo, y=valor, fill=cluster)) +
  geom_boxplot() +
  scale_fill_manual(values=c('#8f8f8f', '#df536b', '#61d04f')) +
  coord_flip() +
  labs(title='Modelo B - Mclust com nearest neighbor') +
  theme_bw()

graf_perfil1 + graf_perfil2


dbf1 <- cbind(db, 'cluster'=factor(mod2$classification, labels=c('C1', 'C2')))
dbf1 %>%
  group_by(cluster) %>%
  summarise('number'=mean(number), 'ASR'=mean(ASR), 'crude_rate'=mean(crude_rate),
            'cumulative_risk'=mean(cumulative_risk)) %>% xtable()


dbf2 <- cbind(db, 'cluster'=factor(mod3_noise$classification, labels=c('outlier', 'C1', 'C2')))
dbf2 %>%
  group_by(cluster) %>%
  summarise('number'=mean(number), 'ASR'=mean(ASR), 'crude_rate'=mean(crude_rate),
            'cumulative_risk'=mean(cumulative_risk)) %>% xtable()


# incerteza
plot_modified(mod2, mod3_noise, what='uncertainty', cex=0.8)

uncerPlot(mod2$z, ylab='Incerteza', main='Modelo A')
par(new=T)
plot(0, axes=F, type='n', xlab='', ylab='', main='Modelo A')

uncerPlot(mod3_noise$z, ylab='Incerteza', main='Modelo B')
par(new=T)
plot(0, axes=F, type='n', xlab='', ylab='', main='Modelo B')

