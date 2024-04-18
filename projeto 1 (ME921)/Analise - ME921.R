##############################
## Packages e configurações ##
##############################

set.seed(1234)
require(dplyr)
require(tidyr)
require(magrittr)
require(geobr)
require(ggplot2)
require(GGally)
require(stringr)
require(patchwork)


##########################
## Importação dos Dados ##
##########################

# carregando municípios do Ceará
all_muni <- read_municipality(
  code_muni='CE',
  year=2020,
  showProgress=F,
  simplified = F
)

all_muni$name_muni <- stringi::stri_trans_general(all_muni$name_muni, # Retirando acentuação
                                                  'Latin-ASCII')

# mortalidade (por ocorrência)
mortalidade <- read.csv2('dados/Motalidade_CE_2010.csv', skip=4, header=F,
                         col.names=c('municipio', 'obitos'), nrows=184) %>%
  separate(sep=6, 'municipio', convert=T, into=c('code_muni_sus', 'name_muni'))

## arrumando name_muni
mortalidade$name_muni <- str_to_title(mortalidade$name_muni) %>% str_trim() %>% 
  stringi::stri_trans_general('Latin-ASCII')  # Retirando acentuação

## corrigindo nome de município de Itapage para Itapaje (posição 87)
mortalidade$name_muni[87] <- 'Itapaje'

# população residente
populacao <- read.csv2('dados/PopulacaoResidente_CE_2010.csv', skip=4, header=F,
                       col.names=c('municipio', 'residentes'), nrows=184) %>% 
  separate(sep=6, 'municipio', convert=T, into=c('code_muni_sus', 'name_muni')) %>%
  select(-'name_muni')

# renda média domiciliar per capita
renda <- read.csv2('dados/RendaMediaDomiciliarPerCapita_CE_2010.csv', skip=4,
                   header=F, col.names=c('municipio', 'renda_media_dom'),
                   nrows=184) %>% 
  separate(sep=6, 'municipio', convert=T, into=c('code_muni_sus', 'name_muni')) %>%
  select(-'name_muni')

# taxa de analfabetismo
analfabetismo <- read.csv2('dados/TaxaAnalfabetismo_CE_2010.csv', skip=4, header=F,
                           col.names=c('municipio', 'analfabetismo'),
                           nrows=184) %>% 
  separate(sep=6, 'municipio', convert=T, into=c('code_muni_sus', 'name_muni')) %>%
  select(-'name_muni')

# juntando banco de dados
db <- mortalidade %>% 
  left_join(populacao, by='code_muni_sus') %>% 
  left_join(renda, by='code_muni_sus') %>% 
  left_join(analfabetismo, by='code_muni_sus') %>% 
  select(-code_muni_sus)

# apenas o que sera utilizado na análise (e retirando Fortaleza)
db_analise <- db %>%
  filter(name_muni != 'Fortaleza') %>% 
  select(c('obitos', 'residentes', 'renda_media_dom', 'analfabetismo'))


##########################
## Análise Exploratória ##
##########################

# COM Fortaleza
ggpairs(db[-c(1)])
hist(db$obitos)
hist(db$residentes)
hist(db$renda_media_dom)
hist(db$analfabetismo)

cor_fortaleza_vermelho <- ifelse(db$name_muni == 'Fortaleza', 'Fortaleza', 'Demais Cidades')

graf_residentes <- db %>% 
  ggplot(aes(y=residentes, x=1:184, color=cor_fortaleza_vermelho)) +
  geom_point(size=1.7) +
  labs(y='Total de Residentes', x='Cidade') +
  scale_color_manual(name='',
                     breaks=c('Fortaleza', 'Demais Cidades'),
                     values=c('Fortaleza'='red', 'Demais Cidades'='blue')) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(legend.position='none')

graf_renda_media <- db %>%
  ggplot(aes(y=renda_media_dom, x=1:184, color=cor_fortaleza_vermelho)) +
  geom_point(size=1.7) +
  labs(y='Renda média domiciliar per capita (Reais)', x='Cidade') +
  scale_color_manual(name='',
                     breaks=c('Fortaleza', 'Demais Cidades'),
                     values=c('Fortaleza'='red', 'Demais Cidades'='blue')) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 

graf_residentes | graf_renda_media


# SEM Fortaleza
ggpairs(db_analise)
hist(db_analise$obitos)
hist(db_analise$renda_media_dom)
hist(db_analise$analfabetismo)


###################
## Clusterizando ##
###################

# K-means: COM FORTALEZA
E <- NULL
for (i in 1:15){
  clst <- kmeans(db[-1], i)
  E[i] <- clst$betweenss/clst$totss 
}
elbows <- data.frame('num'=1:length(E), 'E'=E)

ggplot(data=elbows, aes(x=num, y=E)) +
  geom_point(col='blue') +
  theme_bw()

## Modelo final (~95.4%)
t_com_fortaleza <- kmeans(db[-1], 2)


# K-means: SEM FORTALEZA
elbow <- NULL
for (i in 1:15){
  clst <- kmeans(db_analise, i)
  elbow[i] <- clst$betweenss/clst$totss 
}

elbows <- data.frame('num'=1:length(E), 'E'=E)

ggplot(data=elbows, aes(x=num, y=E)) +
  geom_point(col='blue') +
  theme_bw()

## Modelo final (~86.8%)
t <- kmeans(db_analise, 3)

#############
## tabelas ##
#############
db_analise$clus_num <- as.factor(t$cluster)

db_analise %>%
  group_by(clus_num) %>% 
  summarise('n'=n(), mean(obitos), mean(residentes), mean(analfabetismo),
            mean(renda_media_dom)) 



###########
## Plots ##
###########

color <- c('#619cff', '#00ba38', '#f8766d') # configurando cor

graf1 <- db_analise %>%
  ggplot(aes(y=obitos, x=residentes, color=clus_num)) +
  geom_point() +
  labs(color='', y='Número de Óbitos', x='População Residente') +
  scale_color_manual(labels=c('Cluster 1', 'Cluster 2', 'Cluster 3'),
                     breaks=c(1,2,3),
                     values=c(color[1],color[2],color[3])) +
  theme_bw() +
  theme(legend.position='none')
  
graf2 <- db_analise %>%
  ggplot(aes(x=analfabetismo, y=obitos, color=clus_num)) +
  geom_point() +
  labs(color='', x='Taxa de Analfabetismo', y='') +
  scale_color_manual(labels=c('Cluster 1', 'Cluster 2', 'Cluster 3'),
                     breaks=c(1,2,3),
                     values=c(color[1],color[2],color[3])) +
  theme_bw() +
  theme(legend.position='none')
  
graf3 <- db_analise %>%
  ggplot(aes(x=renda_media_dom, y=obitos, color=clus_num)) +
  geom_point() +
  labs(color='', x='Renda Média Domiciliar Per Capita', y='') +
  scale_color_manual(labels=c('Cluster 1', 'Cluster 2', 'Cluster 3'),
                     breaks=c(1,2,3),
                     values=c(color[1],color[2],color[3])) +
  theme_bw()

graf1 | graf2 | graf3


graf4 <- db_analise %>% 
  ggplot(aes(x=analfabetismo, y=renda_media_dom, color=clus_num)) +
  geom_point() +
  labs(color='', x='Taxa de Analfabetismo', y='Renda Média Domiciliar Per Capita') +
  scale_color_manual(labels=c('Cluster 1', 'Cluster 2', 'Cluster 3'),
                     breaks=c(1,2,3),
                     values=c(color[1],color[2],color[3])) +
  theme_bw() +
  theme(legend.position='none')

graf5 <- db_analise %>% 
  ggplot(aes(x=residentes, y=renda_media_dom, color=clus_num)) +
  geom_point() +
  labs(color='', x='População Residentes', y='Renda Média Domiciliar Per Capita') +
  scale_color_manual(labels=c('Cluster 1', 'Cluster 2', 'Cluster 3'),
                     breaks=c(1,2,3),
                     values=c(color[1],color[2],color[3])) +
  theme_bw() 

graf4 | graf5


db_analise %>% 
  ggplot(aes(x=analfabetismo, y=residentes, color=clus_num)) +
  geom_point() +
  labs(colot='') +
  scale_color_manual(labels=c('Cluster 1', 'Cluster 2', 'Cluster 3'),
                     breaks=c(1,2,3),
                     values=c(color[1],color[2],color[3])) +
  theme_bw()


# configurando cor
all_muni$cor <- as.factor(c(t$cluster[1:58], 4, t$cluster[59:183])) # colocando Fortaleza

# plot dos clusters de CE
all_muni %>% ggplot() +
  geom_sf(aes(fill=cor), color='#00003F', size=.15) +
  theme_void() +
  labs(fill='Cluster') +
  scale_fill_manual(name='', labels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Fortaleza'),
                    breaks=c(1,2,3,4),
                    values=c(color[1],color[2],color[3],'black')) +
  theme(legend.title = element_text(size=18),
        legend.text = element_text(size=14))
