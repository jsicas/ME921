##############################
## Packages e configurações ##
##############################

set.seed(1234)
# require(readr)
require(dplyr)
# require(tidyr)
require(magrittr)
require(geobr)
require(ggplot2)
require(GGally)
require(stringr)


##########################
## Importação dos Dados ##
##########################

# carregando municípios do Ceará
all_muni <- read_municipality(
  code_muni='CE',
  year=2020,
  showProgress=F
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

# taxa de mortalidade
db$mortalidade <- db$obitos/db$residentes * 1000

# apenas o que sera utilizado na análise
db_analise <- db[, c('renda_media_dom', 'analfabetismo', 'mortalidade')]


##########################
## Análise Exploratória ##
##########################

ggpairs(db_analise)
hist(db$obitos)
hist(db$renda_media_dom)
hist(db$analfabetismo)


###################
## Clusterizando ##
###################

# Aplicando K-means com critério de elbow
E <- NULL
for (i in 1:15){
  clst <- kmeans(db_analise, i)
  E[i] <- clst$betweenss/clst$totss 
}
elbows <- data.frame('num'=1:length(E), 'E'=E)

# Modelo final (~88.2%)
t <- kmeans(db_analise, 4)

# configurando cor
color <- c('#1F78B4', 'turquoise', '#B2DF8A', '#33A02C')
# color <- c('#5F6F52', '#F9801F', '#C4461F', '#783D1F')
cor <- ifelse(t$cluster==1, color[1], 0)
cor <- ifelse(t$cluster==2, color[2], cor)
cor <- ifelse(t$cluster==3, color[3], cor)
cor <- ifelse(t$cluster==4, color[4], cor)

db$cor <- cor

all_muni %<>% left_join(db, by='name_muni')


###########
## Plots ##
###########

# pelo gráfico foi selecionado 4 como a quantidade de clusters (~88.2%)
ggplot(data=elbows, aes(x=num, y=E)) +
  geom_point(col='blue')

# plot dos clusters de CE
ggplot() +
  geom_sf(data=all_muni, fill=cor, color='#00003F', size=.15, show.legend=FALSE) +
  theme_void()





plot(silhouette(pam(db[-c(1,2)], 3)))


