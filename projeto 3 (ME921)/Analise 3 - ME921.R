# Packages e Configurações
require(jpeg)
require(wavethresh)
require(cluster)
require(tidyverse)

# Importando imagem
image <- readJPEG('dados/img_reduzido/242.jpg') # Imagem
image_df <- tibble(R=(image[,,1]), G=(image[,,2]), B=(image[,,3])) # Imagem data.frame

## plot da imagem
par(mar=c(1,1,1,1))
plot(c(0,1), c(0,1), type='n', axes=FALSE, xlab='', ylab='', main='', mai=c(0,0,0,0))
rasterImage(image, 0,0,1,1, mai=c(0,0,0,0))

# Análise
## K-means
km <- kmeans(tt[, c('R', 'G', 'B')], centers=100)

image_final <- array(c(as.vector(km$centers[km$cluster,'R']),  # R
                       as.vector(km$centers[km$cluster,'G']),  # G
                       as.vector(km$centers[km$cluster,'B'])), # B
                     dim=c(256,384,3))

# Plotando imagem final
plot(c(0,1), c(0,1), type='n', axes=FALSE, xlab='', ylab='', main='')
rasterImage(image_final, 0,0,1,1)

# Gravando imagem final
writeJPEG(im_final, 'imagem_final.jpeg')


#------- Testes -------#
data(teddy)
t <- imwd(teddy, filter.number=10)
twr <- imwr(t)
greycol <- grey((0:255)/255)
myt <- function(x) 20+sqrt(x)
plot(t$w8L1, col=greycol, transform=TRUE, tfunction=myt)
par(new=T)
plot(t$w8L2, col=greycol, transform=TRUE, tfunction=myt)
par(new=T)
plot(t$w8L3, col=greycol, transform=TRUE, tfunction=myt)
par(new=T)
plot(t$w8L4, col=greycol, transform=TRUE, tfunction=myt)



# tt %>% 
#   mutate(kColors=rgb(km$centers[km$cluster,])) %>% 
#   ggplot(aes(x=X, y=Y, col=I(kColors))) +
#   geom_point(show.legend=F) +
#   theme_minimal()

# im.m <- tt %>% mutate(R=as.vector(km$centers[km$cluster,'R']),
#                       G=as.vector(km$centers[km$cluster,'G']),
#                       B=as.vector(km$centers[km$cluster,'B']))





require(opencv)
# Silly example
mona <- ocv_read('https://jeroen.github.io/images/monalisa.jpg')
mona <- ocv_resize(mona, width = 320, height = 477)

# Edge detection
ocv_edges(t)
ocv_markers(t)
a <- ocv_face(mona)

# Rectangular area
ocv_rectangle(mona, x = 400, y = 300, height = 300, width = 350)
ocv_rectangle(mona, x = 0, y = 100, height = 200)
ocv_rectangle(mona, x = 500, y = 0, width = 75)


img <- ocv_resize(mona, width = 320, height = 477)
pts <- list(x = c(184, 172, 146, 114, 90, 76, 92, 163, 258),
            y = c(72, 68, 70, 90, 110, 398, 412, 385, 210))
ocv_polygon(img, pts)
ocv_polygon(img, pts, crop = TRUE)
ocv_polygon(img, pts, convex = TRUE, crop = TRUE)

# Bounding box based on points
ocv_bbox(img, pts)
# Bounding box of non-zero pixel area
area <- ocv_polygon(img, pts, color = 0, crop = FALSE)
area
area <- ocv_bbox(area)
area






eigenface.candidates <- grep('y', dir(), value=TRUE)


i1 <- readJPEG(eigenface.candidates[1])
# Vou roubar a estrutura para não criar objeto novo
AverageFace <- subject01.normal.pgm
AverageFace@grey <- matrix(0, nrow = d[1], ncol = d[2])
n <- length(eigenface.candidates)
for(j in seq_along(eigenface.candidates)){
  AverageFace@grey <- AverageFace@grey + `@`(get(eigenface.candidates[j]), "grey")/n
}








