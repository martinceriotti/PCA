# Variables cualitativas:

# AX Presencia de axilas: Si – No
# CEstrias Color de las estrías: amarilla, rojas, purpuras, no (no tiene estrías)
# CTallo Color del tallo: rojo, púrpura, verde
# PoRAMA Presencia de ramas Si – No
# CFA Color de panoja a fin de antesis: Blanca – Púrpura – Gris
# Cco Color de la panoja a la cosecha: amarilla, blanca, púrpura, marrón
# TP Tipo de panoja: Diferenciada y terminal – No diferenciada
# FP Forma de panoja: Glomerulada – Amarantiforme
# 
# Origen de los individuos
# AL Altiplano
# VA Valles de altura
# VH Valles húmedos
# VS Valles secos

# Cuantitativas
# DIAMTAL Diámetro del tallo (mm)
# ALTPL Altura de planta (cm)
# LONGHO Longitud de la hoja (mm)
# ANCHOHO Ancho de la hoja (mm)
# LONGPEC Longitud del peciolo (mm)
# LONGPAN Longitud de la panoja (cm)
# LONGGLO Longitud del glomérulo (mm)

# DSBF Días desde siembra a botón floral (días)
# DSFL Días desde siembra a floración (días)
# DSMF Días desde siembra a madurez fisiológica (días)

knitr::opts_chunk$set(echo = TRUE)

masterPath <- "./PCA datos/"

listofpackages <- c( "psych",     "ggplot2",    "knitr",
                     "pastecs",   "FactoMineR", "grid",
                     "gridExtra", "ggfortify",  "factoextra",
                     "corrplot",  "dplyr")
newPackages <- listofpackages[ !(listofpackages %in% installed.packages()[, "Package"])]
if(length(newPackages)) install.packages(newPackages)
for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}

datos <- read.table(paste0(masterPath,"quinoa.txt"),
                    sep="\t",
                    dec=",",
                    fileEncoding = "utf8",
                    header = T)

datos2 <- read.table(paste0(masterPath,"quinoa.txt"),
                    sep="\t",
                    dec=",",
                    fileEncoding = "utf8",
                    header = T)

datos2 <- mutate(datos2, zona = substr(poblaciones,1,2))
knitr::kable(head(datos))
str(datos)

datos <- select(datos, DIAMTAL,   
                ALTPL ,    
                LONGHO,    
                ANCHOHO,   
                LONGPEC ,  
                LONGPAN ,  
                LONGGLO ,  
                DSBF,      
                DSFL,      
                DSMF      )

# Descripción de las variables
res<-pastecs::stat.desc(datos) %>% as.matrix %>% as.data.frame %>% round(2)

res = format(res, scientific = FALSE)

knitr::kable(data.frame(res),digits = 2)

pairs.panels(datos, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)
# Distribuiciones poco campanulares.
# Altas correlaciones para algunas de las variables.

# BoxPlot
dfInd<-stack(datos)
names(dfInd)<-c("valor","variable")
ggplot(dfInd, aes(x = variable, y = valor)) +
  geom_boxplot()+  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Se observan distribuciones muy distintas.

#PRCOMP
mediPca <- prcomp(as.matrix(datos),center = T, scale. = T)
summary(mediPca)
# Tenemos 2 Componentes con desvío mayor a 1. 
# Iguamente nos quedamos con 2 que explican un 81% de la variabilidad

# Importance of components:
                       #   PC1    PC2     PC3    PC4     PC5     PC6     PC7     PC8     PC9    PC10
# Standard deviation     2.6094 1.1498 0.86299 0.6648 0.52099 0.47357 0.30833 0.22724 0.15943 0.11957
# Proportion of Variance 0.6809 0.1322 0.07448 0.0442 0.02714 0.02243 0.00951 0.00516 0.00254 0.00143
# Cumulative Proportion  0.6809 0.8131 0.88759 0.9318 0.95893 0.98136 0.99086 0.99603 0.99857 1.00000

plot(mediPca)

#Matriz de Cargas
#Si todos tienen el mismo signo, son de tamaño. Scores naturales. Las otras se llaman de Forma.

knitr::kable(as.data.frame(unclass(mediPca$rotation)))

#Datos transformados (primeros registros)

knitr::kable((mediPca$x))

# biplot 

autoplot(mediPca, 
         data = datos2, 
         colour = 'zona', 
         loadings = TRUE, 
         loadings.label = TRUE, 
         loadings.label.size = 4,
         size = 4)

#================================= FactoMineR =========================================

mundoPca <- FactoMineR::PCA(X = datos, scale.unit = T,ncp = ncol(datos), graph = T)

var <- get_pca_var(mundoPca)
# Coordinates (vectores propios)
knitr::kable(var$coord)
knitr::kable(head(var$cos2))
knitr::kable(var$contrib)
fviz_contrib(mundoPca, choice = "ind", axes = 1:2)
