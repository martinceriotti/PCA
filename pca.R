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

dfEpf <- read.table(paste0(masterPath,"epf.txt"),
                    sep="\t",
                    dec=".",
                    fileEncoding = "utf8",
                    header = T)

knitr::kable(head(dfEpf))
provincias <- dfEpf$provincia
dfEpf<-dfEpf[,-ncol(dfEpf)] #quitr la columna de provincias para quedarme solo con columnas numéricas 

res<-pastecs::stat.desc(dfEpf) %>% as.matrix %>% as.data.frame %>% round(2)

res = format(res, scientific = FALSE)

knitr::kable(data.frame(res),digits = 2)

pairs.panels(dfEpf, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)

dfInd<-stack(dfEpf)
names(dfInd)<-c("valor","variable")
ggplot(dfInd, aes(x = variable, y = valor)) +
  geom_boxplot()+  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#PCA casero
X <- as.matrix(dfEpf) #remover la última columna de los nombres de provincias
X <- log(X) #trabajamos con logaritomo dada la asimetría de algunas variables

S <- cov(X) #matriz de varianzas y covarianzas
R <- cor(X) # matriz de correlaciones

# Valores propios
eig <- eigen(S)
eig$values

A <- eig$vectors
colnames(A) <- paste0("PC",1:dim(A)[1])
knitr::kable(A)



#Aplicamos la transformación ortogonal de los datos

Z <- as.matrix(X)%*%eigen(S)$vectors
colnames(Z) <- paste0("PC",1:dim(Z)[2])
Z <- apply(Z, 2, function(x) x - mean(x))
knitr::kable(head(Z))


#La varianza de los componentes principales es

knitr::kable(var(Z))


#La matriz A diagonaliza la matriz S

D <- t(A)%*%S%*%A
knitr::kable(D)

#A traspuesta por A da la identidad
knitr::kable(t(A)%*%A)

