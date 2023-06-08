options(scipen=999)
rm(list = ls())
#rm()
#-------------------- Análisis de Trabajo SEM ------------------------------- #
library(sjmisc)
library(tidyverse)
library(haven)
library(ggplot2)
library(Hmisc) 
library(corrplot)


## Preparación de datos  -----

elri <- read_dta("bbdd/BBDD ELRI HOMOLOGADA 3.0 LONG INNOMINADA.dta") %>% 
  filter(ano == 2016, m >= 3) %>% #Muestra Mapuche y No Mapuche
  dplyr::select(starts_with(c("m","d10", "a6")) )%>%  dplyr::mutate_at(c('d10_2', 'd10_3',
                                                    'd10_4', 'd10_5',
                                                    'd10_6','d10_7',
                                                    'd10_8','d10_9'), as.numeric)

## Eliminamos datos perdidos. 

elri[elri== 9999 | elri== 8888] <- NA #recodificar No sabe y No responde en NA
sum(is.na(elri)) #indica cantidad de NA



## ELRI Invertir el ítem

elri$d10_4 <- 6 - elri$d10_4
elri$d10_5 <- 6 - elri$d10_5
elri$d10_7 <- 6 - elri$d10_7
elri$d10_9 <- 6 - elri$d10_9


elri$prom_dominacion <- (elri$d10_2 + elri$d10_3 + elri$d10_4 + elri$d10_5)/4 

elri$prom_desigualdad <- (elri$d10_6 + elri$d10_7 + elri$d10_8 + elri$d10_9)/4 

#elri %>% frq (m)
descrip <- elri %>% group_by(m) %>% dplyr::select(!c(d10_1)) %>% 
  summarise(across(starts_with("d10"), list(media = ~mean(.x, na.rm = TRUE), D.E= ~sd(.x, na.rm = TRUE)))) 

descrip_total <- elri %>% group_by(m) %>% 
  summarise(across(starts_with("prom"), list(media = ~mean(.x, na.rm = TRUE)))); descrip_total


elri_graph <- descrip_total %>%
  pivot_longer(cols = -"m", names_to="Pregunta", values_to="Puntaje") %>% drop_na(); elri_graph


## Crear BBDD

data <- elri %>%
  dplyr::select(starts_with(c("m","d10"))) %>% dplyr::select(!c(d10_1))%>%  dplyr::select(3:11)  %>% drop_na()


data_mapuche <- elri %>% filter(m == 3) %>% 
  dplyr::select(starts_with(c("m","d10"))) %>% dplyr::select(!c(d10_1))%>%  dplyr::select(3:11)  %>% drop_na()


data_nomapuche <- elri %>% filter(m == 4) %>% 
  dplyr:: select(starts_with(c("m","d10"))) %>% dplyr::select(!c(d10_1))%>%  dplyr::select(3:11)  %>% drop_na()


data$m <- NULL #Elimino muestra
data_mapuche$m <- NULL #Eliminamos muestra
data_nomapuche$m <- NULL #Eliminamos muestra


## Análisis exploratorio  -----

pacman::p_load(stargazer, # Reporte a latex
               sjPlot, sjmisc, # reporte y gráficos
               corrplot, # grafico correlaciones
               xtable, # Reporte a latex
               Hmisc, # varias funciones
               psych, # fa y principal factors
               psy, # scree plot function
               nFactors, # parallel
               GPArotation, # rotación
               ltm)

corMat  <- cor(data)  # estimar matriz pearson
options(digits=2)
corMat # muestra matriz



M=cor(data_mapuche)

corrplot(M, method = 'number',
         bg = "White", tl.srt = 45, 
         #title = "Correlación de SDO en Mapuches",
         addCoef.col = "Black",
         type = "lower") # colorful number

title("Correlación de SDO en Mapuches")


M=cor(data_nomapuche)
corrplot(M, method = 'number',
         bg = "White", tl.srt = 45, 
         #title = "Correlación de SDO en Mapuches",
         addCoef.col = "Black",
         type = "lower") # colorful number

title("Correlación de SDO en No Mapuches")

## Total

ind1=data[, c(1, 2, 3,4)] # seleccionar columnas correspondientes
summary(ind1)

ind2=data[, c(5,6,7,8)] # seleccionar columnas correspondientes
summary(ind2)

cronbach.alpha(ind1[, c(1, 2, 3, 4)]) # ltm package

cronbach.alpha(ind2[, c(1, 2, 3, 4)]) # ltm package

## Mapuche

ind1=data_mapuche[, c(1, 2, 3,4)] # seleccionar columnas correspondientes
summary(ind1)

ind2=data_mapuche[, c(5,6,7,8)] # seleccionar columnas correspondientes
summary(ind2)

cronbach.alpha(ind1[, c(1, 2, 3, 4)]) # ltm package

cronbach.alpha(ind2[, c(1, 2, 3, 4)]) # ltm package

# No Mapuche

ind1=data_nomapuche[, c(1, 2, 3,4)] # seleccionar columnas correspondientes
summary(ind1)

ind2=data_nomapuche[, c(5,6,7,8)] # seleccionar columnas correspondientes
summary(ind2)

cronbach.alpha(ind1[, c(1, 2, 3, 4)]) # ltm package

cronbach.alpha(ind2[, c(1, 2, 3, 4)]) # ltm package

### Mismo alpha


### ---- Seleccion de factores

scree.plot(data)  
scree.plot(data_mapuche)  
scree.plot(data_nomapuche)  

### Ejes principales -----

#### -- General

fac_pa1 <- fa(r = data, nfactors = 2, fm= "pa")
#summary(fac_pa)
fac_pa1

#### -- Mapuche

fac_pa2 <- fa(r = data_mapuche, nfactors = 2, fm= "pa")
#summary(fac_pa)
fac_pa2

#### -- No Mapuche

fac_pa3 <- fa(r = data_nomapuche, nfactors = 2, fm= "pa")
#summary(fac_pa)
fac_pa3



### Maximun Likelihood -----

fac_ml1 <- fa(r = data, nfactors = 2, fm= "ml")
fac_ml1


#### -- Mapuche

fac_ml2 <- fa(r = data_mapuche, nfactors = 2, fm= "ml")
#summary(fac_pa)
fac_ml2

#### -- No Mapuche

fac_ml3 <- fa(r = data_nomapuche, nfactors = 2, fm= "ml")
#summary(fac_pa)
fac_ml3

factor.plot(fac_ml1, labels=rownames(fac_ml1$loadings), title = "Análisis Factorial Exploratorio en ambas muestras")
factor.plot(fac_ml2, labels=rownames(fac_ml2$loadings), title = "Análisis Factorial Exploratorio Muestra Mapuche")
factor.plot(fac_ml3, labels=rownames(fac_ml3$loadings), title = "Análisis Factorial Exploratorio Muestra No Mapuche")

## No está bien ajusta. 
## La teoría de que hay dos factores no cuajó . 

### Puntajes ---------

fac_ml1 <- fa(r = data, nfactors = 2, fm= "ml", scores="regression")
data_punt=data
data_punt <- cbind(data_punt, fac_ml1$scores)
head(data_punt)

fac_ml2 <- fa(r = data_mapuche, nfactors = 2, fm= "ml", scores="regression")
data_punt_mapuche=data_mapuche
data_punt_mapuche <- cbind(data_punt_mapuche, fac_ml2$scores)
head(data_punt_mapuche)


fac_ml3 <- fa(r = data_nomapuche, nfactors = 2, fm= "ml", scores="regression")
data_punt_nomapuche=data_nomapuche
data_punt_nomapuche <- cbind(data_punt_nomapuche, fac_ml3$scores)
head(data_punt_nomapuche)


### Rotación --------
#### Varimax (ortogonal) ----
fac_ml_var <- fa(r = data, nfactors = 2, fm= "ml", rotate="varimax") # ortogonal
fac_ml_var 

#### Promax (oblicua) ------

fac_ml_pro <- fa(r = data, nfactors = 2, fm= "ml", rotate="promax")
fac_ml_pro


library(GPArotation)
library(sjmisc)


## Análisis Factorial confirmatorio  ------

pacman::p_load(lavaan, ltm, semPlot) # pacman para instalar / cargar automáticamente


cfa_1 <- '
# latent variables
Dominacion=~ d10_2 + d10_3 + d10_4 + d10_5
Oposicion =~ d10_6 + d10_7 + d10_8 + d10_9
'



cfa_1 <- '
# latent variables
Dominacion=~ d10_2 + d10_3 + d10_4 + d10_5
Oposicion =~ d10_6 + d10_7 + d10_8 + d10_9
'

fit_1 <- cfa(cfa_1,data=data)

semPaths(fit_1, 
        #"std",
        #"est", 
        title = TRUE, fixedStyle = c(1, 1), 
         curvePivot = FALSE,style = "lisrel",
         theme = "colorblind",
         rotation = 2,
         legend.cex = .5,
         layout = "tree2")


title("Diagrama Factorial")



covpov=cov(data) # S, matriz de covarianzas de la muestra
lowerMat(covpov, digits=3)

fitted(fit_1) # covarianzas implicadas en el modelo (Sigma)


fitMeasures(fit_1, c("chisq", "df", "pvalue", "cfi", "rmsea"))

summary(fit_1, fit.measures = TRUE)



fit_2 <- cfa(cfa_1,data=data_mapuche)

semPaths(fit_2, 
         "std",
         "est", 
         title = TRUE, fixedStyle = c(1, 1), 
         curvePivot = FALSE,style = "lisrel",
         theme = "colorblind",
         rotation = 2,
         legend.cex = .5,
         layout = "tree2")

title("Sem Paths de Mapuches")

summary(fit_2)

fit_3 <- cfa(cfa_1,data=data_nomapuche)

semPaths(fit_3, 
         "std",
         "est", 
         title = TRUE, fixedStyle = c(1, 1), 
         curvePivot = FALSE,style = "lisrel",
         theme = "colorblind",
         rotation = 2,
         legend.cex = .5,
         layout = "tree2")

title("Sem Paths de No Mapuches")




## No entendi

sqrt(1.184)  # sd interna, raiz de la varianza estimada
1.184^2

sd(data$d10_2) # sd indicador


