# Carregando bibliotecas
install.packages("arm")
install.packages("psych")
install.packages("sandwich")
install.packages("lmtest")

library(arm)
library(psych)
library(sandwich)
library(lmtest)

# Ajustando diret�rio de arquivos

setwd("C:/Users/Matheus_Medeiros/Desktop/Matheus1/Mestrado/Davi Moreira/Workshop R/Trabalho Final/") #Ajustando diretório

getwd("josematheus-medeiros-bd-tf-ad-ufpe-2018.txt")

# Carregando base de dados

db <- read.csv("josematheus-medeiros-bd-tf-ad-ufpe-2018.txt", sep = ";", fileEncoding = "latin1") #Pegar base de dados

# Verificando carregamento da base dados

dim(db) #Ver dimens�es da base de dados 

head(db)

summary(db) #Resumo da base de dados

# Seccionando dados

as.character(db$ano) #Ler a vari�vel ano como caracter

db$id <- paste(as.character(db$ano),as.character(db$cod_mun)) # Removendo dados duplicados
db <- db[duplicated(db$id) == F,]

df <- db[db$ano > 2002 & db$ano <= 2006,]  #Delimitando a vari�vel Ano a partir de 2003, que corresponde ao primeiro mandato de Lula

df <- df[df$populacao >= 100000,] #Delimitando a vari�vel popula��o para mais de 100000 habitantes, seguindo a indica��o de Cerqueira et al (2017)

df <- na.omit(df) #Eliminando observa��es com dados omissos

# Estat�sticas descritivas das vari�veis

summary(df)

# Gr�ficos descritivos de vari�veis

hist(df$hom_pc, main = 'Histograma Taxa de Homic�dios')
hist(df$gini, main = 'Histograma �ndice de Gini')
hist(df$educacao, main = 'Histograma Anos de Estudos (idade > 18)')
hist(df$renda_media, main = 'Histograma Renda M�dia')
hist(df$pobreza, main = 'Histograma Prop. de Popula��o Linha da Pobreza')
hist(df$pib_pc, main = 'Histograma PIB per capta')

# Ajustando Vari�veis que n�o apresentaram distribui��o normal

df$hom_pc <- log(df$hom_pc)
df$pobreza <- log(df$pobreza)
df$pib_pc <- log(df$pib_pc)

## Verificando associa��o entre VIs e VD

plot(df$hom_pc ~ df$gini)
abline(lm(df$hom_pc ~ df$gini),col = 'red')
cor(df$hom_pc,df$gini)

plot(df$hom_pc ~ df$educacao)
abline(lm(df$hom_pc ~ df$educacao),col = 'red')
cor(df$hom_pc,df$educacao)

plot(df$hom_pc ~ df$renda_media)
abline(lm(df$hom_pc ~ df$renda_media),col = 'red')
cor(df$hom_pc,df$renda_media)

plot(df$hom_pc ~ df$pobreza)
abline(lm(df$hom_pc ~ df$pobreza),col = 'red')
cor(df$hom_pc,df$pobreza)

plot(df$hom_pc ~ df$pib_pc)
abline(lm(df$hom_pc ~ df$pib_pc),col = 'red')
cor(df$hom_pc,df$pib_pc)


reg1 <- lm(hom_pc ~ gini + factor(ano), data = dbnovo2) 

summary(reg1)

reg2 <- lm(hom_pc ~ educacao + factor(ano), data = dbnovo2) 

summary(reg2)

reg3 <- lm(hom_pc ~ pib_pc + factor(ano), data = dbnovo2) 

summary(reg3)

reg4 <- lm(hom_pc ~ pobreza + factor(ano), data = dbnovo2) 

summary(reg4)

reg5 <- lm(hom_pc ~ renda_media + factor(ano), data = dbnovo2) 

summary(reg5)

reg <- lm(hom_pc ~ gini + educacao + renda_media + pobreza + pib_pc + factor(ano), data = dbnovo2) #Regress�o Linear, tendo como VD homic�dios per capita
