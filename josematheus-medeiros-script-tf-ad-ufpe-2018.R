# Carregando bibliotecas
install.packages("arm")
install.packages("psych")
install.packages("sandwich")
install.packages("lmtest")
install.packages("car")

library(car)
library(arm)
library(psych)
library(sandwich)
library(lmtest)

# Ajustando diret�rio de arquivos

setwd("C:/Users/Matheus_Medeiros/Desktop/Matheus1/Mestrado/Davi Moreira/Workshop R/Trabalho Final/") #Ajustando diret�rio

# Carregando base de dados

db <- read.csv("josematheus-medeiros-bd-tf-ad-ufpe-2018.txt", sep = ";", fileEncoding = "latin1") #Pegar base de dados

# Verificando carregamento da base dados

dim(db) # Ver dimens�es da base de dados 

head(db)

summary(db) # Resumo da base de dados

# Seccionando dados

as.character(db$ano) #Ler a vari�vel ano como caracter

db$id <- paste(as.character(db$ano),as.character(db$cod_mun)) # Removendo dados duplicados
db <- db[duplicated(db$id) == F,]

df <- db[db$ano > 2002 & db$ano <= 2006,]  #Delimitando a vari�vel Ano a partir de 2003, que corresponde ao primeiro mandato de Lula

df <- df[df$populacao >= 100000,] # Delimitando a vari�vel popula��o para mais de 100000 habitantes, seguindo a indica��o de Cerqueira et al (2017)

df <- na.omit(df) # Eliminando observa��es com dados omissos

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

reg1 <- lm(hom_pc ~ gini + factor(ano), data = df) 
summary(reg1)

plot(df$hom_pc ~ df$gini)
abline(lm(df$hom_pc ~ df$gini),col = 'red')
cor(df$hom_pc,df$gini)

reg2 <- lm(hom_pc ~ educacao + factor(ano), data = df) 
summary(reg2)

plot(df$hom_pc ~ df$educacao)
abline(lm(df$hom_pc ~ df$educacao),col = 'red')
cor(df$hom_pc,df$educacao)

reg3 <- lm(hom_pc ~ renda_media + factor(ano), data = df) 
summary(reg3)

plot(df$hom_pc ~ df$renda_media)
abline(lm(df$hom_pc ~ df$renda_media),col = 'red')
cor(df$hom_pc,df$renda_media)

reg4 <- lm(hom_pc ~ pobreza + factor(ano), data = df) 
summary(reg4)

plot(df$hom_pc ~ df$pobreza)
abline(lm(df$hom_pc ~ df$pobreza),col = 'red')
cor(df$hom_pc,df$pobreza)

reg5 <- lm(hom_pc ~ pib_pc + factor(ano), data = df) 
summary(reg5)

plot(df$hom_pc ~ df$pib_pc)
abline(lm(df$hom_pc ~ df$pib_pc),col = 'red')
cor(df$hom_pc,df$pib_pc)

# Padronizando Vari�veis
df$hom_pc <- scale(df$hom_pc)
df$gini <- scale(df$gini)
df$educacao <- scale(df$educacao)
df$renda_media <- scale(df$renda_media)
df$pobreza <- scale(df$pobreza)
df$pib_pc <- scale(df$pib_pc)

# Modelo de Regress�o
reg <- lm(hom_pc ~ gini + educacao + renda_media + pobreza + 
            pib_pc + I(pib_pc ^ 2) + factor(ano), data = df)

# Sum�rio do Modelo
summary(reg)

# Gr�fico dos Coeficientes
coefplot(reg, intercept = F)

# Verificando pressupostos do modelo
# Normalidade dos Res�duos
hist(residuals(reg))

# Heterocedasticidade
plot(reg, which = 1)

# Multicolinearidade
vif(reg)

# Utilizando estimador de erros-padr�o de Huber-White para corrigir heterocedasticidade
coeftest(reg,vcov. = vcovHC)
