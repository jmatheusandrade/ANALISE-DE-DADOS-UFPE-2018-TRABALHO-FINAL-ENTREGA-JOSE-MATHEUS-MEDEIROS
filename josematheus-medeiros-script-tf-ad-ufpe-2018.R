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

# Ajustando diretório de arquivos

setwd("C:/Users/Matheus_Medeiros/Desktop/Matheus1/Mestrado/Davi Moreira/Workshop R/Trabalho Final/") #Ajustando diretório

# Carregando base de dados

db <- read.csv("josematheus-medeiros-bd-tf-ad-ufpe-2018.txt", sep = ";", fileEncoding = "latin1") #Pegar base de dados

# Verificando carregamento da base dados

dim(db) # Ver dimensões da base de dados 

head(db)

summary(db) # Resumo da base de dados

# Seccionando dados

as.character(db$ano) #Ler a variável ano como caracter

db$id <- paste(as.character(db$ano),as.character(db$cod_mun)) # Removendo dados duplicados
db <- db[duplicated(db$id) == F,]

df <- db[db$ano > 2002 & db$ano <= 2006,]  #Delimitando a variável Ano a partir de 2003, que corresponde ao primeiro mandato de Lula

df <- df[df$populacao >= 100000,] # Delimitando a variável população para mais de 100000 habitantes, seguindo a indicação de Cerqueira et al (2017)

df <- na.omit(df) # Eliminando observações com dados omissos

# Estatísticas descritivas das variáveis

summary(df)

# Gráficos descritivos de variáveis

hist(df$hom_pc, main = 'Histograma Taxa de Homicídios')
hist(df$gini, main = 'Histograma Índice de Gini')
hist(df$educacao, main = 'Histograma Anos de Estudos (idade > 18)')
hist(df$renda_media, main = 'Histograma Renda Média')
hist(df$pobreza, main = 'Histograma Prop. de População Linha da Pobreza')
hist(df$pib_pc, main = 'Histograma PIB per capta')

# Ajustando Variáveis que não apresentaram distribuição normal

df$hom_pc <- log(df$hom_pc)
df$pobreza <- log(df$pobreza)
df$pib_pc <- log(df$pib_pc)

## Verificando associação entre VIs e VD

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

# Padronizando Variáveis
df$hom_pc <- scale(df$hom_pc)
df$gini <- scale(df$gini)
df$educacao <- scale(df$educacao)
df$renda_media <- scale(df$renda_media)
df$pobreza <- scale(df$pobreza)
df$pib_pc <- scale(df$pib_pc)

# Modelo de Regressão
reg <- lm(hom_pc ~ gini + educacao + renda_media + pobreza + 
            pib_pc + I(pib_pc ^ 2) + factor(ano), data = df)

# Sumário do Modelo
summary(reg)

# Gráfico dos Coeficientes
coefplot(reg, intercept = F)

# Verificando pressupostos do modelo
# Normalidade dos Resíduos
hist(residuals(reg))

# Heterocedasticidade
plot(reg, which = 1)

# Multicolinearidade
vif(reg)

# Utilizando estimador de erros-padrão de Huber-White para corrigir heterocedasticidade
coeftest(reg,vcov. = vcovHC)
