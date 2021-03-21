library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)

#Carregando os dados
data <- read.csv('covid.csv', sep = ";")

#Alterando o tipo dos dados
data$data <- as.Date(data$data)
data$estado[data$estado == ""] <- "NA"
data$municipio[data$municipio == ""] <- "NA"
data$nomeRegiaoSaude[data$nomeRegiaoSaude == ""] <- "NA"
data$casosNovos <- as.numeric(data$casosNovos)
data$obitosNovos <- as.numeric(data$obitosNovos)

#Criando um novo DF, excluindo as 10991 primeiras linhas do DF original
linhas <- c(1:10991)
data.new <- data[-linhas,]

#Estatistica descritiva, mostra todas as informações e dados do DF
summary(data.new)

#Mostra a classe de dados da coluna solicitada
class(data$data)

#substituindo valores negativos por 0
data.new$casosNovos[data.new$casosNovos<0] <- 0
data.new$obitosNovos[data.new$obitosNovos<0] <- 0

#Dia com mais casos
Dia_mais_casos <- max(data.new$casosNovos)

#Dia com mais óbitos
Dia_mais_casos <- max(data.new$obitosNovos)

#soma do total de casos
Total_casos <- sum(data.new$casosNovos)

#Soma do total de óbitos
Total_óbitos <- sum(data.new$obitosNovos)

#População total do Pais
Total_pop <- as.numeric(213642264)

#Letalidade geral
letalidade_geral <- as.numeric(Total_óbitos/Total_casos*100)

#Total de casos por 1mi habitantes
Total_casos_1mi <- Total_casos*1000000/Total_pop

#Total de óbitos por 1mi habitantes
Total_obitos_1mi <- Total_óbitos*1000000/Total_pop

#Grafico de casos x data
ggplot(data.new) + aes(x = data, y= casosNovos, group = data) + geom_line()

#Grafico de óbitos x data
ggplot(data.new) + aes(x = data, y= obitosNovos, group = data) + geom_col()

#Grafico de casos por região
data.new %>% group_by(regiao) %>% 
  summarise(cn = sum(casosNovos)) %>% 
  ggplot(aes(x = fct_reorder(regiao, cn), y = cn))+
  geom_col(color = "dodgerblue")

#Grafico de óbitos por região
data.new %>% group_by(regiao) %>% 
  summarise(co = sum(obitosNovos)) %>% 
  ggplot(aes(x = fct_reorder(regiao, co), y = co))+
  geom_col()

#Grafico de total de casos por estado
data.new %>% group_by(estado) %>% 
  summarise(ce = sum(casosNovos)) %>% 
  ggplot(aes(x = fct_reorder(estado, ce), y = ce))+
  geom_col()





