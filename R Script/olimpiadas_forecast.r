setwd("C:/") #diretório de trabalho
olimpiadas <- read.csv(file="MedalhasAtualizado.csv",head=TRUE,sep=";") #lendo arquivo CSV
head(olimpiadas) #verificando as primeiras linhas

usa <- subset(olimpiadas, Country == "United States" & as.Date(Data) >= as.Date("1984-01-01")) #subset estados unidos
china <- subset(olimpiadas, Country == "China" & as.Date(Data) >= as.Date("1984-01-01")) #subset china
brasil <- subset(olimpiadas, Country == "Brazil" & as.Date(Data) >= as.Date("1948-01-01")) #subset brasil

head(brasil,n=30) #verificando dados atribuídos

ord.usa <- usa[order(usa$Data),] #ordenando por data
ord.china <- china[order(china$Data),] #ordenando por data
ord.brasil <- brasil[order(brasil$Data),] #ordenando por data


ts_olimpiadas_usa = ts(ord.usa$Total, 
                    start=c(1984,1),frequency=0.25) #início 1948, frequência de 4 em 4 anos

ts_olimpiadas_chi = ts(ord.china$Total, 
                    start=c(1984,1),frequency=0.25) #início 1984, frequência de 4 em 4 anos


ts_olimpiadas_bra = ts(ord.brasil$Total, 
                    start=c(1948,1),frequency=0.25) #início 1948, frequência de 4 em 4 anos


plot(ts_olimpiadas_usa) #gráfico de medalhas dos estados unidos até 2016
plot(ts_olimpiadas_chi) #gráfico de medalhas da china até 2016
plot(ts_olimpiadas_bra) #gráfico de medalhas do brasil até 2016


library(forecast) #biblioteca de forecast

m_ets_usa = ets(ts_olimpiadas_usa) #modelo de ets, estados unidos
m_ets_chi = ets(ts_olimpiadas_chi) #modelo de ets, china
m_ets_bra = ets(ts_olimpiadas_bra) #modelo de ets, brasil

f_ets_usa = forecast(m_ets_usa, h=4) # previsão próximas 4 Olimpíadas
f_ets_chi = forecast(m_ets_chi, h=4) # previsão próximas 4 Olimpíadas
f_ets_bra = forecast(m_ets_bra, h=4) # previsão próximas 4 Olimpíadas

plot(f_ets_usa) #gráfico de forecast básico, estados unidos
plot(f_ets_chi) #gráfico de forecast básico, china
plot(f_ets_bra) #gráfico de forecast básico, brasil

summary(f_ets_usa) #resumo das informações do forecast, estados unidos
summary(f_ets_chi) #resumo das informações do forecast, china
summary(f_ets_bra) #resumo das informações do forecast, brasil

library(ggfortify)

autoplot(f_ets_usa, ts.colour = 'blue',predict.colour = 'red',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'lightblue') #gráfico de forecast dos estados unidos
autoplot(f_ets_chi, ts.colour = 'red',predict.colour = 'black',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'yellow')  #gráfico de forecast da china
autoplot(f_ets_bra, ts.colour = 'darkgreen',predict.colour = 'blue',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'yellow') #gráfico de forecast do brasil

autoplot(f_ets_usa, ts.colour = 'blue',predict.colour = 'red',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'lightblue') + ggtitle("Projeção de Total de Medalhas") + labs(x="Ano",y="Total de Medalhas") + annotate("text",x=2000,y=-1,label="Previsão para 2020: 118 medalhas",color="red")
autoplot(f_ets_chi, ts.colour = 'red',predict.colour = 'black',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'yellow') + ggtitle("Projeção de Total de Medalhas") + labs(x="Ano",y="Total de Medalhas") + annotate("text",x=2000,y=-1,label="Previsão para 2020: 72 medalhas",color="black")
autoplot(f_ets_bra, ts.colour = 'darkgreen',predict.colour = 'blue',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'yellow') + ggtitle("Projeção de Total de Medalhas") + labs(x="Ano",y="Total de Medalhas") + annotate("text",x=1980,y=-1,label="Previsão para 2020: 15 medalhas",color="darkblue")


packs <- c("png","grid") #lendo bibliotecas
lapply(packs, require, character.only = TRUE)

imgusa <- readPNG("usa.png") #carregando imagem bandeira estados unidos
imgchina <- readPNG("china.png") #carregando imagem bandeira china
imgbrasil <- readPNG("brasil.png") #carregando imagem bandeira brasil

gusa<- rasterGrob(imgusa, x=.8, y=.1, height=.15,width=.2,interpolate=TRUE) #definindo posição bandeira estados unidos
gchi<- rasterGrob(imgchina, x=.8, y=.1, height=.15,width=.2,interpolate=TRUE) #definindo posição bandeira china
gbra<- rasterGrob(imgbrasil, x=.8, y=.1, height=.15,width=.2,interpolate=TRUE) #definindo posição bandeira brasil

autoplot(f_ets_usa, ts.colour = 'blue',predict.colour = 'red',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'lightblue') + annotation_custom(gusa) + ggtitle("Projeção de Total de Medalhas") + labs(x="Ano",y="Total de Medalhas") + annotate("text",x=2000,y=-1,label="Previsão para 2020: 118 medalhas",color="red") #adicionando função annotation_custom()
autoplot(f_ets_chi, ts.colour = 'red',predict.colour = 'black',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'yellow') + annotation_custom(gchi) + ggtitle("Projeção de Total de Medalhas") + labs(x="Ano",y="Total de Medalhas") + annotate("text",x=2000,y=-1,label="Previsão para 2020: 72 medalhas",color="black")
autoplot(f_ets_bra, ts.colour = 'darkgreen',predict.colour = 'blue',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'yellow') + annotation_custom(gbra) + ggtitle("Projeção de Total de Medalhas") + labs(x="Ano",y="Total de Medalhas") + annotate("text",x=1980,y=-1,label="Previsão para 2020: 15 medalhas",color="darkblue")
