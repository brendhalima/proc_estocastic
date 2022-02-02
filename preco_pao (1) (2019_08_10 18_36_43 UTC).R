library(sqldf)
library(markovchain)
library(diagram)
library(RHmm)
library(surveybootstrap)
library(ggplot2)
library(igraph)

setwd("C:/Users/welitin/Desktop/Estocasticos/Estocasticos 2")
list.files()

precos<- read.csv(file="2018-06-04_Disque_Economia_-_Base_de_Dados.csv", head=T, sep=";",stringsAsFactors = F)
head(precos)

dadospao <- subset(precos, CODIGO_PRODUTO == 19)

dadospao$dia <- as.numeric(substr(dadospao$DATA_PESQUISA, start = 1, stop = 2))
dadospao$mes <- as.numeric(substr(dadospao$DATA_PESQUISA, start = 4, stop = 5))
dadospao$diames <- paste(dadospao$dia,dadospao$mes)

  
resumo <- sqldf("select DATA_PESQUISA,sum(PRECO_PESQUISADO) as soma_valor, count(*) as qtde_med
                          from dadospao group by diames")
resumo$DATA_PESQUISA <- as.Date(resumo$DATA_PESQUISA, "%d/%m/%Y")
resumo$preco_medio <- resumo$soma_valor/resumo$qtde_med
ordenando = resumo[order(resumo$DATA_PESQUISA),]
resumo_2 <- sqldf("select *, case when preco_medio <= 8.5 then 'a'
                                 when preco_medio <= 9 then 'b'
                                 else 'c' end as class_preco
                                 from ordenando")



#Analise Exploratoria dos Dados

ggplot(resumo_2, aes(x=DATA_PESQUISA)) + 
  geom_line(aes(y=preco_medio)) + 
  labs(title="Precos Medio do Pao Frances", 
       caption="Source: Pref Curitiba", 
       y="Preco R$") 

summary(resumo_2)


#Tabela de Categorias
#Arbitrariamente foi escolhido os pontos de corte do tipo
#Se menor que 8.5 = A
#Se entre 8.5 e 9 = B
#Maior que 9 = C



# Grafico de Barras 

dados.plot <- sqldf("select class_preco as Categoria,count(*) as 'FreqAbsoluta'
                                 from resumo_2 group by class_preco")
dados.plot$FreqRelativa  <- dados.plot$FreqAbsoluta/sum(dados.plot$FreqAbsoluta)
dados.plot$FreqCumulativa <- c(0.26,(0.26+0.32),1)

ggplot(dados.plot, aes(x=Categoria, y=FreqRelativa)) +
  geom_bar(stat="identity") + 
  geom_line(aes(y=FreqCumulativa*max(FreqRelativa), group=1)) +
  labs(x="Categoria", y="Frequência Relativa (%)") + 
  geom_text(aes(label=FreqAbsoluta), vjust=-0.8) +
  scale_y_continuous(
    sec.axis=sec_axis(trans=~ .*100/(max(dados.plot$FreqRelativa)), 
                      name = "Frequência Cumulativa (%)"))





#Partindo para Matriz de Transicao
precos <- resumo_2$class_preco
mcFit <- markovchainFit(data=precos,method = "bootstrap",nboot = 1000)

#matriz de transição preco
mcFit$estimate

is.irreducible(mcFit$estimate)
period(mcFit$estimate)
steadyStates(ProbT)

show(mcFit$estimate)

#distribuica estacionaria
steadyStates(mcFit$estimate)
m = 1 
n = length(precos)
set.seed(17021996)
preditos = predict(mcFit$estimate, newdata=precos[1], n.ahead=49)


a = preditos
a = ifelse(preditos==precos[(m+1):n],1,0)

r = (1/(n-m))*sum(a)   
r                             # 0.4375
