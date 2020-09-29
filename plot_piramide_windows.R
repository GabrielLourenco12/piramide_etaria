############################################################
## script para plotar uma piramide etaria animada para    ##
## o Brasil                                               ##
##                                                        ##
## Desenvolvido por Mercel Santos:                        ##
## email: gabriel9lourenco@gmail.com                      ##
## linkedin: linkedin.com/in/gabriellourenco12            ##
############################################################

rm(list=ls())

#########################################
###             Pacotes               ###
#########################################

library(dplyr) # Para manipular os dados
library(tidyr) # Para manipulacao de dados
library(ggplot2) # Para plotar os dados 
library(gganimate) # Para gerar animacao

#Definindo diretorio de trabalho
setwd("E:/GitHub/piramide_etaria")

dados <- read.csv('dados/WPP2019_PopulationByAgeSex_Medium.csv') %>% # Lendo arquivo do tipo csv
  filter(Location=='Brazil') %>% #Selecionando linha de dados para regiao Brasil
  select(Time,AgeGrp,PopMale,PopFemale) %>% #Selecionando coluna de dados
  mutate(PopFemale=PopFemale*-1) %>%  #Tornando os valores Negativos para populacao do Sexo Feminino 
  gather(sexo,populacao,c(PopMale,PopFemale)) %>% #Juntando os dados da populacao dos Sexos Masculino e Feminino em uma unica coluna
  mutate(sexo=as.factor(sexo)) #Convertendo os dados da coluna sexo para fator
dados = dados[dados$Time < 2050,]
#Estabelecendo os rotulos do eixo y do grafico
rotulos.org <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+") 

meu.plot <- ggplot(dados,aes(populacao,AgeGrp,fill=sexo))+ #Definindo dados e colunas usados para plotar o grafico
  geom_bar(stat = "identity",col='grey90',lwd=.3)+ #Definindo o tipo de grafico (grafico de barras)
  geom_text(data=filter(dados,sexo=='PopFemale'), #Plotando os rotulos na posicao central do grafico
            aes(x=0,y=AgeGrp,label=AgeGrp),       
            col='grey20')+
  scale_y_discrete(limits=rotulos.org)+ #Definindo os limites do eixo y
  scale_x_continuous(limits = c(-10000,10000), #Definindo os limites do eixo x
                     breaks = c(-10000,-5000,0,5000,10000), #Definindo posicoes dos rotulos 
                     labels = c('10000','5000','0','5000','10000'))+ #Definindo os rotulos 
  labs(x="Populacao", #Definindo os titulos dos graficos e das legendas
       y=NULL,
       fill='',
       title='Distribuicao da Populacao por Faixa Etaria',
       subtitle = "Ano {closest_state}")+
  scale_fill_manual(values = c('#F680BE','#1F74B4'), # Definindo as cores de preenchimento das barras
                    labels =c('Feminino','Masculino'))+  # Definindo os rotulos da legenda
  theme_minimal()+ #Definindo o tema
  theme(legend.position = 'top', #Definindo a posicao da legenda
        legend.key.size = unit(5,'mm'), #Definindo o tamanho da legenda
        axis.text.y=element_blank())+ #Apagando os rotulos do eixo Y
  transition_states(Time) #Definindo que a animacao sera gerada em funcao do tempo (Anos)


anim <- animate(plot = meu.plot,duration = 15, #Gerando a animacao e especificando a duracao, dimensoes e resolucao
                width=480,height=560,res=110)

#Salvando a animacao
save_animation(file = 'animations/populacao_animado.gif',animation = anim)
