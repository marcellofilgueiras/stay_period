library(tjsp)
library(tidyverse)
library(stringr)
library(ggplot2)
library(lubridate)
library(JurisVis)  

stay_final <- inner_join(stay_agravantes, stay_sem_creditos, by= "processo")

stay_final <- stay_final%>%
  inner_join(stay_dispositivo_corrig, by= "processo")

stay_final2 <- stay_final%>%
  select(processo, parte_nome, tipo_parte, primeiro, segundo, terceiro)%>%
  
  
stay_teste<- stay_final2%>%
  filter(tipo_parte == "outros")%>%
  mutate(tipo_parte = case_when(
    processo %in% recuperanda ~"recuperanda",
    processo %in% tirar ~ "tirar",
    TRUE ~ "credor"))
  
  recuperanda <- c("20658731520208260000","21148443120208260000", "21579374920178260000", "21901755820168260000")

  tirar <- c("20588790520198260000","20694852920188260000", "22590155220188260000", "02864788120108260000", "02864788120108260000", "20724998920168260000")
  
  credor2<-stay_teste%>%
    filter(tipo_parte== "credor")%>%
    pull(processo)
  
  stay_final3<-stay_final%>%
  mutate(tipo_parte = case_when(
      processo %in% recuperanda ~"recuperanda",
      processo %in% tirar ~ "tirar",
      processo %in% credor2 ~ "credor",
      TRUE ~ as.character(tipo_parte)))%>%
    filter(!tipo_parte=="tirar")
  
  table(stay_final3$tipo_parte)
  
  stay_final4 <- stay_final3%>%
    select(processo, tipo_parte, decisão, primeiro, segundo, terceiro,ano_julgamento, orgao_julgador, relator)%>%
    mutate(posicao_prorrog=  case_when(
      tipo_parte == "credor" & decisão == "provido" ~"Contra a prorrogação",
      tipo_parte == "recuperanda" & decisão == "improvido" ~"Contra a prorrogação",
      tipo_parte == "credor" & decisão == "improvido" ~"A favor da prorrogação",
      tipo_parte == "recuperanda" & decisão == "provido" ~"A favor da prorrogação",
      ))
  
 stay_final_merito<- stay_final3%>%
  filter(str_detect(decisão, "provi|parci"))


stay_final_merito2 <- stay_final_merito%>%
  select(processo, tipo_parte, decisão, primeiro, segundo, terceiro,ano_julgamento, orgao_julgador, relator)%>%
  mutate(posicao_prorrog=  case_when(
    tipo_parte == "credor" & decisão == "provido" ~"Contra a prorrogação",
    tipo_parte == "recuperanda" & decisão == "improvido" ~"Contra a prorrogação",
    tipo_parte == "credor" & decisão == "improvido" ~"A favor da prorrogação",
    tipo_parte == "recuperanda" & decisão == "provido" ~"A favor da prorrogação",
    decisão== "parcial" ~ "A favor, mas limita o prazo pugnado"
  )) 

stay_final_merito2_contas<-stay_final_merito2%>%
  group_by(relator)%>%
  count(posicao_prorrog)



##Gráfico
paleta<- c("#619CFF", "#00BA38", "#F8766D")
paleta2<- c("#619CFF", "#00BA38", "#F8766D", "#619CFF", "#00BA38", "#F8766D")

decisões_total <- stay_final3%>%
  ggplot(aes(x=tipo_parte, fill=decisão)) + geom_bar(position="dodge") +
  labs(x = "Natureza da Parte",y="Nº de Decisões", fill = "Decisão do Juízo",
       title = "Decisões acerca da Prorrogação do Stay Period", subtitle = "Divido por Posição do Agravante em Credor ou Recuperando") +
  theme_classic()
decisões_total

table(stay_final3$decisão)

decisões_merito <- stay_final_merito%>%
  ggplot(aes(x=tipo_parte, fill=decisão)) + geom_bar(position="dodge", width = 0.5) +
  labs(x = "Natureza da Parte",y="Nº de Decisões", fill = "Decisão do Juízo",
       title = "Provimentos ou Improvimentos em Agravos sobre Prorrogação do Stay Period", subtitle = "Divido por Posição do Agravante em Credor ou Recuperando", caption = "Fonte: TJSP") +
  scale_x_discrete(labels = c("Credor", "Empresa Recuperanda")) +
  scale_fill_discrete(labels= c("Improvido", "Parcialmente Provido", "Provido")) + theme_classic()
decisões_merito

decisões_merito_prop <- stay_final_merito%>%
  ggplot(aes(x=tipo_parte, fill=decisão)) + geom_bar(position="fill", width = 0.5) +
  labs(x = "Natureza da Parte",y="Porcentagem de Decisões em cada Parte", fill = "Decisão do Juízo",
       title = "Proporção de Provimentos ou Improvimentos em Agravos sobre Prorrogação do Stay Period", subtitle = "Porcentagem por Posição do Agravante em Credor ou Recuperando", caption = "Fonte: TJSP") +
  scale_x_discrete(labels = c("Credor", "Empresa Recuperanda")) +
  scale_fill_discrete(labels= c("Improvido", "Parcialmente Provido", "Provido")) + theme_classic()
decisões_merito_prop

grafico_afavor_contra_prorrog <- stay_final_merito2%>%
    ggplot(aes(x=posicao_prorrog)) + geom_bar( width = 0.5) +
  labs(y = "Nº de Decisões",x="Posição do Juízo",
       title = "Posição das Decisões sobre Prorrogação do Stay Period", caption = "Fonte: TJSP") +
  theme_classic()
grafico_afavor_contra_prorrog

table(stay_final_merito2$posicao_prorrog)


grafico_por_relator <- stay_final_merito2%>%
  ggplot(aes(y=relator, fill= posicao_prorrog)) + geom_bar() +
  labs(x = "Nº de Decisões",y="Relator", fill = "Posição do Juízo",
       title = "Posição de cada Relator acerca da Prorrogação do Stay Period", caption = "Fonte: TJSP") + theme_classic()
grafico_por_relator

grafico_por_ano_camara <- stay_final_merito2%>%
  ggplot(aes(x=posicao_prorrog )) + geom_bar(fill= paleta2, width = 0.5) + facet_wrap("orgao_julgador") + labs(y = "Nº de Decisões", x="Posição do Juízo",title = "Posição das Decisões sobre Prorrogação do Stay Period", caption = "Fonte: TJSP", subtitle ="Dividido por Orgão Julgador") + theme_classic()

grafico_por_ano_camara

grafico_por_ano<- stay_final_merito2%>%
  filter(!ano_julgamento== 2014)%>%
  filter(!ano_julgamento== 2015)%>%
  ggplot(aes(x=as.factor(ano_julgamento), fill= posicao_prorrog)) + geom_bar(position = "fill") +
  labs(y = "Porcentagem de Decisões",x="Ano", fill = "Posição do Juízo",
       title = "Variação da Posição Prorrogação do Stay Periodor Ano de Julgamento", subtitle = "Retirado 2014 e 2015 pelo baixo número de decisões", caption = "Fonte: TJSP") + theme_classic()
grafico_por_ano

