library(tjsp)
library(tidyverse)
library(stringr)
library(ggplot2)
library(lubridate)
library(JurisVis)        

#Baixando Lista de Processos
agravo <- "202"
camara_emp <- "0-1149,0-1150,0-1157,0-1136"

tjsp::baixar_cjsg(livre = "prorrogação E stay", aspas = FALSE, classe= agravo, orgao_julgador = camara_emp , tipo = "A", diretorio = "stay_period/dados_stay")

stay_dados <- tjsp::ler_cjsg(diretorio= "stay_period/dados_stay")%>% 
  mutate(across(assunto:orgao_julgador,~iconv(.x,"UTF-8","Latin1")))%>%
  mutate(across(relator:orgao_julgador,~iconv(.x,"UTF-8","Latin1")))


# filtros iniciais
stay_prorrogacao <- stay_dados %>% 
  filter(str_detect(ementa,"prorrog"))

stay_assuntos <- stay_prorrogacao %>%
  mutate(ementa= remover_acentos(stay_prorrogacao$ementa))

stay_assuntos_dividido <- stay_assuntos %>%
  mutate(ementa=  str_replace(ementa, pattern = "[^[:alnum:], ]", replacement = "-"))%>%
           separate(col= ementa, into = c("primeiro", "segundo"), sep= "[-]", extra = "merge")%>%
            separate(col= segundo, into = c( "segundo", "terceiro"), sep= "[.]|[–]|[-]", extra = "merge")


#FILTROS MAIS PESADOS - RODAR STAY_ASSUNTOS E sTAY_ASSUNTOS_DIVIDIDOS AO FINAL

#Filtro Agravo Interno
stay_exclusao_int <- stay_assuntos_dividido%>% 
  filter(str_detect(primeiro,"(?i)(agravo interno)")) %>% 
  pull("processo")

stay_sem_agint <- filter(stay_assuntos_dividido,!processo %in% stay_exclusao_int)

#Filtro Sem Homologação de Plano Judicial
stay_exclusao_homolg <- stay_sem_agint %>% 
  filter(str_detect(segundo,"(?i)(homolog)"))%>% 
  pull("processo")

stay_exclusao_desagio <- stay_sem_agint%>%
  filter(str_detect(terceiro, "(?i)(deságio|desagio)"))%>%
    pull("processo")
stay_exclusao_plano <- stay_sem_agint%>%
  filter(str_detect(segundo, "(?i)(plano de recuper)"))%>%
  pull("processo")


stay_sem_homolog <- filter( stay_sem_agint,!processo %in% c(stay_exclusao_homolg, stay_exclusao_desagio, stay_exclusao_plano))

#esse aqui nao fiz stay_exclusao_homolg2 <- stay_sem_homolog%>% 
  #filter(str_detect(terceiro,"(?i)(homolog)")) %>% 
  #pull("processo")

#Filtro sem Falência, Execução e Credores Fiduciários

stay_exclusao_exec <- stay_sem_homolog%>%
  filter(str_detect(primeiro,"(?i)(execucao de titulo)"))%>%
  pull("processo")

stay_exclusao_exec2 <- stay_sem_homolog%>%
 filter(str_detect(segundo,"(?i)(execucao)"))%>%
  pull("processo")


stay_exclusao_falencia <- stay_sem_homolog%>%
  filter(str_detect(primeiro,"(?i)(falencia)"))%>%
  pull("processo")

stay_exclusao_falencia2 <- stay_sem_homolog%>%
  filter(str_detect(segundo,"(?i)(convolacao em falencia)"))%>%
  pull("processo")

stay_exclusao_penhora <- stay_sem_homolog%>%
  filter(str_detect(segundo,"(?i)(penh)"))%>%
  pull("processo")


stay_exclusao_fiduci <- stay_sem_homolog%>%
  filter(str_detect(segundo,"(?i)(fiducia)"))%>%
  pull("processo")


stay_exclusao_arrend <- stay_sem_homolog%>%
  filter(str_detect(segundo,"(?i)(arrenda)"))%>%
  pull("processo")

stay_exclusao_habilit <- stay_sem_homolog%>%
  filter(str_detect(primeiro,"(?i)(habilita)"))%>%
  pull("processo")

stay_exclusao_credito <- stay_sem_homolog%>%
  filter(str_detect(segundo,"(?i)(credito)"))%>%
  pull("processo")

stay_exclusao_credito2 <- stay_sem_homolog%>%
  filter(str_detect(terceiro,"(?i)(trabalhi)"))%>%
  pull("processo")


#naofiz stay_exclusao_essenc<- stay_sem_homolog%>%
  #filter(str_detect(segundo,"(?i)(essenci)"))%>%
  #pull("processo")

stay_sem_creditos <- filter(stay_sem_homolog,!processo %in%
  c(stay_exclusao_exec, stay_exclusao_exec2, stay_exclusao_falencia, stay_exclusao_falencia2,stay_exclusao_penhora,stay_exclusao_fiduci, stay_exclusao_arrend, stay_exclusao_habilit, stay_exclusao_credito, stay_exclusao_credito2))

#Filtro de Prazo - não deu certo

#stay_exclusao_ <- stay_sem_creditos%>%
  #filter(str_detect(terceiro,"(?i)(materia)"))%>%
  #pull("processo")


#filtro daquele util mas nao recomendável

##mutate(a_assunto_ementa = case_when(
             #str_detect(ementa, "(?i)(agrav)?<=(.|-)") ~ "agravo",
             #str_detect(ementa, "(?i)(rec)?<=(.|-)") ~ "recuperação judicial",
             #TRUE ~ "outros"
           #))

#Filtros Utilizados


stay_funil_filtros <- jus_funnel(
  stages = c("Consulta ao 2º Grau\n TJSP sobre\n 'prorrogação e stay'",
             "Restringe a julgados com \n prorrogação na ementa",
             "Remove agravos internos",
             "Remove julgados sobre\n homologação do plano de RJ",
             "Remove julgados sobre a \n penhora ou constrição de bens"),
  initial= 760,
  removals= c(402,4,50,17),
  title= "Filtros Aplicados para Limitação à Prorrogação do Stay Period" )

stay_funil_filtros
jus

# para baixar os julgados e metadados, você precisa de uma conta de OAB  no site do tjsp para usar a função autenticar. Não vou colocar a minha aqui, certo?

#Baixando Julgados
autenticar(login = "###########", password = "##########")
 tjsp::tjsp_baixar_acordaos_cjsg(stay_sem_creditos$cdacordao, diretorio = "stay_period/julgados_stay")
stay_julgados<- tjsp_ler_acordaos_cjsg(diretorio = "stay_period/julgados_stay")


#Baixando Metadados
autenticar(login = "############", password = "###########")
baixar_cposg(processos = stay_sem_creditos$processo, diretorio = "stay_period/metadados_stay")

#Analisando Metadados
stay_metadados <- ler_dados_cposg(diretorio = "stay_period/metadados_stay")
stay_partes <- ler_partes(diretorio = "stay_period/metadados_stay" )
stay_agrav <- stay_partes %>%
  filter(str_detect(parte, "(?i)agrav")) 

  
stay_dispositivo <- tjsp::ler_decisoes_cposg(diretorio = "stay_period/metadados_stay")
stay_dispositivo<- stay_dispositivo%>%
  mutate(decisão = classificar_recurso2(stay_dispositivo$dispositivo))

stay_dispositivo_corrig<- stay_dispositivo %>%
  mutate(processo= str_replace_all(processo, "[.|-]", ""))%>%
  mutate(decisão= case_when(
    str_detect(stay_dispositivo_corrig$dispositivo, "parte") ~ "parcial",
    str_detect(stay_dispositivo_corrig$decisão, "desistência") ~ "desistência",
    str_detect(stay_dispositivo_corrig$decisão, "duvida") ~ "duvida",
    str_detect(stay_dispositivo_corrig$decisão, "improvido") ~ "improvido",
    str_detect(stay_dispositivo_corrig$decisão, "não conhecido") ~ "não conhecido",
    str_detect(stay_dispositivo_corrig$decisão, "parcial") ~ "parcial",
    str_detect(stay_dispositivo_corrig$decisão, "prejudicado/extinto") ~ "prejudicado/extinto",
    str_detect(stay_dispositivo_corrig$decisão, "provido") ~ "provido",
    str_detect(stay_dispositivo_corrig$dispositivo, "confirmada") ~ "provido",
  ))
                     
                     stay_sem_homolog,!processo %in%
                                c(stay_exclusao_exec, stay_exclusao_exec2, stay_exclusao_falencia, stay_exclusao_falencia2,stay_exclusao_penhora,stay_exclusao_fiduci, stay_exclusao_arrend, stay_exclusao_habilit, stay_exclusao_credito, stay_exclusao_credito2))


table(stay_dispositivo$decisão)

#tentando filtar pelo stay_agravantes

stay_agrav <- stay_agrav %>%
  mutate(tipo_parte =
           case_when(
             str_detect(parte_nome, "(?i)(rec)") ~ "recuperanda",
             str_detect(parte_nome, "(?i)(banco)") ~ "credor",
             str_detect(parte_nome, "(?i)(caixa|cef)") ~ "credor",
             str_detect(parte_nome, "(?i)(jui|ízo)") ~ "juizo",
             str_detect(parte_nome, "(?i)(fundo)") ~ "credor",
             str_detect(parte_nome, "(?i)(bank)") ~ "credor",
             TRUE ~ "outros"
           ))

stay_agrav%>%
  filter
  ggplot(aes(x=tipo_parte)) + geom_bar() + facet_wrap(vars(parte)) +
  ggtitle("Tipos de Parte e sua Posição nos Processos sobre Prorrogação do Stay Period na 2ª Instância de São Paulo")



stay_agravantes <-stay_agrav %>%
  filter(parte== "Agravante")

table(stay_agravantes$tipo_parte)


stay_agravantes%>%
  ggplot(aes(x=tipo_parte)) + geom_bar() + 
  ggtitle("Agravantes em Processos sobre Prorrogação do Stay Period na 2ª Instância de São Paulo")


 a <- stay_dispositivo%>% count(processo)

 b<- stay_sem_creditos %>% count(processo)
 
 c<- stay_final3 %>% count(processo)
 


#errado
stayexclusoes <- stay_dados %>% 
  filter(str_detect(ementa,"prorrogaçã")) %>% 
  pull("processo")
inc_desvio_sem_nulidade <- filter(inc_desvio,!processo %in% exclusoes)

#graficozin
stay_dispositivo %>%
  ggplot(aes(x=decisão)) + geom_bar()


