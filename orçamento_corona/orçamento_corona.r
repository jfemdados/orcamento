library(tidyverse)
library(readxl)
library(readr)


library(readxl)

#Despesas contratadas com a lei 13979 . Dados de 09/02

despesas_13979_0902_sec_dessocial<- read_excel("~/R2/orçamento/orçamento_corona/despesas_13979_0902.xls", 
                                  skip = 3, n_max = 11)

despesas_13979_0902_secadm <- read_excel("~/R2/orçamento/orçamento_corona/despesas_13979_0902.xls", 
                                  skip = 18, n_max = 2)

despesas_13979_0902_demlurb<- read_excel("~/R2/orçamento/orçamento_corona/despesas_13979_0902.xls", 
                                  skip = 24, n_max = 91)

despesas_13979_0902_sec_saude <- read_excel("~/R2/orçamento/orçamento_corona/despesas_13979_0902.xls", 
                                  skip = 95, n_max = 95+74)

despesas_13979_0902_funalfa <- read_excel("~/R2/orçamento/orçamento_corona/despesas_13979_0902.xls", 
                                  skip = 95+78)




#Despesas contratadas por Outras Modalidades de Licitação
despesas_om_0902 <- read_excel("~/R2/orçamento/orçamento_corona/despesas_om_0902.xls", 
                               skip = 3)



#### RECEITAS

dplyr::
select()
mutate()
filter()
group_by()

count()
summarise()


recursos_1101 <- read_excel("~/R2/orçamento/orçamento_corona/recursos_1101.xls", 
                            skip = 4, n_max = 3+75,
                            col_names = c("origem_recurso", "cnpj_cpfbregao", "valor_liberado", "destinacao_programada"))


recursos_1101[1:61,]$origem_recurso <- "governo federal"
recursos_1101[62:65,]$origem_recurso <- "governo estadual"
recursos_1101[66:75,]$origem_recurso <- "ministerio publico"


recurso_1101<- recursos_1101%>%
  filter(origem_recurso != "JUSTIÇA FEDERAL")%>%
  mutate(gov_fed_s_n= ifelse(origem_recurso== "governo federal", "sim", "nao"))%>%
  mutate(grupos= case_when(
    str_detect(destinacao_programada,"leito") ~ "Leito",
    str_detect(destinacao_programada,"EPI") ~ "epi",
    TRUE ~ "outros"))%>%
  group_by(origem_recurso, gov_fed_s_n, grupos)%>%
  tally(valor_liberado)

ggplot(recurso_1101, aes(y=n, n= grupos, fill= gov_fed_s_n)) +geom_col()



