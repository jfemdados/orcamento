#Orçamento com COVID - Despesas Extraordinária em contratadas por Outras Modalidades de Licitação
#Autor: Marcello Filgueiras - JF EM DADOS

library(tidyverse)

#Importing -------------------------------------------------------------



 ###### Jeito Manual de Importar, um df por secretaria #######

library(readxl)

nome_colunas <- c("Fornecedor",	
                  "CNPJ/CPF",	
                  "Objeto",
                  "Quantitativo_Objeto",
                  "Modalidade_Licitação",
                  "N_Contrato",
                  "Prazo_Contratual",
                  "Valor_Total_contrato",
                  "N_Processo",
                  "Fonte_Recurso",
                  "N_Empenho"	,
                  "Data_Empenho", 
                  "Valor_Empenho",
                  "Valor_Liquidado",
                  "Valor_Pago",
                  "Saldo(Empenho-Pago)",
                  "N_DocDiscal",
                  "Data_DocFiscal")

despesas_om_procon <- read_xls(path = "orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                               sheet = 2,
                               col_names = nome_colunas,
                               skip = 3) %>%
                      mutate(Deparatamento_Pref = "Procon")


despesas_om_sec_segurança<- read_xls(path = "orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                         sheet = 3,
                         col_names = nome_colunas,
                         skip = 3) %>%
  mutate(Deparatamento_Pref = "Secretaria de Segurança")




despesas_om_funalfa<- read_xls(path = "orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                                     sheet = 4,
                                     col_names = nome_colunas,
                                     skip = 3) %>%
  mutate(Deparatamento_Pref = "Funalfa")



despesas_om_demlurb<- read_xls(path = "orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                               sheet = 5,
                               col_names = nome_colunas,
                               skip = 3) %>%
  mutate(Deparatamento_Pref = "Demlurb")


despesas_om_meio_ambiente<- read_xls(path = "orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                               sheet = 6,
                               col_names = nome_colunas,
                               skip = 3) %>%
  mutate(Deparatamento_Pref = "Meio Ambiente")



despesas_om_educacao<- read_xls(path = "orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                                     sheet = 7,
                                     col_names = nome_colunas,
                                     skip = 3) %>%
  mutate(Deparatamento_Pref = "Secretaria de Educação")




despesas_om_desenvolvimento_social <- read_xls(path = "orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                                     sheet = 8,
                                     col_names = nome_colunas,
                                     skip = 3) %>%
  mutate(Deparatamento_Pref = "Secretaria de Desenvolvimento Social")



despesas_om_secom <- read_xls(path = "orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                                               sheet = 9,
                                               col_names = nome_colunas,
                                               skip = 3) %>%
  mutate(Deparatamento_Pref = "Secretaria de Comunicação")



despesas_om_obras <- read_xls(path = "orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                              sheet = 10,
                              col_names = nome_colunas,
                              col_types = 
                              skip = 3) %>%
  mutate(Deparatamento_Pref = "Secretaria de Saúde")



despesas_om_saude <- read_xls(path = "orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                              sheet = 11,
                              col_names = nome_colunas,
                              skip = 3) %>%
  mutate(Deparatamento_Pref = "Secretaria de Saúde")

#Joining -------------------------------------------------------------

despesas_om <- full_join( despesas_om_saude, despesas_om_obras) %>%
  full_join(despesas_om_secom) %>%
  full_join(despesas_om_desenvolvimento_social) %>%
  full_join(despesas_om_educacao) %>%
  full_join(despesas_om_meio_ambiente) %>%
  full_join(despesas_om_desenvolvimento_social) %>%
  full_join(despesas_om_demlurb) %>%
  full_join(despesas_om_funalfa) %>%
  full_join(despesas_om_procon) %>%
  full_join(despesas_om_sec_segurança)



####### Jeito Mais Inteligente #####

library(readxl)

nome_colunas <- c("Fornecedor",	
                  "CNPJ/CPF",	
                  "Objeto",
                  "Quantitativo_Objeto",
                  "Modalidade_Licitação",
                  "N_Contrato",
                  "Prazo_Contratual",
                  "Valor_Total_contrato",
                  "N_Processo",
                  "Fonte_Recurso",
                  "N_Empenho"	,
                  "Data_Empenho", 
                  "Valor_Empenho",
                  "Valor_Liquidado",
                  "Valor_Pago",
                  "Saldo(Empenho-Pago)",
                  "N_DocDiscal",
                  "Data_DocFiscal")

despesas_om_0303 <- read_excel("orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                               col_names = nome_colunas, skip= 2)


#Tidying -------------------------------------------------------------

regex_limpa_fornecedor <- "Fornecedor|TOTAL|SECRETARIA|DEPARTAMENTO MUNICIPAL DE LIMPEZA|FUNDAÇÃO CULTURAL ALFREDO FERREIRA LAGE|AGÊNCIA DE PROTEÇÃO E DEFESA DO CONSUMIDOR"

despesas_om_0303_tidy <- despesas_om_0303 %>%
  mutate(Secretaria = str_extract(despesas_om_0303$Fornecedor, ".*\\(\\w*\\)\\n")) %>%
  fill(Secretaria, .direction = "down") %>%
  tidyr::drop_na(Fornecedor)%>%
  filter(!str_detect(Fornecedor,regex_limpa_fornecedor)) %>%
  relocate(Secretaria, .before= "Fornecedor")
 


secretarias_nome<- c("SECRETARIA DE COMUNICAÇÃO PÚBLICA (SECOM)",
                "SECRETARIA DE OBRAS (SO)", 
                "SECRETARIA DE SAÚDE (SS)",
                "SECRETARIA DE DESENVOLVIMENTO SOCIAL (SDS)",
                "SECRETARIA DE EDUCAÇÃO (SE)",
                "SECRETARIA DE MEIO AMBIENTE E ORDENAMENTO URBANO (SEMAUR)",
                "DEPARTAMENTO MUNICIPAL DE LIMPEZA URBANA (DEMLURB)",
                "FUNDAÇÃO CULTURAL ALFREDO FERREIRA LAGE (FUNALFA)",
                "SECRETARIA DE SEGURANÇA URBANA E CIDADANIA (SESUC)")
 

#Classifying -------------------------------------------------------------


#Modelling -------------------------------------------------------------

#Visualizing -------------------------------------------------------------

#exporting -------------------------------------------------------------