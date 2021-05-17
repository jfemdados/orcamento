#Orçamento com COVID - Despesas Extraordinária em contratadas por Outras Modalidades de Licitação
#Autor: Marcello Filgueiras - JF EM DADOS

library(tidyverse)

#Importing -------------------------------------------------------------

library(readxl)

nome_colunas <- c("Fornecedor",	
                  "CNPJ/CPF",	
                  "Objeto",
                  "Quantitativo_Objeto",
                  "Modalidade_Licitação",
                  "contrato",
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

despesas_om_0303_raw <- read_excel("orçamento_corona/dados_orçamento/despesas_om_0303.xls",
                               col_names = nome_colunas, skip= 2)%>%
  janitor::clean_names()


#Tidying -------------------------------------------------------------

regex_limpa_fornecedor <- "Fornecedor|TOTAL|SECRETARIA|DEPARTAMENTO MUNICIPAL DE LIMPEZA|FUNDAÇÃO CULTURAL ALFREDO FERREIRA LAGE|AGÊNCIA DE PROTEÇÃO E DEFESA DO CONSUMIDOR"

despesas_om_0303_tidy <- despesas_om_0303_raw %>%
  #retirando secretarias das linhas e ao mesmo tempo criano uma coluna para isso
  mutate(secretaria = str_extract(despesas_om_0303$fornecedor, ".*\\(\\w*\\)\\n")) %>%
  fill(secretaria, .direction = "down") %>%
  tidyr::drop_na(fornecedor) %>%
  filter(!str_detect(fornecedor,regex_limpa_fornecedor)) %>%
  relocate(secretaria, .before= "fornecedor") %>%
  #transformando vazios em NA
  na_if("-") %>%
  #antes de fazer regex, vamos deixar tudo minusculo para facilitar
  mutate(across( where(is.character),
              str_to_lower)) %>%
  #na mesma coluna modalidade_licitação, temos a modalidade da licitação e o número do processo licitatório. #Quando há processo licitatório, é precedido por "nº".  #Quando há dispensa de licitação, é feita menção somente a lei, sem "nº".  # Vamos separar!
  separate(modalidade_licitacao,
          into= c("modalidade_licitacao", "n_licitacao"),
          sep= regex("nº|n°|n•")) %>%
  #problema parecido no n_contrato. Primeiro temos o tipo de contrato, para depois o número.
  mutate(n_contrato= str_extract(contrato, "\\d+\\.\\d+\\.\\d+|.+/d+")) %>%
  relocate(n_contrato, .after= "contrato")


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

