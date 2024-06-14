
############################################## Definindo o diretório #####################################################################

setwd("C:/Users/audencio.victor/OneDrive - Ministério da Saúde/e-SUS_Plinio/Dados_Sem_Tratamento/esus_notifica_22") ### Atualizar o ano 

############################################## Instalando Pacotes #######################################################################
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, psych, descr, data.table, lubridate, abjutils, dplyr, tidyverse, openxlsx, geobr, raster, ggspatial, rio, flextable, 
               fields, sf, gt, janitor, readxl, MASS, knitr, ggrepel, udunits2, rstatix, extrafont, here, officer, splitstackshape, sjmisc, 
               hablar, ggsn, surveillance, gridExtra, grid, ggpubr, httr, stringr, gtsummary, rvest, gganimate, hrbrthemes, gghighlight, 
               ggflags, kableExtra, vtable, magrittr, formattable, hrbrthemes)

########################################################################################################################################
# Função para limpeza de dados
clean_data <- function(dados) {
  names(dados) <- make.unique(names(dados))
  dados$dataInicioSintomas <- lubridate::as_date(dados$dataInicioSintomas)
  dados$dataInicioSintomas[dados$dataInicioSintomas < lubridate::as_date("2020-01-01")] <- NA
  dados$dataInicioSintomas[dados$dataInicioSintomas > Sys.Date()] <- NA
  
  dados <- dados %>% 
  mutate(dataInicioSintomas = if_else(is.na(dataInicioSintomas), dataTeste, dataInicioSintomas)) %>% 
  mutate(dataInicioSintomas = if_else(is.na(dataInicioSintomas), dataTesteSorologico, dataInicioSintomas)) %>% 
  mutate(dataInicioSintomas = if_else(is.na(dataInicioSintomas), dataNotificacao, dataInicioSintomas))
  
  dados$anoEpiSintomas <- lubridate::epiyear(dados$dataInicioSintomas)
  dados$semEpiSintomas <- lubridate::epiweek(dados$dataInicioSintomas)
  
  dados <- dados %>% mutate(across(.cols = c("nomeCompleto", "nomeMae", "logradouro", "profissionalSaude", "profissionalSeguranca", "estrangeiro"), 
                                   .fns = ~ rm_accent(.) %>% str_to_upper() %>% str_trim(side = "both") %>% str_replace_all("[^[:alpha:]]", " ") %>% str_squish()))
  
  dados$dataNascimento <- as_date(dados$dataNascimento)
  dados$dataNascimento[dados$dataNascimento < as_date("1901-01-01")] <- NA
  dados$dataNascimento[dados$dataNascimento > Sys.Date()] <- NA
  
  dados$dataNotificacao <- as_date(dados$dataNotificacao)
  dados$dataNotificacao[dados$dataNotificacao < as_date("2020-01-01")] <- NA
  dados$dataNotificacao[dados$dataNotificacao > Sys.Date()] <- NA
  
  dados$dataTeste <- as_date(dados$dataTeste)
  dados$dataTeste[dados$dataTeste < as_date("2020-01-01")] <- NA
  dados$dataTeste[dados$dataTeste > Sys.Date()] <- NA
  
  dados$dataEncerramento <- as_date(dados$dataEncerramento)
  dados$dataEncerramento[dados$dataEncerramento < as_date("2020-01-01")] <- NA
  dados$dataEncerramento[dados$dataEncerramento > Sys.Date()] <- NA
  
  dados$sexo[dados$sexo == "Indefinido" | dados$sexo == ""] <- NA
  dados$racaCor[dados$racaCor == ""] <- NA
  dados$idade <- as.integer(dados$idade)
  dados$idade[dados$idade == 0] <- NA
  
  dados$estado <- as.character(dados$estado)
  dados$estado[dados$estado == ""] <- NA
  
  dados$estadoNotificacao <- as.character(dados$estadoNotificacao)
  dados$estadoNotificacao[dados$estadoNotificacao == ""] <- NA
  
  dados$resultadoTeste <- as.character(dados$resultadoTeste)
  dados$resultadoTeste <- rm_accent(dados$resultadoTeste)
  dados$resultadoTeste <- str_replace_all(dados$resultadoTeste, "[^[:alpha:]]", " ")
  dados$resultadoTeste <- str_to_upper(dados$resultadoTeste)
  dados$resultadoTeste <- str_trim(dados$resultadoTeste, side = "both")
  dados$resultadoTeste <- str_squish(dados$resultadoTeste)
  dados$resultadoTeste[dados$resultadoTeste == ""] <- NA
  
  dados$estadoTeste <- as.character(dados$estadoTeste)
  dados$estadoTeste <- rm_accent(dados$estadoTeste)
  dados$estadoTeste <- str_replace_all(dados$estadoTeste, "[^[:alpha:]]", " ")
  dados$estadoTeste <- str_to_upper(dados$estadoTeste)
  dados$estadoTeste <- str_trim(dados$estadoTeste, side = "both")
  dados$estadoTeste <- str_squish(dados$estadoTeste)
  dados$estadoTeste[dados$estadoTeste == "SOLICITADO"] <- "1-Solicitado"
  dados$estadoTeste[dados$estadoTeste == "CONCLUIDO"] <- "2-Concluido"
  dados$estadoTeste[dados$estadoTeste == "COLETADO"] <- "3-Coletado"
  dados$estadoTeste[dados$estadoTeste == "EXAME NAO SOLICITADO"] <- "4-Nao Solicitado"
  dados$estadoTeste[dados$estadoTeste == ""] <- NA
  
  dados$tipoTeste <- as.character(dados$tipoTeste)
  dados$tipoTeste <- rm_accent(dados$tipoTeste)
  dados$tipoTeste <- str_replace_all(dados$tipoTeste, "[^[:alpha:]]", " ")
  dados$tipoTeste <- str_to_upper(dados$tipoTeste)
  dados$tipoTeste <- str_trim(dados$tipoTeste, side = "both")
  dados$tipoTeste <- str_squish(dados$tipoTeste)
  dados$tipoTeste[dados$tipoTeste == "RT PCR"] <- "1-RT_PCR"
  dados$tipoTeste[dados$tipoTeste == "TESTE RAPIDO ANTICORPO"] <- "2-TR_Anticorpo"
  dados$tipoTeste[dados$tipoTeste == "TESTE RAPIDO ANTIGENO"] <- "3-TR_Antigeno"
  dados$tipoTeste[dados$tipoTeste == "ENZIMAIMUNOENSAIO ELISA IGM"] <- "4-ELISA"
  dados$tipoTeste[dados$tipoTeste == "ENZIMAIMUNOENSAIO ELISA"] <- "4-ELISA"
  dados$tipoTeste[dados$tipoTeste == "IMUNOENSAIO POR ELETROQUIMIOLUMINESCENCIA ECLIA IGG"] <- "5-ECLISA"
  dados$tipoTeste[dados$tipoTeste == "IMUNOENSAIO POR ELETROQUIMIOLUMINESCENCIA ECLIA"] <- "5-ECLISA"
  dados$tipoTeste[dados$tipoTeste == "QUIMIOLUMINESCENCIA CLIA"] <- "6-CLIA"
  dados$tipoTeste[dados$tipoTeste == ""] <- NA
  
  dados$classificacaoFinal <- as.character(dados$classificacaoFinal)
  dados$classificacaoFinal <- rm_accent(dados$classificacaoFinal)
  dados$classificacaoFinal <- str_replace_all(dados$classificacaoFinal, "[^[:alpha:]]", " ")
  dados$classificacaoFinal <- str_to_upper(dados$classificacaoFinal)
  dados$classificacaoFinal <- str_trim(dados$classificacaoFinal, side = "both")
  dados$classificacaoFinal <- str_squish(dados$classificacaoFinal)
  dados$classificacaoFinal[dados$classificacaoFinal == ""] <- NA
  dados$classificacaoFinal <- recode(dados$classificacaoFinal,
                                     "DESCARTADO" = "1-Descartado", "CONFIRMACAO LABORATORIAL" = "2-Confirmado Laboratorial",
                                     "CONFIRMADO LABORATORIAL" = "2-Confirmado Laboratorial", "CONFIRMACAO CLINICO EPIDEMIOLOGICO" = "3-Confirmado Clinico-Epidemiologico",
                                     "CONFIRMADO CLINICO EPIDEMIOLOGICO" = "3-Confirmado Clinico-Epidemiologico", "CONFIRMADO CLINICO IMAGEM" = "4-Confirmado Clinico-Imagem",
                                     "CONFIRMACAO CLINICO IMAGEM" = "4-Confirmado Clinico-Imagem", "CONFIRMACAO CLINICO" = "5-Confirmado Clinico",
                                     "CONFIRMADO CLINICO" = "5-Confirmado Clinico", "CONFIRMADO POR CRITERIO CLINICO" = "5-Confirmado Clinico",
                                     "SINDROME GRIPAL NAO ESPECIFICADA" = "6-SG Nao Especificada")
  
  dados$evolucaoCaso <- as.character(dados$evolucaoCaso)
  dados$evolucaoCaso <- rm_accent(dados$evolucaoCaso)
  dados$evolucaoCaso <- str_replace_all(dados$evolucaoCaso, "[^[:alpha:]]", " ")
  dados$evolucaoCaso <- str_to_upper(dados$evolucaoCaso)
  dados$evolucaoCaso <- str_trim(dados$evolucaoCaso, side = "both")
  dados$evolucaoCaso <- str_squish(dados$evolucaoCaso)
  dados$evolucaoCaso <- recode(dados$evolucaoCaso,
                               "CURA" = "1-Cura", "OBITO" = "2-Obito", "EM TRATAMENTO DOMICILIAR" = "3-Em tratamento domiciliar",
                               "INTERNADO" = "4-Internado", "INTERNADO EM UTI" = "5-Internado em UTI", "IGNORADO" = "6-Ignorado",
                               "CANCELADO" = "7-Cancelado")
  dados$evolucaoCaso[dados$evolucaoCaso == ""] <- NA
  
  dados <- dados %>% mutate(
    resultadoTeste = case_when(
      classificacaoFinal == "2-Confirmado Laboratorial" ~ "POSITIVO",
      TRUE ~ resultadoTeste))
  
  dados <- dados %>% mutate(
    estadoTeste = case_when(
      !is.na(resultadoTeste) ~ "2-Concluido",
      is.na(resultadoTeste) & !is.na(dataTeste) ~ "3-Coletado",
      TRUE ~ estadoTeste))
  
  dados <- dados %>% mutate(
    classificacaoFinal = case_when(
      resultadoTeste == "POSITIVO" ~ "2-Confirmado Laboratorial",
      resultadoTeste != "POSITIVO" & classificacaoFinal %in% c("2-Confirmado Laboratorial", "Confirmado Clinico-Imagem", "5-Confirmado Clinico") ~ "3-Confirmado Clinico-Epidemiologico",
      resultadoTeste == "NEGATIVO" & is.na(classificacaoFinal) ~ "1-Descartado",
      classificacaoFinal == "6-SG Nao Especificada" ~ "6-SG Nao Especificada",
      TRUE ~ classificacaoFinal))
  
  dados <- dados %>% mutate_at(c("cpf"), ~abjutils::clean_id(.))
  
  return(dados)}

###############################################################################################################
###################################### Checando duplicatas no banco ###########################################
###############################################################################################################

# Função para remover duplicatas e reinfecções
remove_duplicatas <- function(dados) {
  dados$X_updated_at <- as.Date(dados$X_updated_at, format="%Y-%m-%d")
  dados$dataInicioSintomas <- as.Date(dados$dataInicioSintomas, format="%Y-%m-%d")
  dados$dataNotificacao <- as.Date(dados$dataNotificacao, format="%Y-%m-%d")
  
  # Checar as duplicatas segundo a Numero de notificacao, Data de inicio de sintomas  
  dados_duplicados <- dados %>%        
    group_by(cpf, dataNascimento, nomeCompleto, nomeMae, numeroNotificacao, dataInicioSintomas) %>% 
    mutate(duplicidade = n()) %>%                     
    ungroup()  
  
  # Banco de dados com apenas a duplicação ou mais 
  Duplicados <- dados_duplicados %>% 
    filter(duplicidade > 1)
  
  # Banco de dados com apenas dados únicos 
  dados_unicos <- dados_duplicados %>% 
    filter(duplicidade == 1)
  
  # Verificar reinfecções entre as duplicatas 
  duplicados_reinf <- Duplicados %>%
    mutate(dif_dias = as.numeric(difftime(dataInicioSintomas, lag(dataInicioSintomas), units = "days"))) %>%
    mutate(reinfecao = if_else(dif_dias >= 90, 1, 0)) %>%
    filter(dif_dias >= 90) %>%
    ungroup()
  table(duplicados_reinf$reinfecao)
  
  # Ordenar os dados
  dados <- dados %>%
    arrange(cpf, dataNascimento, nomeCompleto, nomeMae, numeroNotificacao, dataInicioSintomas)
  
  # Remover duplicatas considerando reinfecções
  dados_limpos <- dados %>%
    group_by(cpf, nomeCompleto, nomeMae, numeroNotificacao) %>%
    mutate(dif_dias = as.numeric(difftime(dataInicioSintomas, lag(dataInicioSintomas), units = "days"))) %>%
    slice_max(order_by = X_updated_at, n = 1, with_ties = FALSE) %>%
    filter(is.na(dif_dias) | dif_dias >= 90) %>%
    ungroup()
  
  # Para filtrar somente os casos de reinfecção
  dados_reinfeccao <- dados_limpos %>%
    filter(dif_dias >= 90)
  
  return(dados_limpos)}


###################################################################################################

#### Ler e Limpar os Arquivos de Cada Estado

estados <- c("ac", "al", "am", "ap", "ba", "ce", "df", "es", "go", "ma", "mg", "ms", "mt", "pa", "pb", "pe", "pi", "pr", 
             "rj", "rn", "ro", "rr", "rs", "sc", "se", "sp", "to")
dados_list <- list()

for (estado in estados) {
  file_path <- paste0("2022_esus-notifica-estado-", estado, ".csv")  ### Actualizar o ano 
  if (file.exists(file_path)) {
    dados_estado <- fread(file_path)
    dados_estado <- clean_data(dados_estado)
    dados_estado <- dados_estado %>% filter(resultadoTeste == "POSITIVO")
    write.csv(dados_estado, paste0("dados_", estado, ".csv"), row.names = FALSE)
    dados_list[[estado]] <- dados_estado }}

# Função para garantir que todas as colunas sejam do tipo character
convert_columns_to_character <- function(df) { df %>% mutate(across(everything(), as.character))}

# Aplicar a função a todos os data frames da lista
dados_list <- lapply(dados_list, convert_columns_to_character)

##### Fazer o merge dos data frames dos estados  
dados <- bind_rows(dados_list)

##### Aplicar a função remoção duplicatas   
dados_limpos_2022 <- remove_duplicatas(dados)   ### Actualizar o ano 



#### Exportando os bancos normalizados e limpos 
# Definir novo diretório
novo_diretorio <- "C:/Users/audencio.victor/OneDrive - Ministério da Saúde/e-SUS_Plinio/Dados_Sem_Tratamento/esus_notifica_22/Dados limpos"  ## Atualizar o ano 
dir.create(novo_diretorio, showWarnings = FALSE)

caminho_arquivo_normalizado <- file.path(novo_diretorio, "dados_normalizados.csv")
caminho_arquivo_limpos <- file.path(novo_diretorio, "dados_sem_dupli_2022.csv")  ### Atualizar o ano 

write.csv(dados, caminho_arquivo_normalizado, row.names = FALSE)
write.csv(dados_limpos_2022, caminho_arquivo_limpos, row.names = FALSE)  ### Atualizar o ano 


###############################################################################################################
############################################# Fim do script completo ##########################################
###############################################################################################################

