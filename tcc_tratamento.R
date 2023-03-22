library(tidyverse)
library(readxl)
library(writexl)
library(stats)
library(reshape2)
library(scales)
library(cluster)
library(factoextra)
library(gridExtra)
library(fastDummies)

#SET.SEED PARA GARANTIR REPRODUÇÃO DOS RESULTADOS
set.seed(23)

#FUNÇÃO PARA EVITAR NOTAÇÃO CIENTÍFICA
options(scipen=999)

###CARREGAR ARQUIVOS - TRANSFERÊNCIAS 2018-2022 E DOIS RESUMOS (POR TIME E POR PAÍS COMPRADOR)
jogadores <- read_excel("jogadores.xlsx")

resumo_transferencias_time <- read_excel("resumo_transferencias_time.xlsx")

resumo_transferencias_comprador <- read_excel("resumo_transferencias_comprador.xlsx")

###CLUSTERIZAÇÃO DA VARIÁVEL TIME PARA CRIAÇÃO DE DUMMYS
###AGRUPAMENTO BASEADO EM DUAS VARIÁVEIS - VALOR TOTAL ARRECADADO E CONTAGEM DE TRANSFERÊNCIAS

#PADRONIZAÇÃO DAS VARIÁVEIS NUMÉRICAS
times_padronizado <- resumo_transferencias_time

times_padronizado[,2:4] <- apply(times_padronizado[,2:4], 2, scale)

#DEFINIÇÃO DA VARIÁVEL "TIME" COMO ROWNAMES PARA CRIAR RÓTULOS NO GRÁFICO
times_padronizado <- column_to_rownames(times_padronizado, var = "Time")

#FORMAÇÃO DE SEIS AGRUPAMENTOS APÓS TESTES COM 3, 4 E 5 AGRUPAMENTOS
clusters_times <- kmeans(times_padronizado, centers = 6, nstart = 25, iter.max = 100)

#GRÁFICO DO RESULTADO
fviz_cluster(clusters_times, ggtheme = theme_minimal(), repel = TRUE,
             data = times_padronizado)

#RESULTADO CONSOLIDADO
cluster_times_final <- resumo_transferencias_time %>% 
  mutate(Cluster = clusters_times$cluster) %>% 
  dplyr::select(everything())

#MÉDIAS PARA IDENTIFICAÇÃO DAS CARACTERÍSTICAS DE CADA CLUSTER
cluster_times_medias <- cluster_times_final %>% 
  group_by(Cluster) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate_if(is.numeric, round, 2)

#TOTAIS PARA IDENTIFICAÇÃO DAS CARACTERÍSTICAS DE CADA CLUSTER
cluster_times_totais <- cluster_times_final %>% 
 group_by(Cluster) %>% 
  dplyr::select(1:3, 5) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate_if(is.numeric, round, 2)

###CARACTERÍSTICAS DOS CLUSTERS:
#VISUALIZAÇÃO DA TABELA DE CONTAGEM
table(cluster_times_final$Cluster)

##1: 13/49 observações; 
#3ºs menores valor e volume médios
  # Botafogo, Coritiba, Ceará, Vitória, Ponte Preta, Atlético-GO, Sport,
  # América-MG, Guarani, Goiás, Avaí, Brasil-RS, Sampaio Corrêa

##2: 7/49 observações; 
#Menores volume e valor (tanto total como médio); 2ª menor média de idade
  # Paraná, Londrina, Confiança, Ituano, Novorizontino, Oeste, Tombense

##3: 9/49 observações; 
#Maior valor total (2º maior valor médio); 3º maior volume (total e média);
#3ª menor média de idade
  # Santos, Grêmio, Athletico, Internacional, Corinthians, São Paulo,
  # Atlético-MG, Fluminense, Vasco

##4: 6/49 observações; 
#Maior volume médio (2º maior total); 3º maior valor médio; 2ª maior média de idade
  # Cruzeiro, Bahia, Chapecoense, Cuiabá, CRB, CSA

##5: 12/49 observações; 
#2º menor valor (total e média); 2º menor volume médio; maior média de idade
  # RB Bragantino, Botafogo-SP, Brusque, Criciúma, Figueirense, Fortaleza,
  # Juventude, Náutico, Operário-PR, Remo, São Bento, Vila Nova

##6: 2/49 observações; 
#Maior valor médio (2º maior total); 2º maior volume médio; menor média de idade
  # Flamengo, Palmeiras

###CLUSTERIZAÇÃO DA VARIÁVEL PAÍS COMPRADOR PARA CRIAÇÃO DE DUMMYS
###AGRUPAMENTO INICIAL BASEADO EM DUAS VARIÁVEIS - VALOR TOTAL ARRECADADO E CONTAGEM DE TRANSFERÊNCIAS

#PADRONIZAÇÃO DAS VARIÁVEIS NUMÉRICAS
comprador_padronizado <- resumo_transferencias_comprador %>% 
  dplyr::select(`País`, `Valor Total`, `Total de Transferências`, `Média de Idade`)

comprador_padronizado[, 2:4] <- apply(comprador_padronizado[,2:4], 2, scale)

#DEFINIÇÃO DA VARIÁVEL "TIME" COMO ROWNAMES PARA CRIAR RÓTULOS NO GRÁFICO
comprador_padronizado <- column_to_rownames(comprador_padronizado, var = "País")

#FORMAÇÃO DE SEIS AGRUPAMENTOS
clusters_comprador <- kmeans(comprador_padronizado, centers = 6, nstart = 25, iter.max = 100)

#GRÁFICO DO RESULTADO
fviz_cluster(clusters_comprador, ggtheme = theme_minimal(), repel = TRUE,
             data = comprador_padronizado)

#RESULTADO CONSOLIDADO
cluster_comprador_final <- resumo_transferencias_comprador %>% 
  mutate(Cluster = clusters_comprador$cluster) %>% 
  dplyr::select(everything())

#MÉDIAS PARA IDENTIFICAÇÃO DAS CARACTERÍSTICAS DE CADA CLUSTER
cluster_comprador_medias <- cluster_comprador_final %>% 
  group_by(Cluster) %>% 
  dplyr::select(1:3, 6:7) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate_if(is.numeric, round, 2)

#TOTAIS PARA IDENTIFICAÇÃO DAS CARACTERÍSTICAS DE CADA CLUSTER
cluster_comprador_totais <- cluster_comprador_final %>% 
  group_by(Cluster) %>% 
  dplyr::select(1:3, 7) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate_if(is.numeric, round, 2)

###CARACTERÍSTICAS DOS CLUSTERS:
#VISUALIZAÇÃO DA TABELA DE CONTAGEM
table(cluster_comprador_final$Cluster)

##1: 8/56 observações; 
#Menor valor (total e médio); Menor volume médio (2º menor total)
#Maior média de idade
  # Paraguai, Catar, Austrália, Cazaquistão, Hong Kong, Malásia, 
  # Rep. Dominicana, Uruguai

##2: 23/56 observações; 
#2º maior volume total (3º menor médio); 2º menor valor médio (3º maior total)
#3ª maior média de idade
#  Arábia Saudita, Japão, Turquia, Egito, México, China, Argentina, Israel,      
# Emirados Árabes, Chile, Armênia, Azerbaijão, Bahrein, Bolívia, Chipre,         
# Colômbia, Coréia do Sul, Equador, Grécia, India, Lituânia, Malta, Tailândia

##3: 5/56 observações;
#Maior valor total (2º maior médio); 3º maior volume (total e médio); 
#2ª menor média de idade
# Itália, Portugal, França, Inglaterra, Rússia

##4: 1/56 observações; 
#Maior volume (total e médio); 2º menor valor total (3º maior médio)
#2ª maior média de idade
# Brasil

##5: 1/56 observações; 
#Maior valor médio (2º maior total); 2º maior volume médio (menor volume total)
#Menor média de idade
# Espanha

##6: 18/56 observações; 
#3º menor valor (total e médio); 3º menor volume total (2º menor médio)
#3ª menor média de idade
# Estados Unidos, Holanda, Ucrânia, Canadá, Alemanha, Dinamarca, Bélgica,
# Bulgária, Hungria, Bangladesh, Croácia, Letônia, Moldávia, Nova Zelândia,
# Peru, Polônia, Romênia, Suíça

##########   ARQUIVOS DE REFERÊNCIA COM IDENTIFICAÇÃO DOS CLUSTERS #############

#IDENTIFICAÇÃO DOS CLUSTERS POR TIME VENDEDOR
times_clusters <- cluster_times_final %>% 
  dplyr::select(Time, Cluster) %>% 
  rename(Cluster_Vendedor = Cluster)

#INCLUSÃO DA VARIÁVEL CLUSTER_VENDEDOR NO DATASET
jogadores <- merge(jogadores, times_clusters, by = "Time", all.x = TRUE)

#FATORIZAÇÃO DA VARIÁVEL
jogadores$Cluster_Vendedor <- as.factor(jogadores$Cluster_Vendedor)

#IDENTIFICAÇÃO DOS CLUSTERS POR PAÍS COMPRADOR
compradores_clusters <- cluster_comprador_final %>% 
  dplyr::select(`País`, Cluster) %>% 
  rename(Cluster_Comprador = Cluster,
         Pais_Comprador = `País`)

#INCLUSÃO DA VARIÁVEL CLUSTER_COMPRADOR NO DATASET
jogadores <- merge(jogadores, compradores_clusters, by = "Pais_Comprador", all.x = TRUE)

#FATORIZAÇÃO DA VARIÁVEL
jogadores$Cluster_Comprador <- as.factor(jogadores$Cluster_Comprador)

#ORGANIZAÇÃO DAS VARIÁVEIS
jogadores_clusters <- jogadores %>% 
  dplyr::select(3, 4, 2, 5:8, 1, everything())

#EXPORTAÇÃO PARA ARQUIVO XLSX - JOGADORES - COM CLIUSTERS
write_xlsx(jogadores_clusters, "jogadores_clusters.xlsx")

###############       CRIAÇÃO DAS VARIÁVEIS DUMMIES      #######################

#VISUALIZAÇÕES DAS CATEGORIAS COM OS MAIORES VALORES TOTAIS
#PARA IDENTIFICAR AS CATEGORIAS DE REFERÊNCIA DAS 5 VARIÁVEIS

#POSIÇÃO - 9 DUMMIES PARA 10 CATEGORIAS
  #REFERÊNCIA: EXTREMO_ESQUERDO
jogadores_clusters %>% 
  group_by(Posicao) %>% 
  dplyr::select(Posicao, Valor) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate_if(is.numeric, round, 2) %>% 
  arrange(desc(Valor))

#LIGA - 3 DUMMIES PARA 4 CATEGORIAS
  #REFERÊNCIA: SÉRIE A
jogadores_clusters %>% 
  group_by(Liga) %>% 
  dplyr::select(Liga, Valor) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate_if(is.numeric, round, 2) %>% 
  arrange(desc(Valor))

#JANELA - UMA DUMMY PARA DUAS CATEGORIAS
  #REFERÊNCIA: VERÃO
jogadores_clusters %>% 
  group_by(Janela) %>% 
  dplyr::select(Janela, Valor) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate_if(is.numeric, round, 2) %>% 
  arrange(desc(Valor))

#CLUSTER TIME VENDEDOR - 5 DUMMIES PARA 6 CATEGORIAS
  #REFERÊNCIA: CLUSTER 3 (Santos, Grêmio, Athletico, Internacional, 
                    # Corinthians, São Paulo, Atlético-MG, Fluminense, Vasco)
jogadores_clusters %>% 
  group_by(Cluster_Vendedor) %>% 
  dplyr::select(Cluster_Vendedor, Valor) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate_if(is.numeric, round, 2) %>% 
  arrange(desc(Valor))

#CLUSTER PAÍS COMPRADOR - 5 DUMMIES PARA 6 CATEGORIAS
  #REFERÊNCIA: CLUSTER 3 (Itália, Portugal, Inglaterra, França, Rússia)
jogadores_clusters %>% 
  group_by(Cluster_Comprador) %>% 
  dplyr::select(Cluster_Comprador, Valor) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate_if(is.numeric, round, 2) %>% 
  arrange(desc(Valor))

###CRIAÇÃO DAS VARIÁVEIS DUMMY PARA 5 VARIÁVEIS CATEGÓRICAS
jogadores_dummies <- dummy_columns(.data = jogadores,
                                   select_columns = c("Cluster_Vendedor", 
                                                      "Cluster_Comprador", 
                                                      "Posicao", "Liga", "Janela"),
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = F)

#EXCLUSÃO DAS CATEGORIAS DE REFERÊNCIA
#EXCLUSÃO DE VARIÁVEIS STRING
#60 VARIÁVEIS (861 OBSERVAÇÕES)
jogadores_dummies <- jogadores_dummies %>% 
  dplyr::select(Jogador, Valor, Idade, everything(), 
                -1, -2, -3, -4, -5, -44, -50, -57, -65, -69)

#EXPORTAÇÃO PARA ARQUIVO XLSX - JOGADORES - COM DUMMIES
write_xlsx(jogadores_dummies, "jogadores_dummies.xlsx")
