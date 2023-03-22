library(tidyverse)
library(readxl)
library(writexl)
library(stats)

###DADOS DE TRANSFERÊNCIAS - 2018 - SÉRIES A E B

#CARREGAR LISTAS DE TRANSFERÊNCIAS
transf_seriea_18 <- read_excel("tm_transferencias_seriea_2018.xlsx")

transf_serieb_18 <- read_excel("tm_transferencias_serieb_2018.xlsx")

#CARREGAR BIOGRAFIA DOS JOGADORES
jogadores_seriea_18 <- read_excel("tm_jogadores_seriea_2018.xlsx")

jogadores_serieb_18 <- read_excel("tm_jogadores_serieb_2018.xlsx")

#SUBSTITUIR NA'S DA COLUNA FULL_NAME COM DADOS DA COLUNA NAME_IN_HOME_COUNTRY
jogadores_seriea_18$full_name <- ifelse(is.na(jogadores_seriea_18$full_name), 
                                        jogadores_seriea_18$name_in_home_country, 
                                        jogadores_seriea_18$full_name)

jogadores_serieb_18$full_name <- ifelse(is.na(jogadores_serieb_18$full_name), 
                                        jogadores_serieb_18$name_in_home_country, 
                                        jogadores_serieb_18$full_name)

#SELECIONAR O NOME DO JOGADOR NA COLUNA PLAYER_NAME
jogadores_seriea_18$player_name <- word(jogadores_seriea_18$player_name, -2, -1)

jogadores_serieb_18$player_name <- word(jogadores_serieb_18$player_name, -2, -1)

#EXCLUIR ESPAÇOS NO INÍCIO DO NOME DOS JOGADORES
jogadores_seriea_18$player_name <- str_trim(jogadores_seriea_18$player_name, side = "both")

jogadores_serieb_18$player_name <- str_trim(jogadores_serieb_18$player_name, side = "both")

#SUBSTITUIR NA'S DA COLUNA FULL_NAME COM DADOS DA COLUNA PLAYER_NAME
jogadores_seriea_18 <- jogadores_seriea_18 %>% mutate(full_name = ifelse(is.na(jogadores_seriea_18$full_name), jogadores_seriea_18$player_name, jogadores_seriea_18$full_name)) %>%
  select(player_name, full_name)

jogadores_serieb_18 <- jogadores_serieb_18 %>% mutate(full_name = ifelse(is.na(jogadores_serieb_18$full_name), jogadores_serieb_18$player_name, jogadores_serieb_18$full_name)) %>%
  select(player_name, full_name)

#SELECIONAR A COMPETIÇÃO NA COLUNA LEAGUE DO BANCO DE TRANSFERÊNCIAS
transf_seriea_18$league <- word(transf_seriea_18$league, -2, -1)

transf_serieb_18$league <- word(transf_serieb_18$league, -2, -1)

###WRANGLING SÉRIE A 2018
#JOIN DOS BANCOS DE TRANSFERÊNCIAS E DE NOMES COMPLETOS
transf_seriea_18 <- left_join(transf_seriea_18,jogadores_seriea_18)

#ELIMINAÇÃO DE DUPLICATAS NA COLUNA FULL_NAMES
transf_seriea_18 <- distinct(transf_seriea_18, full_name, .keep_all = TRUE)

#CONSOLIDAÇÃO DADOS TEMPORADA 2018 SÉRIE A
#REDUÇÃO DE 839 PARA 106 OBSERVAÇÕES QUE ATENDEM AOS FILTROS
brasil_18_seriea <- transf_seriea_18 %>%
  select(player_name, full_name, player_age, player_position, league,
         season, transfer_type, is_loan, transfer_fee, 
         window, team_name, club_2, league_2, country_2) %>%
  filter(is_loan == FALSE & transfer_fee >= 0 & transfer_type == "Departures") %>%
  rename(Jogador = player_name,
         Nome = full_name,
         Idade = player_age,
         Posicao = player_position,
         Liga = league,
         Temporada = season,
         Janela = window,
         Valor = transfer_fee,
         Time = team_name,
         Comprador = club_2,
         Pais_Comprador = country_2) %>%
  select(Jogador, Nome, Idade, Posicao, Liga, Temporada, 
         Janela, Valor, Time, Comprador, Pais_Comprador)

###WRANGLING SÉRIE B 2018
#JOIN DOS BANCOS DE TRANSFERÊNCIAS E DE NOMES COMPLETOS
transf_serieb_18 <- left_join(transf_serieb_18,jogadores_serieb_18)

#ELIMINAÇÃO DE DUPLICATAS NA COLUNA FULL_NAMES
transf_serieb_18 <- distinct(transf_serieb_18, full_name, .keep_all = TRUE)

#CONSOLIDAÇÃO DADOS TEMPORADA 2018 SÉRIE B
#REDUÇÃO DE 1057 PARA 102 OBSERVAÇÕES QUE ATENDEM AOS FILTROS
brasil_18_serieb <- transf_serieb_18 %>%
  select(player_name, full_name, player_age, player_position, league,
         season, transfer_type, is_loan, transfer_fee, 
         window, team_name, club_2, league_2, country_2) %>%
  filter(is_loan == FALSE & transfer_fee >= 0 & transfer_type == "Departures") %>%
  rename(Jogador = player_name,
         Nome = full_name,
         Idade = player_age,
         Posicao = player_position,
         Liga = league,
         Temporada = season,
         Janela = window,
         Valor = transfer_fee,
         Time = team_name,
         Comprador = club_2,
         Pais_Comprador = country_2) %>%
  select(Jogador, Nome, Idade, Posicao, Liga, Temporada, 
         Janela, Valor, Time, Comprador, Pais_Comprador)

#BIND ROWS DAS TRANSFERÊNCIAS COM NOMES COMPLETOS - SÉRIES A E B - 208 OBSERVAÇÕES
brasil_18_consolidado <- bind_rows(brasil_18_seriea, brasil_18_serieb)

#AJUSTES DAS POSIÇÕES DOS JOGADORES E TRADUÇÃO DAS JANELAS
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% "Attacking Midfield"] <- 'Meia-Atacante'
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% "Centre-Forward"] <- 'Centroavante'
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% "Centre-Back"] <- 'Zagueiro'
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% "Right Winger"] <- 'Extremo-Direito'
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% "Left Winger"] <- 'Extremo-Esquerdo'
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% "Left-Back"] <- 'Lateral-Esquerdo'
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% c("Central Midfield", "midfield")] <- 'Meia'
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% "Defensive Midfield"] <- 'Volante'
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% "Right-Back"] <- 'Lateral-Direito'
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% c("Second Striker", "attack")] <- 'Atacante'
brasil_18_consolidado$Posicao[brasil_18_consolidado$Posicao %in% "Goalkeeper"] <- 'Goleiro'
brasil_18_consolidado$Janela[brasil_18_consolidado$Janela %in% "Summer"] <- "Verão"
brasil_18_consolidado$Janela[brasil_18_consolidado$Janela %in% "Winter"] <- "Inverno"

#AJUSTES DOS NOMES DOS 39 TIMES
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Sociedade Esportiva Palmeiras"] <- 'Palmeiras'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Santos FC"] <- 'Santos'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "São Paulo Futebol Clube"] <- 'São Paulo'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Clube de Regatas do Flamengo"] <- 'Flamengo'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Grêmio Foot-Ball Porto Alegrense"] <- 'Grêmio'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Cruzeiro Esporte Clube"] <- 'Cruzeiro'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Clube Atlético Mineiro"] <- 'Atlético-MG'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Sport Club Corinthians Paulista"] <- 'Corinthians'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Sport Club Internacional"] <- 'Internacional'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Centro Sportivo Alagoano (AL)"] <- 'CSA'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Club de Regatas Vasco da Gama"] <- 'Vasco'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Club Athletico Paranaense"] <- 'Athletico'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Esporte Clube Bahia"] <- 'Bahia'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Associação Chapecoense de Futebol"] <- 'Chapecoense'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Fluminense Football Club"] <- 'Fluminense'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Fortaleza Esporte Clube"] <- 'Fortaleza'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Ceará Sporting Club"] <- 'Ceará'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Botafogo de Futebol e Regatas"] <- 'Botafogo'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Goiás Esporte Clube"] <- 'Goiás'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Avaí Futebol Clube (SC)"] <- 'Avaí'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Sport Club do Recife"] <- 'Sport'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Coritiba Foot Ball Club"] <- 'Coritiba'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Esporte Clube Vitória"] <- 'Vitória'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Guarani Futebol Clube (SP)"] <- 'Guarani'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Associação Atlética Ponte Preta"] <- 'Ponte Preta'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Esporte Clube São Bento (SP)"] <- 'São Bento'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Clube de Regatas Brasil (AL)"] <- 'CRB'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Vila Nova Futebol Clube (GO)"] <- 'Vila Nova'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Londrina Esporte Clube (PR)"] <- 'Londrina'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "América Futebol Clube (MG)"] <- 'América-MG'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Figueirense Futebol Clube"] <- 'Figueirense'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Botafogo Futebol Clube (SP)"] <- 'Botafogo-SP'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Paraná Clube"] <- 'Paraná'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Red Bull Bragantino"] <- 'RB Bragantino'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Criciúma Esporte Clube"] <- 'Criciúma'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Oeste Futebol Clube (SP)"] <- 'Oeste'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Cuiabá Esporte Clube (MT)"] <- 'Cuiabá'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Atlético Clube Goianiense"] <- 'Atlético-GO'
brasil_18_consolidado$Time[brasil_18_consolidado$Time %in% "Grêmio Esportivo Brasil (RS)"] <- 'Brasil-RS'

#CORREÇÃO DE REGISTROS DO TRANSFERMARKT - EXCLUSÃO DE DUPLICATAS NA VARIÁVEL JOGADOR
#185 OBSERVAÇÕES
brasil_18_consolidado <- brasil_18_consolidado %>% 
  distinct(Jogador, .keep_all = TRUE) 

###DADOS DE DESEMPENHO - WYSCOUT - SÉRIES A E B
#IMPORTAR DADOS
wyscout_seriea_18 <- read_excel("wyscout_seriea_2018.xlsx")

wyscout_serieb_18 <- read_excel("wyscout_serieb_2018.xlsx")

#BIND ROWS SÉRIES A E B
#1528 OBSERVAÇÕES
wyscout_2018 <- bind_rows(wyscout_seriea_18, wyscout_serieb_18)

#WRANGLING DADOS DE DESEMPENHO (AJUSTE DE NOMES DE VARIÁVEIS)
wyscout_2018 <- wyscout_2018 %>% 
  rename(Jogador = player_name,
         Time = team_name,
         Jogos = `Matches played`,
         Minutos = `Minutes played`,
         Gols = Goals,
         Gols_Cabeca = `Head goals`,
         Gols_sem_Penalti = `Non-penalty goals`,
         xG_media = `xG per 90`,
         Conversao = `Goal conversion, %`,
         Acoes_Ataque_media = `Successful attacking actions per 90`,
         Toques_Area_media = `Touches in box per 90`,
         Conducoes_media = `Progressive runs per 90`,
         Faltas_Recebidas_media = `Fouls suffered per 90`,
         Assistencias = Assists,
         Passes_Chave_media = `Shot assists per 90`,
         Pre_Assistencia_media = `Second assists per 90`,
         Preparacao = `Third assists per 90`,
         Passes_Profundidade_media = `Deep completions per 90`,
         Distância_passes = `Average pass length, m`,
         Passes_Recebidos_media = `Received passes per 90`,
         Acoes_Defesa_media = `Successful defensive actions per 90`,
         Carrinhos_media = `Sliding tackles per 90`,
         Bloqueios_media = `Shots blocked per 90`,
         Interceptacoes_media = `Interceptions per 90`,
         Faltas_Cometidas_media = `Fouls per 90`,
         Amarelos = `Yellow cards`,
         Vermelhos = `Red cards`,
         Gols_Concedidos = `Conceded goals`,
         Gols_Concedidos_media = `Conceded goals per 90`,
         Gols_Evitados_media = `Prevented goals per 90`,
         Jogos_Invicto = `Clean sheets`,
         Percentual_Defesas = `Save rate, %`,
         Passes_Recebidos_Goleiro_media = `Back passes received as GK per 90`) %>%
  mutate(Minutos_media = Minutos / Jogos,
         Chutes_media = (`Shots per 90` * `Shots on target, %`) / 100,
         Cruzamentos_media = (`Crosses per 90` * `Accurate crosses, %`) / 100,
         Dribles_media = (`Dribbles per 90` * `Successful dribbles, %`) / 100,
         Passes_media = (`Passes per 90` * `Accurate passes, %`) / 100,
         Passes_Frente_media = (`Forward passes per 90` * 
                                  `Accurate forward passes, %`) / 100,
         Passes_Terco_Final_media = (`Passes to final third per 90` * 
                                       `Accurate passes to final third, %`) / 100,
         Passes_Area_media = (`Passes to penalty area per 90` * 
                                `Accurate passes to penalty area, %`) / 100,
         Passes_Progressivos_media = (`Progressive passes per 90` * 
                                        `Accurate progressive passes, %`) / 100,
         Duelos_Ataque_media = (`Offensive duels per 90` * 
                                  `Offensive duels won, %`) /  100,
         Duelos_Defesa_media = (`Defensive duels per 90` * 
                                  `Defensive duels won, %`) /  100,
         Duelos_Aereos_media = (`Aerial duels per 90` * 
                                  `Aerial duels won, %`) / 100) %>%
  select(Jogador, Time, Jogos, Minutos, Minutos_media, Gols,
         Gols_Cabeca, Gols_sem_Penalti, xG, xG_media, Conversao, Chutes_media,
         Acoes_Ataque_media, Toques_Area_media, Conducoes_media, Dribles_media, 
         Duelos_Ataque_media, Faltas_Recebidas_media, Assistencias, 
         Passes_Chave_media, Pre_Assistencia_media, Preparacao, Passes_media, 
         Passes_Recebidos_media, Distância_passes, Passes_Frente_media, 
         Passes_Profundidade_media, Passes_Progressivos_media, Passes_Terco_Final_media, 
         Passes_Area_media, Cruzamentos_media, Acoes_Defesa_media, Duelos_Defesa_media, 
         Duelos_Aereos_media, Carrinhos_media, Bloqueios_media, Interceptacoes_media,
         Faltas_Cometidas_media, Amarelos, Vermelhos, Gols_Concedidos,
         Gols_Concedidos_media, Gols_Evitados_media, Jogos_Invicto,
         Percentual_Defesas, Passes_Recebidos_Goleiro_media) %>%
  mutate_if(is.numeric, round, 2)

#FUSÃO DOS DOIS DATA FRAMES - DIVERSOS PROBLEMAS COM DIFERENTES GRAFIAS DE NOMES E/OU CLUBES TROCADOS
brasil_18_merge <- merge(brasil_18_consolidado, wyscout_2018, all = TRUE)

# AJUSTE MANUAL DE 41 GRAFIAS DE NOMES DE JOGADORES OU DE TIMES DE PROCEDÊNCIA NA BASE DO WYSCOUT
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Anderson' & wyscout_2018$Time %in% 'Bahia', 'Feijão', wyscout_2018$Jogador)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Agenor', 'Sport', wyscout_2018$Time)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'A. Doffo', 'Agustín Doffo', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'A. Martinuccio', 'Alejandro Martinuccio', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'A. Baumjohann', 'Alexander Baumjohann', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'A. Ríos', 'Andrés Ríos', wyscout_2018$Jogador)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Anselmo', 'Internacional', wyscout_2018$Time)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Arthur' & wyscout_2018$Time %in% 'Grêmio', 'Arthur Melo', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Bergson', 'Bérgson', wyscout_2018$Jogador)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Bruno' & wyscout_2018$Time %in% 'Bahia', 'São Paulo', wyscout_2018$Time)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'C. Cueva', 'Christian Cueva', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Diego' & wyscout_2018$Time %in% 'CRB', 'Diego Corrêa', wyscout_2018$Jogador)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Diones' & wyscout_2018$Time %in% 'Juventude', 'Botafogo-SP', wyscout_2018$Time)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'D. Riascos', 'Duvier Riascos', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'E. Echeverría', 'Eduardo Echeverría', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Emerson' & wyscout_2018$Time %in% 'Atlético-MG', 'Emerson Royal', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'E. Pavez', 'Esteban Pavez', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'E. Mena', 'Eugenio Mena', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'F. Balbuena', 'Fabián Balbuena', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'F. Mancuello', 'Federico Mancuello', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'G. Beltrán', 'Guillermo Beltrán', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'H. Dourado', 'Henrique Dourado', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'J. Reina', 'Javier Reina', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'J. Cardona', 'Jown Cardona', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Juninho Brandao', 'Junior Brandão', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'L. Desábato', 'Leandro Desábato', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Lucas Paquetà', 'Lucas Paquetá', wyscout_2018$Jogador)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Lúcio Flávio', 'São Bento', wyscout_2018$Time)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Maranhão', 'Fluminense', wyscout_2018$Time)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Márcio' & wyscout_2018$Time %in% 'CRB', 'Márcio Passos', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'M. Silva', 'Martín Silva', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Kozlinski', 'Mauricio Kozlinski', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Nilton', 'Nílton', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'P. Guerrero', 'Paolo Guerrero', wyscout_2018$Jogador)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Rodrigo Sam', 'Corinthians', wyscout_2018$Time)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Ibañez', 'Roger Ibañez', wyscout_2018$Jogador)
wyscout_2018$Time <- ifelse(wyscout_2018$Jogador %in% 'Thiago Ribeiro', 'Guarani', wyscout_2018$Time)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Thomas', 'Thomás', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'V. Cedrón', 'Víctor Cedrón', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Vinícius Júnior', 'Vinicius Junior', wyscout_2018$Jogador)
wyscout_2018$Jogador <- ifelse(wyscout_2018$Jogador %in% 'Willians Santana', 'Willians', wyscout_2018$Jogador)

#REPETE A FUSÃO DOS DOIS DATA FRAMES APÓS AS CORREÇÕES
brasil_18_merge <- merge(brasil_18_consolidado, wyscout_2018, all = TRUE)

#EXCLUSÃO DE OBSERVAÇÕES COM NA'S EM JOGOS & VALOR = 0
#SUBSTITUIÇÃO DE NA'S POR 0 PARA JOGADORES COM VALOR > 0
#6 OBSERVAÇÕES
brasil_18_sobras <- brasil_18_merge %>% 
  filter(!is.na(Nome) & is.na(Jogos) & Valor > 0) %>%
  mutate(across(12:55, replace_na, 0))

#EXCLUSÃO DE NA's NAS VARIÁVEIS NOME E JOGOS
#EXCLUSÃO DE DUPLICATAS NA VARIÁVEIS NOME E JOGOS
#130 OBSERVAÇÕES
brasil_18_merge <- brasil_18_merge %>% 
  filter(!is.na(Nome) & !is.na(Jogos)) %>% 
  distinct(Nome, .keep_all = TRUE) %>% 
  distinct(Jogador, .keep_all = TRUE) 

#BIND ROWS DOS BANCOS DE TRANSFERÊNCIAS 2019/DESEMPENHO 2019 E TRANSFERÊNCIAS 2019/DESEMPENHO 2018
brasil_transferencias_2018 <- bind_rows(brasil_18_merge, brasil_18_sobras)

#CONSOLIDAÇÃO DO DATA FRAME
#136 OBSERVAÇÕES SÉRIES A E B 2018
#41 DAS 136 COM VALOR DE TRANSFERÊNCIA > 0
brasil_transferencias_2018 <- as.data.frame(brasil_transferencias_2018)

#EXPORTAÇÃO PARA ARQUIVO XSML
write_xlsx(brasil_transferencias_2018, "brasil_transferencias_2018.xlsx")
