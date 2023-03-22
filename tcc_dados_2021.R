library(tidyverse)
library(readxl)
library(writexl)
library(stats)

###DADOS DE TRANSFERÊNCIAS - 2021 - SÉRIES A E B

#CARREGAR LISTAS DE TRANSFERÊNCIAS
transf_seriea_21 <- read_excel("tm_transferencias_seriea_2021.xlsx")

transf_serieb_21 <- read_excel("tm_transferencias_serieb_2021.xlsx")

#CARREGAR BIOGRAFIA DOS JOGADORES
jogadores_seriea_21 <- read_excel("tm_jogadores_seriea_2021.xlsx")

jogadores_serieb_21 <- read_excel("tm_jogadores_serieb_2021.xlsx")

#SUBSTITUIR NA'S DA COLUNA FULL_NAME COM DADOS DA COLUNA NAME_IN_HOME_COUNTRY
jogadores_seriea_21$full_name <- ifelse(is.na(jogadores_seriea_21$full_name), 
                                        jogadores_seriea_21$name_in_home_country, 
                                        jogadores_seriea_21$full_name)

jogadores_serieb_21$full_name <- ifelse(is.na(jogadores_serieb_21$full_name), 
                                        jogadores_serieb_21$name_in_home_country, 
                                        jogadores_serieb_21$full_name)

#SELECIONAR O NOME DO JOGADOR NA COLUNA PLAYER_NAME
jogadores_seriea_21$player_name <- word(jogadores_seriea_21$player_name, -2, -1)

jogadores_serieb_21$player_name <- word(jogadores_serieb_21$player_name, -2, -1)

#EXCLUIR ESPAÇOS NO INÍCIO DO NOME DOS JOGADORES
jogadores_seriea_21$player_name <- str_trim(jogadores_seriea_21$player_name, side = "both")

jogadores_serieb_21$player_name <- str_trim(jogadores_serieb_21$player_name, side = "both")

#SUBSTITUIR NA'S DA COLUNA FULL_NAME COM DADOS DA COLUNA PLAYER_NAME
jogadores_seriea_21 <- jogadores_seriea_21 %>% 
  mutate(full_name = ifelse(is.na(jogadores_seriea_21$full_name), 
                            jogadores_seriea_21$player_name, jogadores_seriea_21$full_name)) %>%
  select(player_name, full_name)

jogadores_serieb_21 <- jogadores_serieb_21 %>% 
  mutate(full_name = ifelse(is.na(jogadores_serieb_21$full_name), 
                            jogadores_serieb_21$player_name, jogadores_serieb_21$full_name)) %>%
  select(player_name, full_name)

#SELECIONAR A COMPETIÇÃO NA COLUNA LEAGUE DO BANCO DE TRANSFERÊNCIAS
transf_seriea_21$league <- word(transf_seriea_21$league, -2, -1)

transf_serieb_21$league <- word(transf_serieb_21$league, -2, -1)

###WRANGLING SÉRIE A 2021
#JOIN DOS BANCOS DE TRANSFERÊNCIAS E DE NOMES COMPLETOS
transf_seriea_21 <- left_join(transf_seriea_21,jogadores_seriea_21)

#ELIMINAÇÃO DE DUPLICATAS NA COLUNA FULL_NAMES
transf_seriea_21 <- distinct(transf_seriea_21, full_name, .keep_all = TRUE)

#CONSOLIDAÇÃO DADOS TEMPORADA 2021 SÉRIE A
#REDUÇÃO DE 696 PARA 141 OBSERVAÇÕES QUE ATENDEM AOS FILTROS
brasil_21_seriea <- transf_seriea_21 %>%
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

###WRANGLING SÉRIE B 2021
#JOIN DOS BANCOS DE TRANSFERÊNCIAS E DE NOMES COMPLETOS
transf_serieb_21 <- left_join(transf_serieb_21,jogadores_serieb_21)

#ELIMINAÇÃO DE DUPLICATAS NA COLUNA FULL_NAMES
transf_serieb_21 <- distinct(transf_serieb_21, full_name, .keep_all = TRUE)

#CONSOLIDAÇÃO DADOS TEMPORADA 2021 SÉRIE B
#REDUÇÃO DE 1005 PARA 210 OBSERVAÇÕES QUE ATENDEM AOS FILTROS
brasil_21_serieb <- transf_serieb_21 %>%
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

#BIND ROWS DAS TRANSFERÊNCIAS COM NOMES COMPLETOS - SÉRIES A E B - 351 OBSERVAÇÕES
brasil_21_consolidado <- bind_rows(brasil_21_seriea, brasil_21_serieb)

#AJUSTES DAS POSIÇÕES DOS JOGADORES E TRADUÇÃO DAS JANELAS
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% "Attacking Midfield"] <- 'Meia-Atacante'
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% "Centre-Forward"] <- 'Centroavante'
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% c("Centre-Back", "defence")] <- 'Zagueiro'
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% c("Right Winger", "Right Midfield")] <- 'Extremo-Direito'
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% c("Left Midfield", "Left Winger")] <- 'Extremo-Esquerdo'
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% "Left-Back"] <- 'Lateral-Esquerdo'
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% c("Central Midfield", "midfield")] <- 'Meia'
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% "Defensive Midfield"] <- 'Volante'
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% "Right-Back"] <- 'Lateral-Direito'
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% c("Second Striker", "attack")] <- 'Atacante'
brasil_21_consolidado$Posicao[brasil_21_consolidado$Posicao %in% "Goalkeeper"] <- 'Goleiro'
brasil_21_consolidado$Janela[brasil_21_consolidado$Janela %in% "Summer"] <- "Verão"
brasil_21_consolidado$Janela[brasil_21_consolidado$Janela %in% "Winter"] <- "Inverno"

#AJUSTES DOS NOMES DOS 40 TIMES
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Sociedade Esportiva Palmeiras"] <- 'Palmeiras'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Clube de Regatas do Flamengo"] <- 'Flamengo'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in%  "Sport Club Corinthians Paulista"] <- 'Corinthians'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Grêmio Foot-Ball Porto Alegrense"] <- 'Grêmio'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Sport Club Internacional"] <- 'Internacional'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Clube Atlético Mineiro"] <- 'Atlético-MG'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "São Paulo Futebol Clube"] <- 'São Paulo'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Santos FC"] <- 'Santos'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Fluminense Football Club"] <- 'Fluminense'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Red Bull Bragantino"] <- 'RB Bragantino'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Club Athletico Paranaense"] <- 'Athletico'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Fortaleza Esporte Clube"] <- 'Fortaleza'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Esporte Clube Bahia"] <- 'Bahia'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Sport Club do Recife"] <- 'Sport'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Esporte Clube Juventude"] <- 'Juventude'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Atlético Clube Goianiense"] <- 'Atlético-GO'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Ceará Sporting Club"] <- 'Ceará'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Cuiabá Esporte Clube (MT)"] <- 'Cuiabá'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "América Futebol Clube (MG)"] <- 'América-MG'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Associação Chapecoense de Futebol"] <- 'Chapecoense'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Club de Regatas Vasco da Gama"] <- 'Vasco'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Botafogo de Futebol e Regatas"] <- 'Botafogo'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Cruzeiro Esporte Clube"] <- 'Cruzeiro'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Avaí Futebol Clube (SC)"] <- 'Avaí'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Centro Sportivo Alagoano (AL)"] <- 'CSA'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Coritiba Foot Ball Club"] <- 'Coritiba'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Clube de Regatas Brasil (AL)"] <- 'CRB'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Associação Atlética Ponte Preta"] <- 'Ponte Preta'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Vila Nova Futebol Clube (GO)"] <- 'Vila Nova'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Clube Náutico Capibaribe"] <- 'Náutico'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Guarani Futebol Clube (SP)"] <- 'Guarani'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Ituano Futebol Clube (SP)"] <- 'Ituano'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Goiás Esporte Clube"] <- 'Goiás'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Criciúma Esporte Clube"] <- 'Criciúma'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Tombense Futebol Clube (MG)"] <- 'Tombense'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Londrina Esporte Clube (PR)"] <- 'Londrina'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Operário Ferroviário Esporte Clube (PR)"] <- 'Operário-PR'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Grêmio Novorizontino (SP)"] <- 'Novorizontino'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Sampaio Corrêa Futebol Clube (MA)"] <- 'Sampaio Corrêa'
brasil_21_consolidado$Time[brasil_21_consolidado$Time %in% "Brusque Futebol Clube (SC)"] <- 'Brusque'

#CORREÇÃO DE REGISTROS DO TRANSFERMARKT - EXCLUSÃO DE DUPLICATAS NA VARIÁVEL JOGADOR
#317 OBSERVAÇÕES
brasil_21_consolidado <- brasil_21_consolidado %>% 
  distinct(Jogador, .keep_all = TRUE) 

###DADOS DE DESEMPENHO - WYSCOUT - SÉRIES A E B
#IMPORTAR DADOS
wyscout_seriea_21 <- read_excel("wyscout_seriea_2021.xlsx")

wyscout_serieb_21 <- read_excel("wyscout_serieb_2021.xlsx")

#BIND ROWS SÉRIES A E B
#1530 OBSERVAÇÕES
wyscout_2021 <- bind_rows(wyscout_seriea_21, wyscout_serieb_21)

#WRANGLING DADOS DE DESEMPENHO (AJUSTE DE NOMES DE VARIÁVEIS)
wyscout_2021 <- wyscout_2021 %>% 
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
brasil_21_merge <- merge(brasil_21_consolidado, wyscout_2021, all = TRUE)

# AJUSTE MANUAL DE 47 GRAFIAS DE NOMES DE JOGADORES OU DE TIMES DE PROCEDÊNCIA NA BASE DO WYSCOUT
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Adenílson', 'Adenilson', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Alan Ruschel', 'Cruzeiro', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'André' & wyscout_2021$Time %in% 'Sport', 'André Felipe', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'A. Araos', 'Ángelo Araos', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Arthur Gomes', 'Santos', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Bruninho' & wyscout_2021$Time %in% 'Juventude', 'Ituano', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'C. Palacios', 'Carlos Palacios', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'C. Pinares', 'César Pinares', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Cristovam', 'Cristóvam', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Daniel Guedes', 'Santos', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Diego Tardelli', 'Atlético-MG', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'D. Borrero', 'Dylan Borrero', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Edson' & wyscout_2021$Time %in% 'Bahia', 'Edson Fernando', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Everton Felipe', 'São Paulo', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Felipe Augusto', 'Corinthians', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'G. Cano', 'Germán Cano', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Guilherme Dellatorre', 'Dellatorre', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Hernanes', 'São Paulo', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Jean Silva', 'Vila Nova', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'J. Savarino', 'Jefferson Savarino', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'J. Cazares', 'Juan Cazares', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Lohan', 'América-MG', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Luan Polli', 'Sport', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Luiz Gustavo', 'Cuiabá', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Marcão' & wyscout_2021$Time %in% 'Sport', 'Marcão Silva', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Márcio Araújo', 'Sport', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Marcos Júnior', 'Vasco', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Matheus Oliveira', 'Atlético-GO', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Moacir', 'Criciúma', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Nádson', 'CSA', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Norberto Neto', 'Norberto', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Odivan', 'Juventude', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Patric', 'Sport', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Rafael Longuine', 'Santos', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'R. Cáceres', 'Raúl Cáceres', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Rodrigo Andrade', 'Sampaio Corrêa', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Ronan', 'Sampaio Corrêa', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Saulo', 'Saulo Mineiro', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Tárik', 'Tarik', wyscout_2021$Jogador)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Vitor Leque', 'Atlético-GO', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Werley', 'Vasco', wyscout_2021$Time)
wyscout_2021$Time <- ifelse(wyscout_2021$Jogador %in% 'Yago Rocha', 'Tombense', wyscout_2021$Time)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Zémarcio', 'Zemarcio', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Luan Silva', 'Luan Pereira', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Edson' & wyscout_2021$Time %in% 'Bahia', 'Edson Fernando', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Saulo' & wyscout_2021$Time %in% 'Ceará', 'Saulo Mineiro', wyscout_2021$Jogador)
wyscout_2021$Jogador <- ifelse(wyscout_2021$Jogador %in% 'Henrique' & wyscout_2021$Time %in% 'Coritiba', 'Henrique Vermudt', wyscout_2021$Jogador)

#REPETE A FUSÃO DOS DOIS DATA FRAMES APÓS AS CORREÇÕES
brasil_21_merge <- merge(brasil_21_consolidado, wyscout_2021, all = TRUE)

###PROSPECÇÃO DE DADOS DE DESEMPENHO DA TEMPORADA 2021
###PARA JOGADORES TRANSFERIDOS EM 2021 SEM DADOS DE DESEMPENHO EM 2021
#FILTRO DA BASE DE DADOS BRASIL_21_MERGE PELA AUSÊNCIA DE NA's NA VARIÁVEL JOGOS
brasil_21_sobras <- brasil_21_merge %>% 
  filter(!is.na(Nome) & is.na(Jogos)) %>% 
  select(1:11)

#EXCLUSÃO DE NA's NA VARIÁVEL NOME E JOGOS
#EXCLUSÃO DE DUPLICATAS NA VARIÁVEIS NOME E JOGOS
#212 OBSERVAÇÕES
brasil_21_merge <- brasil_21_merge %>% 
  filter(!is.na(Nome) & !is.na(Jogos)) %>% 
  distinct(Nome, .keep_all = TRUE) %>% 
  distinct(Jogador, .keep_all = TRUE) 

#MERGE DAS TRANSFERÊNCIAS QUE SOBRARAM DE 2021 COM OS DADOS DE DESEMPENHO DE 2020
# (TÊM DADOS DE TRANSFERÊNCIA MAS NÃO TÊM DE DESEMPENHO)
brasil_21_sobras <- merge(brasil_21_sobras, wyscout_2020,  all = TRUE)

# AJUSTE MANUAL DE 11 GRAFIAS DE NOMES DE JOGADORES OU DE TIMES DE PROCEDÊNCIA NA BASE DO WYSCOUT
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Caíque França', 'Corinthians', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'C. Rentería', 'Sport', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'C. Rentería' & wyscout_2020$Time %in% 'Sport', 'Carlos Rentería', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Daniel Silva', 'Sampaio Corrêa', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'F. Barrandeguy', 'Federico Barrandeguy', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Felipe Augusto', 'Cruzeiro', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Genílson', 'Criciúma', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'M. Vázquez', 'Mariano Vázquez', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Matheus Mancini', 'Ituano', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Pedro Lucas', 'Internacional', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Philipe Maia', 'Criciúma', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Kozlinski', 'Mauricio Kozlinski', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'B. Lucumí', 'Brayan Lucumí', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Nathan' & wyscout_2020$Time %in% 'Coritiba', 'Nathan Fogaça', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Henrique' & wyscout_2020$Time %in% 'Vasco', 'Henrique Silva', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'M. Viña', 'Matías Viña', wyscout_2020$Jogador)

#REPETE A FUSÃO DOS DOIS DATA FRAMES APÓS AS CORREÇÕES
brasil_21_sobras <- merge(brasil_21_sobras, wyscout_2020,  all = TRUE)

#EXCLUSÃO DE OBSERVAÇÕES COM NA'S EM JOGOS & VALOR = 0
#SUBSTITUIÇÃO DE NA'S POR 0 PARA JOGADORES COM VALOR > 0
#37 OBSERVAÇÕES
brasil_21_sobras <- brasil_21_sobras %>% 
  filter(!is.na(Nome)) %>% 
  distinct(Nome, .keep_all = TRUE) %>% 
  distinct(Jogador, .keep_all = TRUE) %>%
  mutate(Jogos = ifelse(Valor > 0 & is.na(Jogos), 0, Jogos)) %>% 
  filter(!is.na(Jogos)) %>%
  mutate(across(12:55, replace_na, 0))

#BIND ROWS DOS BANCOS DE TRANSFERÊNCIAS 2020/DESEMPENHO 2020 E TRANSFERÊNCIAS 2020/DESEMPENHO 2019
brasil_transferencias_2021 <- bind_rows(brasil_21_merge, brasil_21_sobras)

#CONSOLIDAÇÃO DO DATA FRAME
#249 OBSERVAÇÕES SÉRIES A E B 2021
#40 DAS 249 COM VALOR DE TRANSFERÊNCIA
brasil_transferencias_2021 <- as.data.frame(brasil_transferencias_2021)

#EXPORTAÇÃO PARA ARQUIVO XSML
write_xlsx(brasil_transferencias_2021, "brasil_transferencias_2021.xlsx")
