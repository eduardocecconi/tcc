library(tidyverse)
library(readxl)
library(writexl)
library(stats)

###DADOS DE TRANSFERÊNCIAS - 2020 - SÉRIES A E B

#CARREGAR LISTAS DE TRANSFERÊNCIAS
transf_seriea_20 <- read_excel("tm_transferencias_seriea_2020.xlsx")

transf_serieb_20 <- read_excel("tm_transferencias_serieb_2020.xlsx")

#CARREGAR BIOGRAFIA DOS JOGADORES
jogadores_seriea_20 <- read_excel("tm_jogadores_seriea_2020.xlsx")

jogadores_serieb_20 <- read_excel("tm_jogadores_serieb_2020.xlsx")

#SUBSTITUIR NA'S DA COLUNA FULL_NAME COM DADOS DA COLUNA NAME_IN_HOME_COUNTRY
jogadores_seriea_20$full_name <- ifelse(is.na(jogadores_seriea_20$full_name), 
                                        jogadores_seriea_20$name_in_home_country, 
                                        jogadores_seriea_20$full_name)

jogadores_serieb_20$full_name <- ifelse(is.na(jogadores_serieb_20$full_name), 
                                        jogadores_serieb_20$name_in_home_country, 
                                        jogadores_serieb_20$full_name)

#SELECIONAR O NOME DO JOGADOR NA COLUNA PLAYER_NAME
jogadores_seriea_20$player_name <- word(jogadores_seriea_20$player_name, -2, -1)

jogadores_serieb_20$player_name <- word(jogadores_serieb_20$player_name, -2, -1)

#EXCLUIR ESPAÇOS NO INÍCIO DO NOME DOS JOGADORES
jogadores_seriea_20$player_name <- str_trim(jogadores_seriea_20$player_name, side = "both")

jogadores_serieb_20$player_name <- str_trim(jogadores_serieb_20$player_name, side = "both")

#SUBSTITUIR NA'S DA COLUNA FULL_NAME COM DADOS DA COLUNA PLAYER_NAME
jogadores_seriea_20 <- jogadores_seriea_20 %>% 
  mutate(full_name = ifelse(is.na(jogadores_seriea_20$full_name), 
                            jogadores_seriea_20$player_name, jogadores_seriea_20$full_name)) %>%
  select(player_name, full_name)

jogadores_serieb_20 <- jogadores_serieb_20 %>% 
  mutate(full_name = ifelse(is.na(jogadores_serieb_20$full_name), 
                            jogadores_serieb_20$player_name, jogadores_serieb_20$full_name)) %>%
  select(player_name, full_name)

#SELECIONAR A COMPETIÇÃO NA COLUNA LEAGUE DO BANCO DE TRANSFERÊNCIAS
transf_seriea_20$league <- word(transf_seriea_20$league, -2, -1)

transf_serieb_20$league <- word(transf_serieb_20$league, -2, -1)

###WRANGLING SÉRIE A 2020
#JOIN DOS BANCOS DE TRANSFERÊNCIAS E DE NOMES COMPLETOS
transf_seriea_20 <- left_join(transf_seriea_20,jogadores_seriea_20)

#ELIMINAÇÃO DE DUPLICATAS NA COLUNA FULL_NAMES
transf_seriea_20 <- distinct(transf_seriea_20, full_name, .keep_all = TRUE)

#CONSOLIDAÇÃO DADOS TEMPORADA 2020 SÉRIE A
#REDUÇÃO DE 696 PARA 141 OBSERVAÇÕES QUE ATENDEM AOS FILTROS
brasil_20_seriea <- transf_seriea_20 %>%
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

###WRANGLING SÉRIE B 2020
#JOIN DOS BANCOS DE TRANSFERÊNCIAS E DE NOMES COMPLETOS
transf_serieb_20 <- left_join(transf_serieb_20,jogadores_serieb_20)

#ELIMINAÇÃO DE DUPLICATAS NA COLUNA FULL_NAMES
transf_serieb_20 <- distinct(transf_serieb_20, full_name, .keep_all = TRUE)

#CONSOLIDAÇÃO DADOS TEMPORADA 2020 SÉRIE B
#REDUÇÃO DE 1005 PARA 210 OBSERVAÇÕES QUE ATENDEM AOS FILTROS
brasil_20_serieb <- transf_serieb_20 %>%
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
brasil_20_consolidado <- bind_rows(brasil_20_seriea, brasil_20_serieb)

#AJUSTES DAS POSIÇÕES DOS JOGADORES E TRADUÇÃO DAS JANELAS
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% "Attacking Midfield"] <- 'Meia-Atacante'
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% "Centre-Forward"] <- 'Centroavante'
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% c("Centre-Back", "defence")] <- 'Zagueiro'
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% c("Right Winger", "Right Midfield")] <- 'Extremo-Direito'
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% c("Left Midfield", "Left Winger")] <- 'Extremo-Esquerdo'
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% "Left-Back"] <- 'Lateral-Esquerdo'
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% c("Central Midfield", "midfield")] <- 'Meia'
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% "Defensive Midfield"] <- 'Volante'
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% "Right-Back"] <- 'Lateral-Direito'
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% "Second Striker"] <- 'Atacante'
brasil_20_consolidado$Posicao[brasil_20_consolidado$Posicao %in% "Goalkeeper"] <- 'Goleiro'
brasil_20_consolidado$Janela[brasil_20_consolidado$Janela %in% "Summer"] <- "Verão"
brasil_20_consolidado$Janela[brasil_20_consolidado$Janela %in% "Winter"] <- "Inverno"

#AJUSTES DOS NOMES DOS 40 TIMES
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Clube de Regatas do Flamengo"] <- 'Flamengo'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in%  "Sport Club Corinthians Paulista"] <- 'Corinthians'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Grêmio Foot-Ball Porto Alegrense"] <- 'Grêmio'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Sport Club Internacional"] <- 'Internacional'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Clube Atlético Mineiro"] <- 'Atlético-MG'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Sociedade Esportiva Palmeiras"] <- 'Palmeiras'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "São Paulo Futebol Clube"] <- 'São Paulo'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Santos FC"] <- 'Santos'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Fluminense Football Club"] <- 'Fluminense'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Red Bull Bragantino"] <- 'RB Bragantino'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Club Athletico Paranaense"] <- 'Athletico'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Fortaleza Esporte Clube"] <- 'Fortaleza'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Esporte Clube Bahia"] <- 'Bahia'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Sport Club do Recife"] <- 'Sport'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Esporte Clube Juventude"] <- 'Juventude'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Atlético Clube Goianiense"] <- 'Atlético-GO'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Ceará Sporting Club"] <- 'Ceará'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Cuiabá Esporte Clube (MT)"] <- 'Cuiabá'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "América Futebol Clube (MG)"] <- 'América-MG'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Associação Chapecoense de Futebol"] <- 'Chapecoense'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Club de Regatas Vasco da Gama"] <- 'Vasco'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Botafogo de Futebol e Regatas"] <- 'Botafogo'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Cruzeiro Esporte Clube"] <- 'Cruzeiro'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Avaí Futebol Clube (SC)"] <- 'Avaí'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Centro Sportivo Alagoano (AL)"] <- 'CSA'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Coritiba Foot Ball Club"] <- 'Coritiba'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Clube de Regatas Brasil (AL)"] <- 'CRB'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Associação Atlética Ponte Preta"] <- 'Ponte Preta'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Vila Nova Futebol Clube (GO)"] <- 'Vila Nova'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Clube Náutico Capibaribe"] <- 'Náutico'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Guarani Futebol Clube (SP)"] <- 'Guarani'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Associação Desportiva Confiança (SE)"] <- 'Confiança'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Goiás Esporte Clube"] <- 'Goiás'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Grêmio Esportivo Brasil (RS)"] <- 'Brasil-RS'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Clube do Remo (PA)"] <- 'Remo'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Londrina Esporte Clube (PR)"] <- 'Londrina'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Operário Ferroviário Esporte Clube (PR)"] <- 'Operário-PR'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Esporte Clube Vitória"] <- 'Vitória'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Sampaio Corrêa Futebol Clube (MA)"] <- 'Sampaio Corrêa'
brasil_20_consolidado$Time[brasil_20_consolidado$Time %in% "Brusque Futebol Clube (SC)"] <- 'Brusque'

#CORREÇÃO DE REGISTROS DO TRANSFERMARKT - EXCLUSÃO DE DUPLICATAS NA VARIÁVEL JOGADOR
#323 OBSERVAÇÕES
brasil_20_consolidado <- brasil_20_consolidado %>% 
  distinct(Jogador, .keep_all = TRUE) 

#ALTERAÇÃO PONTUAL DE UM REGISTRO NA VARIÁVEL TIME
brasil_20_consolidado$Time <- ifelse(brasil_20_consolidado$Jogador %in% 'Yuri Lara', 'Oeste', brasil_20_consolidado$Time)

###DADOS DE DESEMPENHO - WYSCOUT - SÉRIES A E B
#IMPORTAR DADOS
wyscout_seriea_20 <- read_excel("wyscout_seriea_2020.xlsx")

wyscout_serieb_20 <- read_excel("wyscout_serieb_2020.xlsx")

#BIND ROWS SÉRIES A E B
#1530 OBSERVAÇÕES
wyscout_2020 <- bind_rows(wyscout_seriea_20, wyscout_serieb_20)

#WRANGLING DADOS DE DESEMPENHO (AJUSTE DE NOMES DE VARIÁVEIS)
wyscout_2020 <- wyscout_2020 %>% 
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
brasil_20_merge <- merge(brasil_20_consolidado, wyscout_2020, all = TRUE)

# AJUSTE MANUAL DE 74 GRAFIAS DE NOMES DE JOGADORES OU DE TIMES DE PROCEDÊNCIA NA BASE DO WYSCOUT
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Alecsandro', 'CSA', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Anderson Martins', 'São Paulo', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Bady', 'Guarani', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Bruno Michel', 'Ponte Preta', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Bruno Xavier', 'Juventude', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Camacho', 'Guilherme Camacho', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Cajuru', 'Alexandre Cajuru', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Cleberson', 'Sport', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Edílson' & wyscout_2020$Time %in% 'Goiás', 'Cruzeiro', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Elvis', 'Élvis', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Emerson Silva' & wyscout_2020$Time %in% 'Juventude', 'Emerson', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Evandro', 'Santos', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'F. Lombardi', 'Fernando Lombardi', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Fernandão', 'Bahia', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Fernando' & wyscout_2020$Time %in% 'Botafogo', 'Fernando Costanza', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Gegé', 'Gegê', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Genilson', 'Genílson', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Giovanni' & wyscout_2020$Time %in% 'Cruzeiro', 'Bahia', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'G. Carneiro', 'Gonzalo Carneiro', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Ítalo', 'Ítalo Melo', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Ítalo Melo', 'CSA', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Jackson' & wyscout_2020$Time %in% 'Sampaio Corrêa', 'Remo', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Jean Drosny', 'Jean Carlos', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Jean Patrick', 'Sport', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Jefferson' & wyscout_2020$Time %in% 'Náutico', 'Jefferson Nem', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Johnatan Vital', 'CSA', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'J. Gómez', 'Jonatan Gómez', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'J. Caicedo', 'Jordy Caicedo', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Judivan', 'Cruzeiro', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Juninho Quixadá', 'Ceará', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'K. Quevedo', 'Kevin Quevedo', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Leandro Silva' & wyscout_2020$Time %in% 'Vitória', 'América-MG', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Luan Santos' & wyscout_2020$Time %in% 'Sampaio Corrêa', 'Luan Ferreira', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Lucas Cândido', 'Atlético-MG', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'L. Mugni', 'Lucas Mugni', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Lucas Verissimo', 'Lucas Veríssimo', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Luciano', 'Grêmio', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Luciano Castán', 'Luciano Castan', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Matheus Cavichioli', 'Guarani', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Matheus Dantas', 'Flamengo', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Matheus Neris', 'Palmeiras', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Matheus Sales', 'Palmeiras', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'M. Boselli', 'Mauro Boselli', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Mike', 'Goiás', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Michel Douglas', 'CSA', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Nathan Cachorrão' & wyscout_2020$Time %in% 'Brasil-RS', 'Nathan', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Nenê Bonilha', 'Fortaleza', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Olivio', 'Olívio', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Pedro Henrique' & wyscout_2020$Time %in% 'Athletico', 'Corinthians', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Pedro Ken', 'Juventude', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Pedrão', 'Pedro Maranhão', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Rafael Lima', 'Coritiba', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Rafael Ribeiro' & wyscout_2020$Time %in% 'Náutico', 'Rafael Dumas', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Raí Ramos', 'Londrina', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Ramon Tanque', 'Ramon', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Raul', 'Vasco', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Renan Bressan', 'Juventude', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Ricardo Oliveira', 'Atlético-MG', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Rithely', 'Sport', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Roberson', 'Cruzeiro', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Robinho' & wyscout_2020$Time %in% 'Grêmio', 'Cruzeiro', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Rone', 'Chapecoense', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Ruan Renato', 'Botafogo', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Sabino' & wyscout_2020$Time %in% 'Coritiba', 'Santos', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Thiago Neves', 'Grêmio', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Wanderley', 'Coritiba', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Washington', 'Washington Santana', wyscout_2020$Jogador)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Wesley' & wyscout_2020$Time %in% 'CRB', 'Avaí', wyscout_2020$Time)
wyscout_2020$Time <- ifelse(wyscout_2020$Jogador %in% 'Willian Maranhão', 'Vasco', wyscout_2020$Time)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Willians Santana', 'Willians', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Y. Soteldo', 'Yeferson Soteldo', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Yuri' & wyscout_2020$Time %in% 'Oeste', 'Yuri Lara', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Willian Simões', 'Wilian Simões', wyscout_2020$Jogador)
wyscout_2020$Jogador <- ifelse(wyscout_2020$Jogador %in% 'Zanocelo', 'Vinicius Zanocelo', wyscout_2020$Jogador)

#REPETE A FUSÃO DOS DOIS DATA FRAMES APÓS AS CORREÇÕES
brasil_20_merge <- merge(brasil_20_consolidado, wyscout_2020, all = TRUE)

###PROSPECÇÃO DE DADOS DE DESEMPENHO DA TEMPORADA 2019 
###PARA JOGADORES TRANSFERIDOS EM 2020 SEM DADOS DE DESEMPENHO EM 2020
#FILTRO DA BASE DE DADOS BRASIL_19_MERGE PELA AUSÊNCIA DE NA's NA VARIÁVEL JOGOS
brasil_20_sobras <- brasil_20_merge %>% 
  filter(!is.na(Nome) & is.na(Jogos)) %>% 
  select(1:11)

#EXCLUSÃO DE NA's NA VARIÁVEL NOME E JOGOS
#EXCLUSÃO DE DUPLICATAS NA VARIÁVEIS NOME E JOGOS
#193 OBSERVAÇÕES
brasil_20_merge <- brasil_20_merge %>% 
  filter(!is.na(Nome) & !is.na(Jogos)) %>% 
  distinct(Nome, .keep_all = TRUE) %>% 
  distinct(Jogador, .keep_all = TRUE) 

#MERGE DAS TRANSFERÊNCIAS QUE SOBRARAM DE 2020 COM OS DADOS DE DESEMPENHO DE 2019 
# (TÊM DADOS DE TRANSFERÊNCIA MAS NÃO TÊM DE DESEMPENHO)
brasil_20_sobras <- merge(brasil_20_sobras, wyscout_2019,  all = TRUE)

# AJUSTE MANUAL DE 11 GRAFIAS DE NOMES DE JOGADORES OU DE TIMES DE PROCEDÊNCIA NA BASE DO WYSCOUT
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Adalberto', 'Vila Nova', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Andrey' & wyscout_2019$Time %in% 'CRB', 'Sampaio Corrêa', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Muralha', 'Alex Muralha', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Alex Muralha', 'Flamengo', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'A. Guerra', 'Alejandro Guerra', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Alejandro Guerra', 'Palmeiras', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'Anderson Leite' & wyscout_2019$Time %in% 'Londrina', 'Anderson', wyscout_2019$Jogador)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'André' & wyscout_2019$Time %in% 'Grêmio', 'André Felipe', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Arthur Caculé', 'Cuiabá', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Eduardo Ramos', 'Remo', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Felipe Saraiva', 'Ponte Preta', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Gilsinho', 'Vila Nova', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'J. Carli', 'Joel Carli', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'John Lennon', 'Juventude', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Jordi', 'Vasco', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Leandro Almeida', 'Guarani', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Léo Rigo', 'Operário-PR', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Liel', 'Vila Nova', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Luanderson', 'Náutico', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Marcelo Hermes', 'Cruzeiro', wyscout_2019$Time)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Mateus Anderson', 'Ponte Preta', wyscout_2019$Time)
wyscout_2019$Jogador <- ifelse(wyscout_2019$Jogador %in% 'O. Berrío', 'Orlando Berrío', wyscout_2019$Jogador)
wyscout_2019$Time <- ifelse(wyscout_2019$Jogador %in% 'Wellington Nem', 'Fortaleza', wyscout_2019$Time)

#REPETE A FUSÃO DOS DOIS DATA FRAMES APÓS AS CORREÇÕES
brasil_20_sobras <- merge(brasil_20_sobras, wyscout_2019,  all = TRUE)

#EXCLUSÃO DE OBSERVAÇÕES COM NA'S EM JOGOS & VALOR = 0
#SUBSTITUIÇÃO DE NA'S POR 0 PARA JOGADORES COM VALOR > 0
#48 OBSERVAÇÕES
brasil_20_sobras <- brasil_20_sobras %>% 
  filter(!is.na(Nome)) %>% 
  distinct(Nome, .keep_all = TRUE) %>% 
  distinct(Jogador, .keep_all = TRUE) %>%
  mutate(Jogos = ifelse(Valor > 0 & is.na(Jogos), 0, Jogos)) %>% 
  filter(!is.na(Jogos)) %>%
  mutate(across(12:55, replace_na, 0))

#BIND ROWS DOS BANCOS DE TRANSFERÊNCIAS 2020/DESEMPENHO 2020 E TRANSFERÊNCIAS 2020/DESEMPENHO 2019
brasil_transferencias_2020 <- bind_rows(brasil_20_merge, brasil_20_sobras)

#CONSOLIDAÇÃO DO DATA FRAME
#241 OBSERVAÇÕES SÉRIES A E B 2020
#39 DAS 241 COM VALOR DE TRANSFERÊNCIA
brasil_transferencias_2020 <- as.data.frame(brasil_transferencias_2020)

#EXPORTAÇÃO PARA ARQUIVO XSML
write_xlsx(brasil_transferencias_2020, "brasil_transferencias_2020.xlsx")
