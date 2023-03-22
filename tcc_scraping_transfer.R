library(worldfootballR)
library(tidyverse)
library(writexl)

###SCRAPING DE DADOS DA SÉRIE A
###TEMPORADAS 2017-2022

###2017
#OBTER AS URL's DOS TIMES
times_urls_2017 <- tm_league_team_urls(country_name = "Brazil", start_year = 2017)

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
transferencias_2017 <- tm_team_transfers(team_url = times_urls_2017, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(transferencias_2017, "tm_transferencias_seriea_2017.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_urls_2017 <- tm_player_bio(player_urls = transferencias_2017$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_urls_2017, "tm_jogadores_seriea_2017.xlsx") 

###2018
#OBTER AS URL's DOS TIMES
times_urls_2018 <- tm_league_team_urls(country_name = "Brazil", start_year = 2018)

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
transferencias_2018 <- tm_team_transfers(team_url = times_urls_2018, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(transferencias_2018, "tm_transferencias_seriea_2018.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_urls_2018 <- tm_player_bio(player_urls = transferencias_2018$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_urls_2018, "tm_jogadores_seriea_2018.xlsx") 

###2019
#OBTER AS URL's DOS TIMES
times_urls_2019 <- tm_league_team_urls(country_name = "Brazil", start_year = 2019)

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
transferencias_2019 <- tm_team_transfers(team_url = times_urls_2019, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(transferencias_2019, "tm_transferencias_seriea_2019.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_urls_2019 <- tm_player_bio(player_urls = transferencias_2019$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_urls_2019, "tm_jogadores_seriea_2019.xlsx") 

###2020
#OBTER AS URL's DOS TIMES
times_urls_2020 <- tm_league_team_urls(country_name = "Brazil", start_year = 2020)

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
transferencias_2020 <- tm_team_transfers(team_url = times_urls_2020, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(transferencias_2020, "tm_transferencias_seriea_2020.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_urls_2020 <- tm_player_bio(player_urls = transferencias_2020$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_urls_2020, "tm_jogadores_seriea_2020.xlsx") 

###2021
#OBTER AS URL's DOS TIMES
times_urls_2021 <- tm_league_team_urls(country_name = "Brazil", start_year = 2021)

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
transferencias_2021 <- tm_team_transfers(team_url = times_urls_2021, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(transferencias_2021, "tm_transferencias_seriea_2021.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_urls_2021 <- tm_player_bio(player_urls = transferencias_2021$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_urls_2021, "tm_jogadores_seriea_2021.xlsx") 

###2022
#OBTER AS URL's DOS TIMES
times_urls_2022 <- tm_league_team_urls(start_year = 2022, league_url = "https://www.transfermarkt.com/campeonato-brasileiro-serie-a/startseite/wettbewerb/BRA1")

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
transferencias_2022 <- tm_team_transfers(team_url = times_urls_2022, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(transferencias_2022, "tm_transferencias_seriea_2022.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_urls_2022 <- tm_player_bio(player_urls = transferencias_2022$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_urls_2022, "tm_jogadores_seriea_2022.xlsx") 

################################################################################

###SCRAPING DE DADOS DA SÉRIE B
###TEMPORADAS 2017-2022

###2017
#OBTER AS URL's DOS TIMES
serieb_urls_2017 <- tm_league_team_urls(start_year = 2017, league_url = "https://www.transfermarkt.com/campeonato-brasileiro-serie-b/startseite/wettbewerb/BRA2")

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
serieb_transf_2017 <- tm_team_transfers(team_url = serieb_urls_2017, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(serieb_transf_2017, "tm_transferencias_serieb_2017.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_serieb_urls_2017 <- tm_player_bio(player_urls = serieb_transf_2017$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_serieb_urls_2017, "tm_jogadores_serieb_2017.xlsx")

###2018
#OBTER AS URL's DOS TIMES
serieb_urls_2018 <- tm_league_team_urls(start_year = 2018, league_url = "https://www.transfermarkt.com/campeonato-brasileiro-serie-b/startseite/wettbewerb/BRA2")

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
serieb_transf_2018 <- tm_team_transfers(team_url = serieb_urls_2018, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(serieb_transf_2018, "tm_transferencias_serieb_2018.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_serieb_urls_2018 <- tm_player_bio(player_urls = serieb_transf_2018$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_serieb_urls_2018, "tm_jogadores_serieb_2018.xlsx")

###2019
#OBTER AS URL's DOS TIMES
serieb_urls_2019 <- tm_league_team_urls(start_year = 2019, league_url = "https://www.transfermarkt.com/campeonato-brasileiro-serie-b/startseite/wettbewerb/BRA2")

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
serieb_transf_2019 <- tm_team_transfers(team_url = serieb_urls_2019, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(serieb_transf_2019, "tm_transferencias_serieb_2019.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_serieb_urls_2019 <- tm_player_bio(player_urls = serieb_transf_2019$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_serieb_urls_2019, "tm_jogadores_serieb_2019.xlsx")

###2020
#OBTER AS URL's DOS TIMES
serieb_urls_2020 <- tm_league_team_urls(start_year = 2020, league_url = "https://www.transfermarkt.com/campeonato-brasileiro-serie-b/startseite/wettbewerb/BRA2")

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
serieb_transf_2020 <- tm_team_transfers(team_url = serieb_urls_2020, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(serieb_transf_2020, "tm_transferencias_serieb_2020.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_serieb_urls_2020 <- tm_player_bio(player_urls = serieb_transf_2020$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_serieb_urls_2020, "tm_jogadores_serieb_2020.xlsx")

###2021
#OBTER AS URL's DOS TIMES
serieb_urls_2021 <- tm_league_team_urls(start_year = 2021, league_url = "https://www.transfermarkt.com/campeonato-brasileiro-serie-b/startseite/wettbewerb/BRA2")

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
serieb_transf_2021 <- tm_team_transfers(team_url = serieb_urls_2021, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(serieb_transf_2021, "tm_transferencias_serieb_2021.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_serieb_urls_2021 <- tm_player_bio(player_urls = serieb_transf_2021$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_serieb_urls_2021, "tm_jogadores_serieb_2021.xlsx")

###2022
#OBTER AS URL's DOS TIMES
serieb_urls_2022 <- tm_league_team_urls(start_year = 2022, league_url = "https://www.transfermarkt.com/campeonato-brasileiro-serie-b/startseite/wettbewerb/BRA2")

#LISTA DE JOGADORES COM VARIÁVEIS DE TRANSFERÊNCIAS
serieb_transf_2022 <- tm_team_transfers(team_url = serieb_urls_2022, transfer_window = "all")

#EXPORTAR LISTA DE TRANSFERÊNCIAS
write_xlsx(serieb_transf_2022, "tm_transferencias_serieb_2022.xlsx") 

#BIOGRAFIAS DOS JOGADORES COM VARIÁVEL DE NOME COMPLETO
jogadores_serieb_urls_2022 <- tm_player_bio(player_urls = serieb_transf_2022$player_url)

#EXPORTAR BIOGRAFIAS
write_xlsx(jogadores_serieb_urls_2022, "tm_jogadores_serieb_2022.xlsx")
