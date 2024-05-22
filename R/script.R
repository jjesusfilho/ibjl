library(ssh)

s<-ssh_connect(host="ubuntu@200.144.244.200:2222")

scp_download(s,"~/hd-extra/projetos/ibjl/data")

base <- qs::qread("data/base.qs")
library(tidyverse)

movimentacao<-base %>%
  select(processo,data,principal,detalhe)

movimentacao <- tempo_movimentacao(movimentacao)

nomes <- c("processo", "data", "principal", "detalhe", "anterior", "decorrencia",
  "decorrencia_acumulada", "mov_anterior", "mov_posterior", "posterior",
  "decorrencia_posterior")

movimentacao <- movimentacao %>%
  select(processo,mov_anterior,principal, mov_posterior, anterior,decorrencia,data,posterior,decorrencia_posterior,decorrencia_acumulada)

writexl::write_xlsx(movimentacao,"data/movimentacao.xlsx")

movimentacao<-ungroup(movimentacao)

movimentacao <- movimentacao %>%
       mutate(mov_anterior = lead(principal),
              mov_posterior = lag(principal))

count(movimentacao, principal, sort = T) %>% View()

sub_decorrentes <- count(movimentacao,mov_anterior,principal,mov_posterior, decorrencia, sort=TRUE)

sub_decorrentes <- sub_decorrentes %>%
                arrange(desc(n),desc(decorrencia))


writexl::write_xlsx(subsequentes,"data/subsequentes.xlsx")

tempo_movimentacao<-function(df,data = data){

  data <- rlang::enexpr(data)

  df %>%
    dplyr::group_by(processo) %>%
    dplyr::mutate(anterior:=dplyr::lead(!!data),
                  posterior:=dplyr::lag(!!data),
                  decorrencia=JurisMiner:::lapso(anterior,!!data,unidade="dia"),
                  decorrencia_posterior = JurisMiner:::lapso(!!data,posterior,unidade="dia"),
                  decorrencia_acumulada=tidyr::replace_na(decorrencia,0) %>%
                    rev() %>% cumsum() %>% rev())


}


## Classificações

classificados<-classificados %>%
  slice(1:729)
classificados$n<-NULL
movimentacao<- movimentacao %>%
     left_join(classificados,by="principal")

movimentacao<-ungroup(movimentacao)

count(movimentacao,agente,decorrencia, sort=T) %>% View()

mov_agente <- movimentacao %>%
             count(processo,agente,decorrencia)

mov_agente <- movimentacao %>%
             select(processo,agente,decorrencia,principal)

             pivot_wider(names_from=agente)


mov_agente <- mov_agente %>%
  dplyr::group_by_at(dplyr::vars(-decorrencia)) %>%
  dplyr::mutate(row_id = 1:dplyr::n()) %>%
  dplyr::ungroup() %>%
  tidyr::spread(key = agente, value = decorrencia) %>%
  dplyr::select(-row_id)

movimentacao<-movimentacao %>%
   mutate(agente= replace_na(agente,"outros"))

movimentacao<-movimentacao %>%
      mutate(agente = ifelse(agente=="Sevidor","Servidor",agente))

ma <- mov_agente %>%
             group_by(principal,processo) %>%
             rownames_to_column("id") %>%
              pivot_wider(names_from=agente,values_from = decorrencia)


ma<-ma %>%
  janitor::clean_names() %>%
  group_by(processo,principal) %>%
  fill(servidor:mp,.direction="up")

