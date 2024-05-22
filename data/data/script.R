library(tidyverse)
df <- tibble(
  arquivo = a, 
  processo = str_extract(a,"\\d{20}")
) %>% 
  mutate(ano = str_sub(processo,10,13) %>% as.numeric) %>% 
  filter(ano == 2017)


df <- df %>% 
   sample_n(5000)

rstudioapi::viewer(df$arquivo[1])

mov <- ler_movimentacao_tjrj(df$arquivo)


writexl::write_xlsx(mov,"movimentacao.xlsx")

tb1 <- mov %>% 
  group_by(processo) %>% 
  arrange(desc(data)) %>% 
  count(principal,sort=TRUE)



writexl::write_xlsx(tb1,"tb1.xlsx")

tb2 <- mov %>% 
  group_by(processo) %>% 
  arrange(desc(data)) %>% 
      tempo_movimentacao()
writexl::write_xlsx(tb2,"tb2.xlsx")

tb3 <- mov %>% 
       group_by(processo) %>% 
       arrange(desc(data)) %>% 
       tempo_movimentacao() %>% 
       mutate(trio = paste0(lead(principal)," - ",principal," - ", lag(principal))) %>% 
       ungroup() %>% 
       group_by(trio) %>%
       summarize(media = mean(decorrencia))
  
writexl::write_xlsx(tb3,"tb3.xlsx")

tb4 <- mov %>% 
  group_by(processo) %>% 
  arrange(desc(data)) %>% 
  mutate(trio = paste0(lead(principal)," - ",principal," - ", lag(principal))) %>% 
  count(trio,sort= TRUE)
writexl::write_xlsx(tb4,"tb4.xlsx")
