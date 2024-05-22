
library(tidyverse)


audiencia <- mov %>%
           filter(str_detect(movimentacao,"(?i)designada audi.ncia")) %>%
           mutate(data_audiencia = str_extract(movimentacao,"(?<=Data..)\\d+/\\d+/\\d+"),
                  hora_audiencia = str_extract(movimentacao,"(?<=Hora..)\\d+\\:\\d+"),
                  local_audiencia = str_extract(movimentacao,"(?<=Local).+"),
                  situacao_audiencia = str_extract(movimentacao,"(?<=Situa.{5}).+")
           )

