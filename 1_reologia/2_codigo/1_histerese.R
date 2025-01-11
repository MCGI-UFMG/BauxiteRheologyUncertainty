#Código para cálculo da histere dos dados reológicos da dissertação de mestrado
#Gabriel Fonseca Cunha e Chat GPT 4.0, 2024


# ---------------- BIBLIOTECAS ---------------
library(tidyverse)
library(ggplot2)

# ---------------- LEITURA DE DADOS ---------------
#Abre arquivo de dados e constrói dataframe
dat <- openxlsx::read.xlsx(xlsxFile = 'D:/data/data_geral.xlsx',
                           sheet    = 'data_geral',
                           rows     = 1:14465,
                           cols     = 1:5)

class(dat)

#Remove registros sem dados ou com dados negativos
dat <- na.omit(dat)
dat <- dat[dat$gamma >= 0 & dat$tau >= 0, ]

nfase=n_distinct(dat$fase) #número de diferentes valores para 'fase'
nTS=n_distinct(dat$TS) #número de diferentes valores para 'TS'

#Remove objetos para liberação de memória
rm(nfase)
rm(nTS)
gc()


# ---------------- CÁLCULO DE ÁREA ABAIXO DA CURVA ---------------
calcular_area <- function(df) {
  df_ordenado <- df[order(df$gamma), ]
  area <- 0
  for (i in 1:(nrow(df_ordenado) - 1)) {
    delta_x <- df_ordenado$gamma[i + 1] - df_ordenado$gamma[i]
    media_y <- (df_ordenado$tau[i + 1] + df_ordenado$tau[i]) / 2
    area <- area + delta_x * media_y
  }
  return(area)
}

# Dividir 'dat' em uma lista de dataframes de acordo com os grupos
list_df <- dat %>% group_by(amostra, fase, TS) %>% group_split()

# Aplicar 'calcular_area' em cada dataframe da lista e combinar os resultados
areas <- lapply(list_df, calcular_area)

# Criar um dataframe para armazenar os resultados
areas_df <- do.call(rbind, lapply(seq_along(list_df), function(i) {
  data.frame(amostra = list_df[[i]]$amostra[1], 
             fase = list_df[[i]]$fase[1], 
             TS = list_df[[i]]$TS[1], 
             area = areas[[i]])
}))


# ---------------- CÁLCULO DA HISTERESE ---------------
#Certifique-se de que os dados estão ordenados corretamente
areas_ord <- areas_df %>%
  arrange(amostra, TS, fase)

#Calcular a histerese
histerese_df <- areas_ord %>%
  group_by(amostra, TS) %>%
  mutate(histerese = area - lead(area)) %>%
  # Removendo a última linha de cada grupo, pois não tem valor seguinte para calcular a diferença
  filter(!is.na(histerese))

#Calcular a média dos valores de histerese por combinação de fase e TS
media_histerese <- histerese_df %>%
  group_by(TS, fase) %>%
  summarise(media_histerese = mean(histerese), .groups = 'drop')


# ---------------- GRÁFICO ---------------
p <- ggplot(media_histerese, aes(x = TS, y = media_histerese, color = as.factor(fase))) +
  geom_point() +
  labs(title = "Média de Áreas de Histerese vs TS",
       x = "Teor de Sólidos (%)",
       y = "Média de Áreas de Histerese (Pa/s)",
       color = "Fases") +
  theme_minimal()

ggsave(filename = "D:/output/graf_histerese.jpg", plot = p,
       units = "px", width = 1100, height = 600, dpi = 170)

rm(p)
