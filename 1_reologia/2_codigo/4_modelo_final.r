#Código para modelo estatístico final da dissertação de mestrado e extração de resultados
#Gabriel Fonseca Cunha e Chat GPT 4.0, 2024


# ---------------- BIBLIOTECAS ---------------
library(brms) #modelagem bayesiana multinível
library(tidybayes) #organização e manipulação de dados para posterioris bayesianas
library(tidyverse) #organização e manipulação de dados
library(ggthemes) #temas para plotagem utilizando ggplot2
library(viridis) #paleta de cores para plotagem utilizando ggplot2 
library(cowplot) #composição de múltiplas plotagens ggplot2
library(broom) #representação de resultados de modelagem estatística em tibbles


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

#Elimina 
dat_teste <- dat %>% filter(fase != 1 & TS != 70)

nfase=n_distinct(dat_teste$fase) #número de diferentes valores para 'fase'
nTS=n_distinct(dat_teste$TS) #número de diferentes valores para 'TS'

#Remove objetos para liberação de memória
rm(dat)
rm(nfase)
rm(nTS)
gc()


# ---------------- DEFINIÇÃO DO MODELO ---------------
modelo_HB_FIN1 <- brms::bf(tau ~ a + b * (gamma^c),
                           a ~ 1 + (1|TS) + (1|TS:fase),
                           b ~ 1 + (1|TS) + (1|TS:fase),
                           c ~ 1 + (1|TS) + (1|TS:fase),
                           sigma ~ gamma + (1|TS),
                           nl = TRUE)

#Priors sugeridas
get_prior(formula = modelo_HB_FIN1,
          data = dat_teste,
          family = brmsfamily('Gaussian'))


# ---------------- DEFINIÇÃO DE PRIORIS E AJUSTE ---------------
#Definição de prioris
prior <- c(prior(uniform(10, 8000), class = 'b', nlpar = 'a', lb = 10, ub = 8000),
           prior(uniform(1, 1000), class = 'b', nlpar = 'b', lb = 1, ub = 1000),
           prior(uniform(10^-3, 1), class = 'b', nlpar = 'c', lb = 10^-3, ub = 1),
           prior(student_t(3, 0, 2.5), class = 'Intercept', dpar = 'sigma'))

#Ajuste
set.seed(123)

fit <- brm(formula = modelo_HB_FIN1,
           data = dat_teste,
           family = brmsfamily('Gaussian'),
           prior = prior,
           control = list(adapt_delta = 0.95),
           iter = 15000,
           warmup = 7000,
           thin = 5,
           chains = 4,
           cores = 4,
           seed = 123)


# ---------------- VERIFICAÇÃO PRELIMINAR DOS AJUSTES ---------------
summary(fit)

plot(fit)

pp_check(fit, ndraws = 100)

pp_check(fit, ndraws = 30, type = "boxplot")
pp_check(fit, ndraws = 30, type = "dens")
pp_check(fit, ndraws = 30, type = "ribbon")
pp_check(fit, ndraws = 30, type = "scatter")
pp_check(fit, ndraws = 30, type = "stat")

plot(conditional_effects(fit), points = TRUE)

posterior_summary(fit)

expose_functions(fit, vectorize = TRUE)

# ---------------- GRÁFICOS PARA CURVA GERAL ---------------
    # ---------------- Criação de databases ---------------
TSs <- c(30, 40, 50, 55, 60, 65)

#"Fixed credible interval"
#Não considera erros residuais (brms::posterior_epred / tidybayes::epred_draws)
#Não considera efeitos de grupo (re_formula = NA)
curva_geral_fitd <- fit %>% 
  epred_draws(newdata = expand_grid(fase = c(2,3,4),
                                    TS = TSs,
                                    gamma = seq(0, 70, by = 1)),
              re_formula = NA)

#"Fixed prediction interval"
#Considera erros residuais (brms::posterior_predict / tidybayes::predicted_draws)
#Não considera efeitos de grupo (re_formula = NA)
curva_geral_pred <- fit %>% 
  predicted_draws(newdata = expand_grid(fase = c(2,3,4),
                                        TS = TSs,
                                        gamma = seq(0, 70, by = 1)),
                  re_formula = NA)

#"Group credible interval"
#Não considera erros residuais (brms::posterior_epred / tidybayes::epred_draws)
#Considera efeitos de grupo (re_formula = ~ (1|TS))
curva_geral_fitd_grupo <- fit %>% 
  epred_draws(newdata = expand_grid(fase = c(2,3,4),
                                    TS = TSs,
                                    gamma = seq(0, 70, by = 1)),
              re_formula = NULL,
              allow_new_levels=TRUE)

#"Group prediction interval"
#Considera erros residuais (brms::posterior_predict / tidybayes::predicted_draws)
#Considera efeitos de grupo (re_formula = ~ (1|TS))
curva_geral_pred_grupo <- fit %>% 
  predicted_draws(newdata = expand_grid(fase = c(2,3,4),
                                        TS = TSs,
                                        gamma = seq(0, 70, by = 1)),
                  re_formula = NULL,
                  allow_new_levels=TRUE)

    # ---------------- Geração dos gráficos ---------------
scale_fill_modified <- scale_fill_viridis_d(
  option = "plasma",
  breaks = c(0.2, 0.4, 0.6, 0.8, 0.9),
  labels = c("20%", "40%", "60%", "80%", "90%"),
  guide = guide_legend(
    title = "Probabilidade",
    direction = "horizontal", # Configura a legenda para ser horizontal
    title.position = "top", # Posiciona o título da legenda acima das chaves
    label.position = "bottom" # Posiciona os rótulos abaixo das chaves
  )
)

plot_curva_geral_fitd <- ggplot(curva_geral_fitd, aes(x = gamma, y = .epred)) +
  stat_lineribbon(.width = c(0.2, 0.4, 0.6, 0.8, 0.9), linewidth = 0.3) +
  scale_fill_modified +
  geom_point(data = dat_teste, aes(x = gamma, y = tau), color = "black", size = 0.6) +
  labs(x = "Taxa de Cisalhamento (1/s)", y = "Tensão de Cisalhamento (Pa)") +
  theme_minimal() +
  ggtitle("Intervalo de Credibilidade - Curva Geral")

plot_curva_geral_pred <- ggplot(curva_geral_pred, aes(x = gamma, y = .prediction)) +
  stat_lineribbon(.width = c(0.2, 0.4, 0.6, 0.8, 0.9), linewidth = 0.3) +
  scale_fill_modified +
  geom_point(data = dat_teste, aes(x = gamma, y = tau), color = "black", size = 0.6) +
  labs(x = "Taxa de Cisalhamento (1/s)", y = "Tensão de Cisalhamento (Pa)") +
  theme_minimal() +
  ggtitle("Intervalo de Predição - Curva Geral")

plot_curva_geral_fitd_grupo <- ggplot(curva_geral_fitd_grupo, aes(x = gamma, y = .epred)) +
  stat_lineribbon(.width = c(0.2, 0.4, 0.6, 0.8, 0.9), linewidth = 0.3) +
  scale_fill_modified +
  geom_point(data = dat_teste, aes(x = gamma, y = tau), color = "black", size = 0.6) +
  labs(x = "Taxa de Cisalhamento (1/s)", y = "Tensão de Cisalhamento (Pa)") +
  theme_minimal() +
  ggtitle("Intervalo de Credibilidade - Curva Geral, Considerando Efeitos de Grupo")

plot_curva_geral_pred_grupo <- ggplot(curva_geral_pred_grupo, aes(x = gamma, y = .prediction)) +
  stat_lineribbon(.width = c(0.2, 0.4, 0.6, 0.8, 0.9), linewidth = 0.3) +
  scale_fill_modified +
  geom_point(data = dat_teste, aes(x = gamma, y = tau), color = "black", size = 0.6) +
  labs(x = "Taxa de Cisalhamento (1/s)", y = "Tensão de Cisalhamento (Pa)") +
  theme_minimal() +
  ggtitle("Intervalo de Predição - Curva Geral, Considerando Efeitos de Grupo")

#Plotagem gráficos com intervalos de credibilidade
nome = "plot_geral_cred"

plot_cred <- plot_grid(
  plot_curva_geral_fitd + theme(legend.position="none"),
  plot_curva_geral_fitd_grupo + theme(legend.position="none"),
  labels = "AUTO",
  ncol = 1,
  align = "v"
)

legenda_cred <- get_legend(plot_curva_geral_fitd + scale_fill_modified)

plot_geral_cred <- plot_grid(
  plot_cred,
  legenda_cred,
  ncol = 1,
  rel_heights = c(1, 0.1) # Ajuste conforme necessário para o tamanho da legenda
)

ggsave(
  filename = paste0("D:/", nome, ".jpg"),
  plot = plot_geral_cred,
  device = "jpg",
  scale = 1,
  units = "px",
  width = 1000,
  height = 1300,
  dpi = 130)


#Plotagem gráficos com intervalos de predição
nome = "plot_geral_pred"

plot_pred <- plot_grid(
  plot_curva_geral_pred + theme(legend.position="none"),
  plot_curva_geral_pred_grupo + theme(legend.position="none"),
  labels = "AUTO",
  ncol = 1,
  align = "v"
)

legenda_pred <- get_legend(plot_curva_geral_pred + scale_fill_modified)

plot_geral_pred <- plot_grid(
  plot_pred,
  legenda_pred,
  ncol = 1,
  rel_heights = c(1, 0.1) # Ajuste conforme necessário para o tamanho da legenda
)

ggsave(
  filename = paste0("D:/", nome, ".jpg"),
  plot = plot_geral_pred,
  device = "jpg",
  scale = 1,
  units = "px",
  width = 1000,
  height = 1300,
  dpi = 130)

rm(nome)
rm(curva_geral_fitd)
rm(curva_geral_fitd_grupo)
rm(curva_geral_pred)
rm(curva_geral_pred_grupo)
rm(plot_curva_geral_fitd)
rm(plot_curva_geral_fitd_grupo)
rm(plot_curva_geral_pred)
rm(plot_curva_geral_pred_grupo)
rm(plot_cred)
rm(plot_pred)
rm(plot_geral_cred)
rm(plot_geral_pred)
rm(legenda_cred)
rm(legenda_pred)
gc()


# ---------------- GRÁFICOS PARA CADA TS ---------------
TSs <- c(30, 40, 50, 55, 60, 65)

scale_fill_modified <- scale_fill_viridis_d(
  option = "plasma",
  breaks = c(0.2, 0.4, 0.6, 0.8, 0.9),
  labels = c("20%", "40%", "60%", "80%", "90%"),
  guide = guide_legend(
    title = "Probabilidade",
    direction = "horizontal", # Configura a legenda para ser horizontal
    title.position = "top", # Posiciona o título da legenda acima das chaves
    label.position = "bottom" # Posiciona os rótulos abaixo das chaves
  )
)

for (TSfiltro in TSs) {
  
  dat_teste_filtro <- dat_teste %>% filter(TS == TSfiltro)

  #"Group credible interval"
  #Não considera erros residuais (brms::posterior_epred / tidybayes::epred_draws)
  #Considera efeitos de grupo fase (re_formula = NULL)
  curva_TS_fitd <- fit %>% 
    epred_draws(newdata = expand_grid(fase = c(2,3,4),
                                      TS = TSfiltro,
                                      gamma = seq(0, 70, by = 1)),
                re_formula = NULL)
  
  #"Group prediction interval"
  #Considera erros residuais (brms::posterior_predict / tidybayes::predicted_draws)
  #Considera efeitos de grupo fase (re_formula = ~ NULL)
  curva_TS_pred <- fit %>% 
    predicted_draws(newdata = expand_grid(fase = c(2,3,4),
                                          TS = TSfiltro,
                                          gamma = seq(0, 70, by = 1)),
                    re_formula = NULL,
                    allow_new_levels=TRUE)
  
  #Geração dos gráficos
  plot_curva_TS_fitd <- ggplot(curva_TS_fitd, aes(x = gamma, y = .epred)) +
    stat_lineribbon(.width = c(0.2, 0.4, 0.6, 0.8, 0.9), linewidth = 0.3) +
    scale_fill_modified +
    geom_point(data = dat_teste_filtro, aes(x = gamma, y = tau), color = "black", size = 0.6) +
    labs(x = "Taxa de Cisalhamento (1/s)", y = "Tensão de Cisalhamento (Pa)") +
    theme_minimal() +
    ggtitle(paste0("Intervalo de Credibilidade - Grupo TS=", TSfiltro, "%"))
  
  plot_curva_TS_pred <- ggplot(curva_TS_pred, aes(x = gamma, y = .prediction)) +
    stat_lineribbon(.width = c(0.2, 0.4, 0.6, 0.8, 0.9), linewidth = 0.3) +
    scale_fill_modified +
    geom_point(data = dat_teste_filtro, aes(x = gamma, y = tau), color = "black", size = 0.6) +
    labs(x = "Taxa de Cisalhamento (1/s)", y = "Tensão de Cisalhamento (Pa)") +
    theme_minimal() +
    ggtitle(paste0("Intervalo de Predição - Grupo TS=", TSfiltro, "%"))
  
  #Plotagem dos gráficos
  plot_TS <- plot_grid(
    plot_curva_TS_fitd + theme(legend.position="none"),
    plot_curva_TS_pred + theme(legend.position="none"),
    labels = "AUTO",
    ncol = 1,
    align = "v"
  )

  legenda_TS <- get_legend(plot_curva_TS_fitd + scale_fill_modified)
  
  plot_final_TS <- plot_grid(
    plot_TS,
    legenda_TS,
    ncol = 1,
    rel_heights = c(1, 0.1) # Ajuste conforme necessário para o tamanho da legenda
  )
  
  ggsave(
    filename = paste0("D:/", "plot_TS", TSfiltro, ".jpg"),
    plot = plot_final_TS,
    device = "jpg",
    scale = 1,
    units = "px",
    width = 1000,
    height = 1300,
    dpi = 130)
  
  rm(dat_teste_filtro)
  rm(curva_TS_fitd)
  rm(curva_TS_pred)
  rm(plot_curva_TS_fitd)
  rm(plot_curva_TS_pred)
  rm(plot_TS)
  rm(legenda_TS)
  gc()

}


# ---------------- AJUSTE HB PARA CADA TS---------------
TSs <- c(30, 40, 50, 55, 60, 65)

gamma_pred = c(seq(0, 2.5, by = 0.25), seq(3, 70, by = 1))

parametros <- tibble(TS = integer(),
                     quantil = character(),
                     prob = numeric(),
                     a = numeric(),
                     b = numeric(),
                     c = numeric())


#Ajuste de parâmetros para cada probabilidade, para cada TS
for (TSfiltro in TSs) {
  
  dat_teste_filtro <- dat_teste %>% filter(TS == TSfiltro)
  
  #"Group prediction interval"
  #Considera erros residuais (brms::posterior_predict / tidybayes::predicted_draws)
  #Considera efeitos de grupo fase (re_formula = ~ NULL)
  curva_TS_pred <- fit %>% 
    predicted_draws(newdata = expand_grid(fase = c(2,3,4),
                                          TS = TSfiltro,
                                          gamma = gamma_pred),
                    re_formula = NULL,
                    allow_new_levels=TRUE)
  
  #Gerar tibble com os quantis para cada valor de taxa de cisalhamento 
  quantis <- curva_TS_pred %>%
    group_by(gamma) %>%
    summarise(
      Q10 = quantile(.prediction, 0.1),
      Q20 = quantile(.prediction, 0.2),
      Q30 = quantile(.prediction, 0.3),
      Q40 = quantile(.prediction, 0.4),
      Q50 = quantile(.prediction, 0.5),
      Q60 = quantile(.prediction, 0.6),
      Q70 = quantile(.prediction, 0.7),
      Q80 = quantile(.prediction, 0.8),
      Q90 = quantile(.prediction, 0.9)) %>% ungroup()
  
  quantil_names <- names(quantis)[-1]
  
  #Ajuste da equação de Herschel Bulkley para cada quantil
  fit_curves <- list()
  set.seed(123)
  
  for (quantil1 in quantil_names) {
    formula <- as.formula(paste(quantil1, "~ a + b * gamma^c"))
    
    fit_quantis <- tryCatch({
      nls(formula,
          data = quantis,
          start = list(a = 2000, b = 45, c = 0.8),
          lower = c(1, 1, 0.01),
          upper = c(4000, 200, 1),
          algorithm = "port",
          control = nls.control(maxiter = 1000, minFactor = 1e-8))
    }, error = function(e) {NULL}
    )
    
    if (!is.null(fit_quantis)) {
      fit_curves[[quantil1]] <- tidy(fit_quantis)
    } else {
      fit_curves[[quantil1]] <- tibble(term = NA,
                                       estimate = NA,
                                       std.error = NA,
                                       statistic = NA,
                                       p.value = NA)
    }
  }
  rm(fit_quantis)
  rm(quantil1)
  rm(formula)
  
  #Função com Equação de Herschel Bulkley
  eq_HB <- function(gamma, a, b, c) {
    tau <- a + b * gamma^c
    return(tau)
  }
  
  #Tibble com os parâmetros ajustados
  for (quantil2 in names(fit_curves)) {
    parametros <- add_row(parametros,
                          TS = TSfiltro,
                          quantil = quantil2,
                          prob = substr(quantil2, nchar(quantil2)-1, nchar(quantil2))
                          %>% as.numeric()/100,
                          a = fit_curves[[quantil2]][fit_curves[[quantil2]]$term == "a",
                                                     "estimate"]
                          %>% as.numeric(),
                          b = fit_curves[[quantil2]][fit_curves[[quantil2]]$term == "b",
                                                     "estimate"]
                          %>% as.numeric(),
                          c = fit_curves[[quantil2]][fit_curves[[quantil2]]$term == "c",
                                                     "estimate"]
                          %>% as.numeric(),
                          )
  }
  rm(quantil2)
  
  #Dataframe com os valores calculados para as curvas
  curvas <- data.frame(quantil = character(), gamma = numeric(), tau = numeric())
  
  #Loop para calcular tau para cada valor de gamma e para cada grupo de coeficientes
  for (quantil3 in names(fit_curves)) {
    a <- parametros %>% filter(TS == TSfiltro, quantil == quantil3) %>% select(a) %>% as.numeric()
    b <- parametros %>% filter(TS == TSfiltro, quantil == quantil3) %>% select(b) %>% as.numeric()
    c <- parametros %>% filter(TS == TSfiltro, quantil == quantil3) %>% select(c) %>% as.numeric()
    
    for (gamma1 in gamma_pred) {
      tau1 <- eq_HB(gamma1, a, b, c)
      curvas <- rbind(curvas, data.frame(quantil = quantil3, gamma = gamma1, tau = tau1))
    }
  }
  rm(a)
  rm(b)
  rm(c)
  rm(quantil3)
  rm(gamma1)
  rm(tau1)


  #Geração de gráficos para variação dos parâmetros
  p <- ggplot(curvas, aes(x = gamma, y = tau, color = quantil)) + 
    geom_line() +
    geom_point(data = quantis, aes(x = gamma, y = Q10), color = "black", size = 0.2) + 
    geom_point(data = quantis, aes(x = gamma, y = Q20), color = "black", size = 0.2) + 
    geom_point(data = quantis, aes(x = gamma, y = Q30), color = "black", size = 0.2) + 
    geom_point(data = quantis, aes(x = gamma, y = Q40), color = "black", size = 0.2) + 
    geom_point(data = quantis, aes(x = gamma, y = Q50), color = "black", size = 0.2) + 
    geom_point(data = quantis, aes(x = gamma, y = Q60), color = "black", size = 0.2) + 
    geom_point(data = quantis, aes(x = gamma, y = Q70), color = "black", size = 0.2) + 
    geom_point(data = quantis, aes(x = gamma, y = Q80), color = "black", size = 0.2) + 
    geom_point(data = quantis, aes(x = gamma, y = Q90), color = "black", size = 0.2) +
    ggtitle(paste0("Ajuste de curva aos limites dos intervalos de predição - TS ", TSfiltro, "%")) +
    xlab("Taxa de Cisalhamento (1/s)") +
    ylab("Tensão de Cisalhamento (Pa)") +
    labs(color = "Curvas ajustadas ao quantil predito") +
    scale_color_discrete(guide = guide_legend(reverse = TRUE))
  
  ggsave(
    filename = paste0("D:/", "plot_ajuste_TS", TSfiltro, ".jpg"),
    plot = p,
    device = "jpg",
    scale = 1,
    units = "px",
    width = 1000,
    height = 500,
    dpi = 120)
  rm(p)
}


    # ---------------- Avaliação dos resultados para parâmetros ---------------

#Função para plotar e salvar gráficos
plot_and_save <- function(data,
                          x_var,
                          y_var,
                          filter_var,
                          filter_vals,
                          x_lab,
                          y_lab,
                          prefix) {
  for (val in filter_vals) {
    filtered_data <- data %>% filter({{filter_var}} == val)
    p <- ggplot(filtered_data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
      geom_point() +
      labs(title = paste("Parâmetro", prefix, "para", deparse(substitute(filter_var)), "=", val),
           x = x_lab,  
           y = y_lab)
    
    file_path <- paste0("D:/", gsub(" ", "_", prefix), "_", deparse(substitute(filter_var)),
                        "=", val, ".png")
    
    ggsave(file_path, plot = p, width = 7, height = 5, dpi = 200)
  }
}

# Plotando e salvando os gráficos para 'a', 'b', e 'c' vs 'TS' para quantis específicos
quantis_plot <- c("Q20", "Q50", "Q80")
plot_and_save(parametros, "TS", "a", quantil, quantis_plot, "Teor de Sólidos (%)",
              "Tensão Limite de Escoamento (Pa)", "a vs TS")
plot_and_save(parametros, "TS", "b", quantil, quantis_plot, "Teor de Sólidos (%)",
              "Índice de Consistência", "b vs TS")
plot_and_save(parametros, "TS", "c", quantil, quantis_plot, "Teor de Sólidos (%)",
              "Índice de Escoamento", "c vs TS")

# Plotando e salvando os gráficos para 'a', 'b', e 'c' vs 'prob' para TS específicos
TS_vals <- c(30, 50, 65)
plot_and_save(parametros, "prob", "a", TS, TS_vals, "Probabilidade",
              "Tensão Limite de Escoamento (Pa)", "a vs quantil")
plot_and_save(parametros, "prob", "b", TS, TS_vals, "Probabilidade",
              "Índice de Consistência", "b vs quantil")
plot_and_save(parametros, "prob", "c", TS, TS_vals, "Probabilidade",
              "Índice de Escoamento", "c vs quantil")

