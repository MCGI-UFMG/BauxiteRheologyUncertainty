#Código para teste do ajuste de modelos para parâmetros da equação de Herchel-Bulkley
#Gabriel Fonseca Cunha e Chat GPT 4.0, 2024

# ---------------- BIBLIOTECAS ---------------
library(tidyverse)
library(brms)
library(dplyr)
library(ggplot2)

# ---------------- LEITURA DE DADOS ---------------
#Abre arquivo de dados e constrói dataframe
dat <- openxlsx::read.xlsx(xlsxFile = 'IncertezaReologiaRejBauxita\reologia\1_dados\dados_reometria.xlsx',
                           sheet    = 'dados',
                           rows     = 1:14465,
                           cols     = 1:5)

class(dat)

#Remove registros sem dados ou com dados negativos
dat <- na.omit(dat)
dat <- dat[dat$gamma >= 0 & dat$tau >= 0, ]

#Cria database menor, para fases 2 e 3, e TS 50, 55 e 60
dat_teste <- dat %>%
  filter(fase %in% c(2, 3) & TS %in% c(50, 55, 60))

nfase=n_distinct(dat_teste$fase) #número de diferentes valores para 'fase'
nTS=n_distinct(dat_teste$TS) #número de diferentes valores para 'TS'

#Remove dat para liberação de memória
rm(dat)
gc()


# ---------------- DEFINIÇÃO DOS MODELOS ---------------
modelo_HB_1 <- brms::bf(tau ~ a + b * (gamma^c),
                        a ~ 1 + (1|TS|fase),
                        b ~ 1 + (1|TS|fase),
                        c ~ 1 + (1|TS|fase),
                        nl = TRUE)

modelo_HB_2 <- brms::bf(tau ~ a + b * (gamma^c),
                          a ~ 1 + (1|TS) + (1|TS:fase),
                          b ~ 1 + (1|TS) + (1|TS:fase),
                          c ~ 1 + (1|TS) + (1|TS:fase),
                          nl = TRUE)

modelo_HB_3 <- brms::bf(tau ~ a + b * (gamma^c),
                        a ~ 1 + (1|TS:fase),
                        b ~ 1 + (1|TS:fase),
                        c ~ 1 + (1|TS:fase),
                        nl = TRUE)

modelo_HB_4 <- brms::bf(tau ~ a + b * (gamma^c),
                          a ~ 1 + (1|TS) + (1|fase),
                          b ~ 1 + (1|TS) + (1|fase),
                          c ~ 1 + (1|TS) + (1|fase),
                          nl = TRUE)

gc()


# ---------------- DEFINIÇÃO DE PRIORIS E AJUSTE ---------------
#Definição de prioris
myprior <- c(prior(uniform(10, 5000), nlpar = 'a', lb = 10, ub = 5000),
             prior(uniform(1, 1000), nlpar = 'b', lb = 1, ub = 1000),
             prior(uniform(10^-3, 1), nlpar = 'c', lb = 10^-3, ub = 1))

#Ajustes
set.seed(123)

fit1 <- brm(formula = modelo_HB_1,
            data = dat_teste,
            family = brmsfamily('Gaussian'),
            prior = myprior,
            control = list(adapt_delta = 0.9),
            iter = 10000,
            warmup = 5000,
            thin = 5,
            chains = 4,
            cores = 6,
            refresh = 0,
            seed = 123)

fit2 <- brm(formula = modelo_HB_2,
            data = dat_teste,
            family = brmsfamily('Gaussian'),
            prior = myprior,
            control = list(adapt_delta = 0.9),
            iter = 10000,
            warmup = 5000,
            thin = 5,
            chains = 4,
            cores = 6,
            refresh = 0,
            seed = 123)

fit3 <- brm(formula = modelo_HB_3,
            data = dat_teste,
            family = brmsfamily('Gaussian'),
            prior = myprior,
            control = list(adapt_delta = 0.9),
            iter = 10000,
            warmup = 5000,
            thin = 5,
            chains = 4,
            cores = 6,
            refresh = 0,
            seed = 123)

fit4 <- brm(formula = modelo_HB_3,
            data = dat_teste,
            family = brmsfamily('Gaussian'),
            prior = myprior,
            control = list(adapt_delta = 0.9),
            iter = 10000,
            warmup = 5000,
            thin = 5,
            chains = 4,
            cores = 6,
            refresh = 0,
            seed = 123)

# ---------------- VERIFICAÇÃO PRELIMINAR DOS AJUSTES ---------------
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

plot(fit1)
plot(fit2)
plot(fit3)
plot(fit4)

pp_check(fit1, ndraws = 100)
pp_check(fit2, ndraws = 100)
pp_check(fit3, ndraws = 100)
pp_check(fit4, ndraws = 100)

LOO_result <- LOO(fit1, fit2, fit3, fit4)
#Foi identificado que fit3 e fit4 são modelos equivalentes.
