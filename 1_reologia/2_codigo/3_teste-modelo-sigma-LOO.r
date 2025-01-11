#Código para teste do ajuste de modelos para parâmetros da equação de Herchel-Bulkley
#considerando condicionamento do desvio padrão da variável de resposta
#Gabriel Fonseca Cunha e Chat GPT 4.0, 2024


# ---------------- BIBLIOTECAS ---------------
library(tidyverse)
library(brms)
library(dplyr)


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

#Verificação do dataframe
head(dat_teste)
str(dat_teste)


# ---------------- DEFINIÇÃO DOS MODELOS ---------------
modelo_HB_teste1 <- brms::bf(tau ~ a + b * (gamma^c),
                             a ~ 1 + (1|TS) + (1|TS:fase),
                             b ~ 1 + (1|TS) + (1|TS:fase),
                             c ~ 1 + (1|TS) + (1|TS:fase),
                             nl = TRUE)

modelo_HB_teste2 <- brms::bf(tau ~ a + b * (gamma^c),
                             a ~ 1 + (1|TS) + (1|TS:fase),
                             b ~ 1 + (1|TS) + (1|TS:fase),
                             c ~ 1 + (1|TS) + (1|TS:fase),
                             sigma ~ gamma,
                             nl = TRUE)

modelo_HB_teste3 <- brms::bf(tau ~ a + b * (gamma^c),
                             a ~ 1 + (1|TS) + (1|TS:fase),
                             b ~ 1 + (1|TS) + (1|TS:fase),
                             c ~ 1 + (1|TS) + (1|TS:fase),
                             sigma ~ gamma + (1|TS),
                             nl = TRUE)

# ---------------- GET PRIOR ---------------
get_prior(formula = modelo_HB_teste1,
          data = dat_teste,
          family = brmsfamily('Gaussian'))

get_prior(formula = modelo_HB_teste2,
          data = dat_teste,
          family = brmsfamily('Gaussian'))

get_prior(formula = modelo_HB_teste3,
          data = dat_teste,
          family = brmsfamily('Gaussian'))


# ---------------- DEFINIÇÃO DE PRIORIS E AJUSTE ---------------
#Definição de prioris
prior <- c(prior(uniform(10, 8000), nlpar = 'a', lb = 10, ub = 8000),
           prior(uniform(1, 1000), nlpar = 'b', lb = 1, ub = 1000),
           prior(uniform(10^-3, 1), nlpar = 'c', lb = 10^-3, ub = 1))

prior_sigma <- c(prior(uniform(10, 8000), class = 'b', nlpar = 'a', lb = 10, ub = 8000),
                 prior(uniform(1, 1000), class = 'b', nlpar = 'b', lb = 1, ub = 1000),
                 prior(uniform(10^-3, 1), class = 'b', nlpar = 'c', lb = 10^-3, ub = 1),
                 prior(student_t(3, 0, 2.5), class = 'Intercept', dpar = 'sigma'))

#Ajustes
set.seed(123)

fit1 <- brm(formula = modelo_HB_teste1,
            data = dat_teste,
            family = brmsfamily('Gaussian'),
            prior = prior,
            control = list(adapt_delta = 0.95),
            iter = 4000,
            warmup = 2000,
            thin = 5,
            chains = 1,
            cores = 6,
            seed = 123)

fit2_teste <- brm(formula = modelo_HB_teste2,
                  data = dat_teste,
                  family = brmsfamily('Gaussian'),
                  prior = prior_sigma,
                  control = list(adapt_delta = 0.95),
                  iter = 4000,
                  warmup = 2000,
                  thin = 5,
                  chains = 1,
                  cores = 6,
                  seed = 123)

fit3_teste <- brm(formula = modelo_HB_teste3,
                  data = dat_teste,
                  family = brmsfamily('Gaussian'),
                  prior = prior_sigma,
                  control = list(adapt_delta = 0.95),
                  iter = 4000,
                  warmup = 2000,
                  thin = 5,
                  chains = 1,
                  cores = 6,
                  seed = 123)


# ---------------- VERIFICAÇÃO PRELIMINAR DOS AJUSTES ---------------
summary(fit1)
summary(fit2_teste)
summary(fit3_teste)

plot(fit1)
plot(fit2_teste)
plot(fit3_teste)

plot(conditional_effects(fit1), points = TRUE)
plot(conditional_effects(fit2_teste), points = TRUE)
plot(conditional_effects(fit3_teste), points = TRUE)

pp_check(fit1, ndraws = 100)
pp_check(fit2_teste, ndraws = 100)
pp_check(fit3_teste, ndraws = 100)

plot(predict(fit1))
plot(predict(fit2_teste))
plot(predict(fit3_teste))

LOO_result <- LOO(fit1, fit2_teste, fit3_teste)