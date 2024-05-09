# ANALISE DESCRITIVA

library(readxl)
library(ggplot2)
library(corrplot)
library(cowplot)
library(skimr)
library(lavaan)
library(DMwR2)
library(semTools)
library(semPlot)
library(openxlsx)
library(rstan)
library(blavaan)
library(brms)
library(seminr)
library(blavaan)
library(bayesplot)
library(posterior)

# Definir o caminho do arquivo
file_path <- "/Users/nuno/faculdade/bayesiana/dataset.xlsx"

# Ler o arquivo Excel
dataset <- read_excel(file_path)

# Exibir as primeiras linhas do conjunto de dados
head(dataset)

# Exibir um resumo do conjunto de dados
summary(dataset)

# Exibir a estrutura do conjunto de dados 
str(dataset)

# Exibir as dimensões do conjunto de dados -> 213 observações e 29 variáveis
dim(dataset)

# Exibir os nomes das variáveis
names(dataset)

# Ver onde estão os valores omissos
missing_values <- lapply(dataset, function(x) which(is.na(x)))

# Imprimir os índices dos valores omissos
print(missing_values)

# Substituir os valores omissos por NA
dataset[is.na(dataset)] <- NA

# Verificar dataset após substituição
dataset

head(dataset)
dim(dataset)  # Verificar dimensões
summary(dataset)
sapply(dataset, function(x) length(unique(x)))
sum(is.na(dataset))  # Verificar valores omissos

# Imputar valores omissos usando imputação KNN
dataset <- knnImputation(dataset, k = 5)  # Ajustar k se necessário

# Plotar boxplot para inspeção visual das distribuições das variáveis
boxplot(dataset[,3:29])

# Calcular e plotar correlações
correlation <- cor(dataset, use = "pairwise.complete.obs")  # Lidar com dados omissos
corrplot(correlation)

# Encontrar pares altamente correlacionados
threshold <- 0.7
high_correlation_idx <- which(correlation > threshold & correlation < 1, arr.ind = TRUE)
top_correlations <- list()

for (i in 1:nrow(high_correlation_idx)) {
  var1 <- rownames(correlation)[high_correlation_idx[i, 1]]
  var2 <- rownames(correlation)[high_correlation_idx[i, 2]]
  corr_value <- correlation[high_correlation_idx[i, 1], high_correlation_idx[i, 2]]
  top_correlations[[paste(var1, "&", var2)]] <- corr_value
}

#################################################################################################

# ARTIGO

# Carregar o pacote seminr
# 

# mm <- constructs(
#   composite("CI", multi_items("CI", 1:6)),
#   composite("PE", multi_items("PE", 1:6)),
#   composite("AUT", multi_items("aut", 1:4)),
#   composite("COM", multi_items("com", 1:4)),
#   composite("REL", multi_items("rel", 1:4)),
#   composite("Achievement", multi_items("ach", 1:2), weights = mode_B)
# )

# # Definindo o modelo estrutural
# sm <- relationships(
#   paths(from = "AUT", to = c("CI", "PE", "Achievement")),
#   paths(from = "COM", to = c("CI", "PE", "Achievement")),
#   paths(from = "REL", to = c("CI", "PE", "Achievement")),
#   paths(from = "CI", to = "Achievement"),
#   paths(from = "PE", to = "Achievement")
# )

# # Ajustar o modelo PLS-SEM
# modelo_pls <- estimate_pls(data = dataset, measurement_model = mm, structural_model = sm)

# # Sumarizar os resultados
# summary(modelo_pls)

################################################################################

# PARTE 3 - modelo de medida usando Análise Fatorial Confirmatória


# Definição das variáveis latentes
model_cfa <- '
  CI =~ CI1 + CI2 + CI3 + CI4 + CI5 + CI6
  PE =~ PE1 + PE2 + PE3 + PE4 + PE5 + PE6
  AUT =~ aut1 + aut2 + aut3 + aut4
  COM =~ com1 + com2 + com3 + com4
  REL =~ rel1 + rel2 + rel3 + rel4
  Achievement =~ ach1 + ach2
'

# Ajustar o modelo CFA Bayesiano usando bcfa do blavaan
modelfit_cfa <- bcfa(
  model = model_cfa,
  data = dataset,
  std.lv = TRUE,
  n.chains = 3,
  burnin = 5000,
  sample = 1000,
  target = "stan",
  dp = dpriors(b = "normal(0,10)", theta = "gamma(1, 0.5)")
)

# Sumário dos resultados
(summary(modelfit_cfa, standardized = TRUE, rsquare = TRUE, neff = TRUE, postmedian = TRUE))

# Plot prior normal para coeficientes
curve(dnorm(x, mean = 0, sd = 10), from = -30, to = 30, main = "Distribuição a priori para Coeficientes (Normal(0,10))", xlab = "Valor do Coeficiente", ylab = "Densidade")

# Plot prior gamma para desvios padrão
curve(dgamma(x, shape = 1, rate = 0.5), from = 0, to = 20, main = "Distribuição a priori para Desvios Padrão (Gamma(1,0.5))", xlab = "Valor do Desvio Padrão", ylab = "Densidade")


# Extract posterior samples
# posterior_samples <- posterior_samples(modelfit_cfa, variables = c("b", "theta"))

# # Plot histogram or density for a specific coefficient
# hist(posterior_samples$b[,1], main = "Posterior Distribution for a Coefficient", xlab = "Coefficient Value", breaks = 30, probability = TRUE)
# lines(density(posterior_samples$b[,1]), col = "blue")

# # Plot histogram or density for a specific standard deviation
# hist(posterior_samples$theta[,1], main = "Posterior Distribution for a Standard Deviation", xlab = "Standard Deviation Value", breaks = 30, probability = TRUE)
# lines(density(posterior_samples$theta[,1]), col = "blue")
# # Número total de variáveis
# total_vars <- 26

# # Número de variáveis por plot
# vars_per_plot <- 6

# # Criar uma sequência de gráficos para cada grupo de variáveis
# for (i in seq(1, total_vars, by = vars_per_plot)) {
#   upper_bound <- min(i + vars_per_plot - 1, total_vars)
#   plot(modelfit_cfa, plot.type = "trace", pars = i:upper_bound)
# }

####################################################################################

# Exemplo

# x <- seq(from = -90, to = 90, by = 1)
# data <- dnorm(x, mean = 30, sd = 10)
# prior <- dnorm(x, mean = 10, sd = 5)
# posterior <- 0.5 * dnorm(x, mean = 10, sd = 5) + 0.5 * dnorm(x, mean = 30, sd = 10)

# plot(x, prior, type = "l", col = "red")
# lines(x, posterior, type = "l", col = "green")
# lines(x, data , type = "l", col = "blue")