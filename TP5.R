# Carregamento e vetorização dos dados
dados <- read.csv("mistura.txt", header = FALSE)
dados <- unlist(dados)
dados <- as.vector(dados)


# Conjuntos de possíveis estimativas iniciais utilizados
conjunto1 <- c(0.5,-0.7261, 29.9, 4.811046, 4.811046)
conjunto2 <- c(0.1, 0,  1, sd(dados), sd(dados))
conjunto3 <- c(0.5, 6, 21, 1, 1)
conjunto4 <- c(0.4, 14, 22, 8, 4)


#' Title em
#'
#' @param dados : vetor de dados da mistura de distribuições
#' @param conjunto : estimativas iniciais para os parâmetros
#'
#' @return conjunto : estimativas atualizadas pelo Algoritmo EM
em <- function(dados, conjunto) {
  Ep = conjunto[1] * dnorm(dados, conjunto[2], sqrt(conjunto[4])) / (
    conjunto[1] * dnorm(dados, conjunto[2], sqrt(conjunto[4])) +
      (1 - conjunto[1]) * dnorm(dados, conjunto[3], sqrt(conjunto[5]))
  )
  conjunto[1] = mean(Ep)
  conjunto[2] = sum(Ep * dados) / sum(Ep)
  conjunto[3] = sum((1 - Ep) * dados) / sum(1 - Ep)
  conjunto[4] = sum(Ep * (dados - conjunto[2]) ^ 2) / sum(Ep)
  conjunto[5] = sum((1 - Ep) * (dados - conjunto[3]) ^ 2) / sum(1 - Ep)
  conjunto
}


iteracoes <<- 0
#' Title iteracao.helper
#'
#' @param dados : vetor de dados da mistura de distribuições
#' @param conjunto : estimativas iniciais para os parâmetros
#'
#' @return resultado: Data Frame com as estimativas finais após atingir o critério
#'                    e número de iterações realizadas

iteracao.helper <- function(dados, conjunto) {
  passo.em = em(dados, conjunto)
  for (i in 1:5) {
    if (abs(conjunto[i] - passo.em[i]) > 0.00000000000001) {
      conjunto = passo.em
      iteracao.helper(dados, conjunto)
      iteracoes <<- iteracoes + 1
    }
    else
      passo.em
  }
  estimativas <- c("p", "mu_1", "mu_2", "sigma_1^2", "sigma_2^2")
  resultado <- data.frame(estimativas, passo.em)
  return(append(resultado, iteracoes))
}

iteracao.helper(dados, conjunto1)
iteracao.helper(dados, conjunto2)
iteracao.helper(dados, conjunto3)
iteracao.helper(dados, conjunto4)


# Gráficos usando ggplot2 + gridExtra
p1 <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 12.218441927, sd = sqrt(27.519357947)),
    col = 'red'
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = 17.996614181, sd = sqrt(22.946778066)),
    col = 'blue'
  ) + ggtitle("Densidades das Normais \n Conjunto 2")
p2 <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 10.3834012, sd = sqrt(5.5454028)),
    col = 'red'
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = 19.9811271, sd = sqrt(8.4833039)),
    col = 'blue'
  ) + ggtitle("Densidades das Normais \n Conjunto 3")
p3 <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 13.3639992, sd = sqrt(13.1367257)),
    col = 'red'
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = 21.3882995, sd = sqrt(3.1261634)),
    col = 'blue'
  ) + ggtitle("Densidades das Normais \n Conjunto 4")
require(gridExtra)
grid.arrange(p1, p2, p3)
grid.arrange(p1, p2, p3, ncol = 3)
