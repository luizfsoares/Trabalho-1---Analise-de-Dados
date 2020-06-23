getwd()
setwd('C:/Users/preda/Documents/Estudo R')

#Lendo e atribuindo o conjunto de dados que será utilizado. Em environment é possivel checar a variável criada e verificar a tabela completa
financialControl <- read.csv('dados/dadosATV.csv')

#---------------------------------------------------QUESTÃO 1------------------------------------------------------------

#cálculo da média
mean(financialControl[["horas_logado"]]) #média de 5.316013 horas nos últimos 30 dias.

#cálculo da média aparada excluindo 10% dos valores extremos adicionando o parâmetro trim
mean(financialControl[["horas_logado"]], trim=0.1)

#cálculo da mediana
median(financialControl[["horas_logado"]])

#A mediana é menor do que a média, porque a média leva em consideração os outliears e é bastante afetada por elas, enquanto a mediana não é muito afetada por outliers
#Justamente porque a média aparada é igual a média normal, porém, excluindo alguns valores de ambos os extremos, o qual podem ser outliers, se aproximando mais do valor da mediana.

#---------------------------------------------------QUESTÃO 2------------------------------------------------------------

#cálculo do desvio padrão
sd(financialControl[["horas_logado"]])

#O desvio padrão representa o quanto os dados desse atributo variam em torno da média do mesmo

#---------------------------------------------------QUESTÃO 3------------------------------------------------------------

