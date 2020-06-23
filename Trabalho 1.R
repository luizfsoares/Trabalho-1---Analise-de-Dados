getwd()
setwd('C:/Users/preda/Documents/Estudo R')

#Lendo e atribuindo o conjunto de dados que ser� utilizado. Em environment � possivel checar a vari�vel criada e verificar a tabela completa
financialControl <- read.csv('dados/dadosATV.csv')

#---------------------------------------------------QUEST�O 1------------------------------------------------------------

#c�lculo da m�dia
mean(financialControl[["horas_logado"]]) #m�dia de 5.316013 horas nos �ltimos 30 dias.

#c�lculo da m�dia aparada excluindo 10% dos valores extremos adicionando o par�metro trim
mean(financialControl[["horas_logado"]], trim=0.1)

#c�lculo da mediana
median(financialControl[["horas_logado"]])

#A mediana � menor do que a m�dia, porque a m�dia leva em considera��o os outliears e � bastante afetada por elas, enquanto a mediana n�o � muito afetada por outliers
#Justamente porque a m�dia aparada � igual a m�dia normal, por�m, excluindo alguns valores de ambos os extremos, o qual podem ser outliers, se aproximando mais do valor da mediana.

#---------------------------------------------------QUEST�O 2------------------------------------------------------------

#c�lculo do desvio padr�o
sd(financialControl[["horas_logado"]])

#O desvio padr�o representa o quanto os dados desse atributo variam em torno da m�dia do mesmo

#---------------------------------------------------QUEST�O 3------------------------------------------------------------
