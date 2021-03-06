#Aluno: Luiz Felipe Soares Cardoso
#Trabalho 1 - Introdu��o � An�lise de Dados


getwd()
setwd('C:/Users/preda/Documents/Estudo R')

#Lendo e atribuindo o conjunto de dados que ser� utilizado. Em environment � possivel checar a vari�vel criada e verificar a tabela completa
financialControl <- read.csv('dados/dadosATV.csv')

#---------------------------------------------------QUEST�O 1-------------------------------------------------------------------------------------------

#C�lculo da m�dia
mean(financialControl[["horas_logado"]]) #m�dia de 5.316013 horas nos �ltimos 30 dias.

#C�lculo da m�dia aparada excluindo 10% dos valores extremos adicionando o par�metro trim
mean(financialControl[["horas_logado"]], trim=0.1)

#C�lculo da mediana
median(financialControl[["horas_logado"]])

#A mediana � menor do que a m�dia, porque a m�dia leva em considera��o os outliears e � bastante afetada por elas, enquanto a mediana n�o � t�o sens�vel aos dados
#Justamente porque a m�dia aparada � igual a m�dia normal, por�m, excluindo alguns valores de ambos os extremos, o qual podem ser outliers, se aproximando mais do valor da mediana.

#---------------------------------------------------QUEST�O 2---------------------------------------------------------------------------------------------

#C�lculo do desvio padr�o
sd(financialControl[["horas_logado"]])

#O desvio padr�o representa o quanto os dados desse atributo variam em torno da m�dia do mesmo. Dessa forma, o valor do desvio padr�o
#representa cerca de 68% do valor da m�dia, ou seja, � um valor elevado para desvio pad�o, pois est� pr�ximo do valor da m�dio.
#Isso indica que por ter um desvio padr�o um pouco alto, os valores dos dados est�o mais espalhados, s�o mais heterog�neos.

#---------------------------------------------------QUEST�O 3-------------------------------------------------------------------------------------------

#Os quartis separam os dados de 25% em 25%. Logo, o primeiro quartil diz respeito a 25%, o segundo � 50% (pode-se dizer que tamb�m � a mediana)
#e o terceiro corresponde a 75%

#PRIMEIRO QUARTIL
#25% dos usu�rios ficaram logados no app at� 2.22 horas no ultimo mes
quantile(financialControl[["horas_logado"]], probs = 0.25)

#SEGUNDO QUARTIL
#50% dos usu�rios ficaram logados no app at� 4.62 horas no ultimo mes
quantile(financialControl[["horas_logado"]], probs = 0.50) #� igual a mediana.

#TERCEIRO QUARTIL
#75% dos usu�rios ficaram logados no app at� 7.9225 horas no ultimo mes
quantile(financialControl[["horas_logado"]], probs = 0.75)

#vetor com os tr�s quartis
quantile(financialControl[["horas_logado"]], probs =c(.25, .50, .75))

#� possivel avaliar a dispers�o e a tendencia central do conjunto de dados.
#Uma das interpreta��es poss�veis � que 25% das pessoas passaram 2.22 horas no ultimo m�s.
#Tamb�m outros 25% passam mais do que 7.9225 horas no ultimo m�s.

#---------------------------------------------------QUEST�O 4--------------------------------------------------------------------------------------------

#BoxPlot da vari�vel horas logado
#� possivel visualizar que os dados est�o mais dispersos ap�s a mediana.
boxplot(financialControl[["horas_logado"]], xlab = ("Horas Logado"), border = "brown", horizontal = TRUE, col = "green")


#a) Com a plotagem desse gr�fico � possivel visualizar que h� um elemento bem fora dos limites superiores e inferiores, ou seja, h� outlier.
#b) Este outlier indica que existe um usu�rio que esteve logado no aplicativo por mais de 40 horas no �ltimo m�s.

#---------------------------------------------------QUEST�O 5--------------------------------------------------------------------------------------------

breaks <- seq(from=min(financialControl[["horas_logado"]]), to=max(financialControl[["horas_logado"]]), length=4)
breaks #0.08, 13.46333, 26.84667, 40.23  (4 intervalos)

#Tabela de frequ�ncia dividido em 4 intervalos
hl_freq <- cut(financialControl[["horas_logado"]], breaks=breaks, right=TRUE, include.lowest = TRUE)
financialControl['HlFreq'] <- hl_freq
table(hl_freq)


#Forma passada em classe de representar histograma
hist(financialControl[["horas_logado"]], breaks=breaks)

#INTERPRETA��O (alguma informa��es geradas)
#Quase nenhum usu�rio passou mais de 13.5h logados no aplicativo nos �ltimos 30 dias.
#Cerca de 98% dos usu�rios passaram de 0.08h at� 13.5h logados no aplicativo.
#� um gr�fico com dados assim�tricos a esquerda
#Em quest�o de horas logadas, os usu�rios se concentram a esquerda do histograma, nas menores horas.
#Os usu�rios usam pouco o aplicativo ou fazem loguins r�pidos.


#Outra forma!!
#Plotar histograma com ggplot2 vai plotar por padr�o 30 barras, por�m � poss�vel modificar com o par�metro bins
ggplot(data = financialControl, aes(x= horas_logado)) + #aes indica o que ser� botar nos eixos
  geom_histogram(fill = 'blue', color = 'black', bins =4, alpha = 0.5) + ylab("Frequ�ncia") + xlab("Quantidade de Horas Logado")


#---------------------------------------------------QUEST�O 6---------------------------------------------------------------------------------------------------

#Checar correla��o entre duas variaveis: horas logado no app e no app do Banco do Brasil
#usando a biblioteca ggplot2

graf_disp <- ggplot(financialControl, aes(x=horas_logado, y=horas_logado_BB)) + #+ indica ao R que h� outros elementos a serem adicionados
                      geom_smooth(method="loess", se = F) +
                      geom_point() +
                      labs(subtitle = "Correla��o entre as vari�veis", x = "Horas Logadas no App", y = "Horas Logadas no App do BB")
plot(graf_disp)


#Gr�fico de dispers�o com C�digo da Aula
plot(financialControl$horas_logado, financialControl$horas_logado_BB, xlab="Horas Logadas no App", ylab="Horas Logadas no App do BB")

#AN�LISE

#Ao analisar os pontos do gr�fico de dispers�o e a curva tra�ada � poss�vel compreender que as duas vari�veis s�o inversamente proporcionais,
#... de modo que os maiores valores para o eixo y (horas logadas no aplicativo do Banco do Brasil) representa tamb�m os menores
#... valores para o eixo X (horas logadas no aplicativo). Portanto, � l�gico supor que o cliente que usa bastante um aplicativo, usa bem
#... bem menos o outro, e vice-versa(Correla��o Negativa).

#---------------------------------------------------QUEST�O 7---------------------------------------------------------------------------------------------------
#Compartimenta��o Hexagonal

ggplot(financialControl, aes(x=horas_logado, y=horas_logado_BB)) + 
      stat_binhex(colour="white") +
      theme_bw() +
      scale_fill_gradient(low="gray", high = "red") +
      labs(x="Horas Logadas no App", y="Horas Logadas no App do BB")

#AN�LISE

#� poss�vel visualizar algo bem semelhante � quest�o anterior. Onde o vermelho est� mais forte � justamente nas partes iniciais do eixo X.
#Isso mostra que, j� que o eixo X representa a quantidade de horas logadas no APP, a maior quantidade de pessoas que usaram POUCO o aplicativo
#...em contrapartida usaram MUITO o aplicativo do Banco do Brasil, pois os vermelhos de tonalidade mais forte (maior quantidade de registros),
#...est�o concentrados para os menores valores de X (horas do app) e maiores valores de Y (horas do BB), ou seja, a medida que os valores de X
#...v�o se aproximando de 0, a quantidade de registros que possuem valores altos para o Y aumentam.

#---------------------------------------------------QUEST�O 8------------------------------------------------------------------------------------------------

#Tabela de Contig�ncia
#Relacionar duas vari�veis qualitativas, no caso, canal e cartao_cred.

tbl <- CrossTable(financialControl$canal, financialControl$cartao_cred, prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE,
                  xlab="Sistema Operacional:", ylab="Integrou cart�o?")
tbl

#AN�LISE

#70% das pessoas (2101 pessoas) utilizam celular com sistema android e entre estas, 61,5% (1292 pessoas) teve pelo menos um cart�o integrado.
#Por outro lado, o restante (899 pessoas) usam sistema iOS
#Dessa forma, a porcentagem de pessoas que utilizam android � maior e a porcentagem de cart�es integrados tamb�m � maior no android.

#---------------------------------------------------QUEST�O 9-------------------------------------------------------------------------------------------------

#Boxplot com duas vari�veis

boxplot(horas_logado ~ canal, data = financialControl, ylim=c(0,50), ylab = "Horas Logado no App", xlab = "Sistema Operacional")

#AN�LISE

#Em uma primeira vista, entre os usu�rios de android e ios, os gr�ficos de boxplot est�o extremamente semelhantes, por�m, � possivel ver
#que a caixa do iOS � um pouco mais achatada, tendo as seguintes caracter�scitas. as linhas primeiro (at� 25% dos dados) e terceiro (at� 75% dos dados)
#s�o menores para o gr�fico do ios. Isso implica que os valores est�o mais concentrados nas menores quantidades de horas logado no app, ou seja, at� 25%
#e tamb�m at� 75% dos clientes que utilizam o sistema iOS passaram um POUCO menos de tempo logado no app do que os usu�rios de Android.
#Por isso o sistema Android apresentou um pouco mais de atividade dos clientes nos �ltimos 30 dias.
#Em rela��o aos limites e a mediana aparentam ser bem semelhantes, a diferen�a ocorre mais entre o segundo e terceiro quartil, onde
#a faixa � maior no android e por isso os dados est�o mais dispersos.

#---------------------------------------------------QUEST�O 10----------------------------------------------------------------------------------------------

#Gr�fico violino: melhoria do BoxPLot anterior

ggplot(data = financialControl, aes(canal, horas_logado)) + 
      ylim(0, 50) +
      geom_violin(draw_quantiles = c(.25, .50, .75), linetype=2) +
      geom_violin(fill=NA, size=1.1)
      theme_bw() + 
      labs(x="Sistema Operacional", y="Horas Logado no App")
      
#AN�LISE
      
#H� uma densidade maior de pessoas no IOS que ficaram logadas no aplicativo pelas horas representadas pela mediana, que � semelhante para ambos.
#Enquanto � poss�vel identificar uma densidade levemente maior no sistema Android acima do terceiro quartil.