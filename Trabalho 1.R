#Aluno: Luiz Felipe Soares Cardoso
#Trabalho 1 - Introdução à Análise de Dados


getwd()
setwd('C:/Users/preda/Documents/Estudo R')

#Lendo e atribuindo o conjunto de dados que será utilizado. Em environment é possivel checar a variável criada e verificar a tabela completa
financialControl <- read.csv('dados/dadosATV.csv')

#---------------------------------------------------QUESTÃO 1-------------------------------------------------------------------------------------------

#Cálculo da média
mean(financialControl[["horas_logado"]]) #média de 5.316013 horas nos últimos 30 dias.

#Cálculo da média aparada excluindo 10% dos valores extremos adicionando o parâmetro trim
mean(financialControl[["horas_logado"]], trim=0.1)

#Cálculo da mediana
median(financialControl[["horas_logado"]])

#A mediana é menor do que a média, porque a média leva em consideração os outliears e é bastante afetada por elas, enquanto a mediana não é tão sensível aos dados
#Justamente porque a média aparada é igual a média normal, porém, excluindo alguns valores de ambos os extremos, o qual podem ser outliers, se aproximando mais do valor da mediana.

#---------------------------------------------------QUESTÃO 2---------------------------------------------------------------------------------------------

#Cálculo do desvio padrão
sd(financialControl[["horas_logado"]])

#O desvio padrão representa o quanto os dados desse atributo variam em torno da média do mesmo. Dessa forma, o valor do desvio padrão
#representa cerca de 68% do valor da média, ou seja, é um valor elevado para desvio padão, pois está próximo do valor da médio.
#Isso indica que por ter um desvio padrão um pouco alto, os valores dos dados estão mais espalhados, são mais heterogêneos.

#---------------------------------------------------QUESTÃO 3-------------------------------------------------------------------------------------------

#Os quartis separam os dados de 25% em 25%. Logo, o primeiro quartil diz respeito a 25%, o segundo é 50% (pode-se dizer que também é a mediana)
#e o terceiro corresponde a 75%

#PRIMEIRO QUARTIL
#25% dos usuários ficaram logados no app até 2.22 horas no ultimo mes
quantile(financialControl[["horas_logado"]], probs = 0.25)

#SEGUNDO QUARTIL
#50% dos usuários ficaram logados no app até 4.62 horas no ultimo mes
quantile(financialControl[["horas_logado"]], probs = 0.50) #é igual a mediana.

#TERCEIRO QUARTIL
#75% dos usuários ficaram logados no app até 7.9225 horas no ultimo mes
quantile(financialControl[["horas_logado"]], probs = 0.75)

#vetor com os três quartis
quantile(financialControl[["horas_logado"]], probs =c(.25, .50, .75))

#É possivel avaliar a dispersão e a tendencia central do conjunto de dados.
#Uma das interpretações possíveis é que 25% das pessoas passaram 2.22 horas no ultimo mês.
#Também outros 25% passam mais do que 7.9225 horas no ultimo mês.

#---------------------------------------------------QUESTÃO 4--------------------------------------------------------------------------------------------

#BoxPlot da variável horas logado
#É possivel visualizar que os dados estão mais dispersos após a mediana.
boxplot(financialControl[["horas_logado"]], xlab = ("Horas Logado"), border = "brown", horizontal = TRUE, col = "green")


#a) Com a plotagem desse gráfico é possivel visualizar que há um elemento bem fora dos limites superiores e inferiores, ou seja, há outlier.
#b) Este outlier indica que existe um usuário que esteve logado no aplicativo por mais de 40 horas no último mês.

#---------------------------------------------------QUESTÃO 5--------------------------------------------------------------------------------------------

breaks <- seq(from=min(financialControl[["horas_logado"]]), to=max(financialControl[["horas_logado"]]), length=4)
breaks #0.08, 13.46333, 26.84667, 40.23  (4 intervalos)

#Tabela de frequência dividido em 4 intervalos
hl_freq <- cut(financialControl[["horas_logado"]], breaks=breaks, right=TRUE, include.lowest = TRUE)
financialControl['HlFreq'] <- hl_freq
table(hl_freq)


#Forma passada em classe de representar histograma
hist(financialControl[["horas_logado"]], breaks=breaks)

#INTERPRETAÇÃO (alguma informações geradas)
#Quase nenhum usuário passou mais de 13.5h logados no aplicativo nos últimos 30 dias.
#Cerca de 98% dos usuários passaram de 0.08h até 13.5h logados no aplicativo.
#É um gráfico com dados assimétricos a esquerda
#Em questão de horas logadas, os usuários se concentram a esquerda do histograma, nas menores horas.
#Os usuários usam pouco o aplicativo ou fazem loguins rápidos.


#Outra forma!!
#Plotar histograma com ggplot2 vai plotar por padrão 30 barras, porém é possível modificar com o parâmetro bins
ggplot(data = financialControl, aes(x= horas_logado)) + #aes indica o que será botar nos eixos
  geom_histogram(fill = 'blue', color = 'black', bins =4, alpha = 0.5) + ylab("Frequência") + xlab("Quantidade de Horas Logado")


#---------------------------------------------------QUESTÃO 6---------------------------------------------------------------------------------------------------

#Checar correlação entre duas variaveis: horas logado no app e no app do Banco do Brasil
#usando a biblioteca ggplot2

graf_disp <- ggplot(financialControl, aes(x=horas_logado, y=horas_logado_BB)) + #+ indica ao R que há outros elementos a serem adicionados
                      geom_smooth(method="loess", se = F) +
                      geom_point() +
                      labs(subtitle = "Correlação entre as variáveis", x = "Horas Logadas no App", y = "Horas Logadas no App do BB")
plot(graf_disp)


#Gráfico de dispersão com Código da Aula
plot(financialControl$horas_logado, financialControl$horas_logado_BB, xlab="Horas Logadas no App", ylab="Horas Logadas no App do BB")

#ANÁLISE

#Ao analisar os pontos do gráfico de dispersão e a curva traçada é possível compreender que as duas variáveis são inversamente proporcionais,
#... de modo que os maiores valores para o eixo y (horas logadas no aplicativo do Banco do Brasil) representa também os menores
#... valores para o eixo X (horas logadas no aplicativo). Portanto, é lógico supor que o cliente que usa bastante um aplicativo, usa bem
#... bem menos o outro, e vice-versa(Correlação Negativa).

#---------------------------------------------------QUESTÃO 7---------------------------------------------------------------------------------------------------
#Compartimentação Hexagonal

ggplot(financialControl, aes(x=horas_logado, y=horas_logado_BB)) + 
      stat_binhex(colour="white") +
      theme_bw() +
      scale_fill_gradient(low="gray", high = "red") +
      labs(x="Horas Logadas no App", y="Horas Logadas no App do BB")

#ANÁLISE

#É possível visualizar algo bem semelhante à questão anterior. Onde o vermelho está mais forte é justamente nas partes iniciais do eixo X.
#Isso mostra que, já que o eixo X representa a quantidade de horas logadas no APP, a maior quantidade de pessoas que usaram POUCO o aplicativo
#...em contrapartida usaram MUITO o aplicativo do Banco do Brasil, pois os vermelhos de tonalidade mais forte (maior quantidade de registros),
#...estão concentrados para os menores valores de X (horas do app) e maiores valores de Y (horas do BB), ou seja, a medida que os valores de X
#...vão se aproximando de 0, a quantidade de registros que possuem valores altos para o Y aumentam.

#---------------------------------------------------QUESTÃO 8------------------------------------------------------------------------------------------------

#Tabela de Contigência
#Relacionar duas variáveis qualitativas, no caso, canal e cartao_cred.

tbl <- CrossTable(financialControl$canal, financialControl$cartao_cred, prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE,
                  xlab="Sistema Operacional:", ylab="Integrou cartão?")
tbl

#ANÁLISE

#70% das pessoas (2101 pessoas) utilizam celular com sistema android e entre estas, 61,5% (1292 pessoas) teve pelo menos um cartão integrado.
#Por outro lado, o restante (899 pessoas) usam sistema iOS
#Dessa forma, a porcentagem de pessoas que utilizam android é maior e a porcentagem de cartões integrados também é maior no android.

#---------------------------------------------------QUESTÃO 9-------------------------------------------------------------------------------------------------

#Boxplot com duas variáveis

boxplot(horas_logado ~ canal, data = financialControl, ylim=c(0,50), ylab = "Horas Logado no App", xlab = "Sistema Operacional")

#ANÁLISE

#Em uma primeira vista, entre os usuários de android e ios, os gráficos de boxplot estão extremamente semelhantes, porém, é possivel ver
#que a caixa do iOS é um pouco mais achatada, tendo as seguintes caracteríscitas. as linhas primeiro (até 25% dos dados) e terceiro (até 75% dos dados)
#são menores para o gráfico do ios. Isso implica que os valores estão mais concentrados nas menores quantidades de horas logado no app, ou seja, até 25%
#e também até 75% dos clientes que utilizam o sistema iOS passaram um POUCO menos de tempo logado no app do que os usuários de Android.
#Por isso o sistema Android apresentou um pouco mais de atividade dos clientes nos últimos 30 dias.
#Em relação aos limites e a mediana aparentam ser bem semelhantes, a diferença ocorre mais entre o segundo e terceiro quartil, onde
#a faixa é maior no android e por isso os dados estão mais dispersos.

#---------------------------------------------------QUESTÃO 10----------------------------------------------------------------------------------------------

#Gráfico violino: melhoria do BoxPLot anterior

ggplot(data = financialControl, aes(canal, horas_logado)) + 
      ylim(0, 50) +
      geom_violin(draw_quantiles = c(.25, .50, .75), linetype=2) +
      geom_violin(fill=NA, size=1.1)
      theme_bw() + 
      labs(x="Sistema Operacional", y="Horas Logado no App")
      
#ANÁLISE
      
#Há uma densidade maior de pessoas no IOS que ficaram logadas no aplicativo pelas horas representadas pela mediana, que é semelhante para ambos.
#Enquanto é possível identificar uma densidade levemente maior no sistema Android acima do terceiro quartil.