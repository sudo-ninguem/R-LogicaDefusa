############################### LOGICA DIFUSA #############################################

## Utilizando a lógica difusa vamos aperfeiçoar um sistema especialista para diagnostico de asma

install.packages("sets", dependencies = T ) 

## O pacote para logica difusa em R é o (sets) além disso vamos baixar as dependencias do pacote

library(sets)

## Para fins didaticos e testar a lógica difusa vamos usar um caso de diagnostico de asma 

sets_options("universe", seq(1, 100, 1))

## neste comando estamos criando um (universo) de possíbilidades, póderiamos criar diversos universos

## mas estamos criando 1 universo que vai até 100 e vai de 1 em 1 (1,100,1)


variaveis <- set(
  Frequencia = fuzzy_partition(varnames = c( MenDSemanas  = 30, MaiDSemanas = 60, Diario = 70, Continuo=90), radius=20, FUN = fuzzy_cone),
  SABA = fuzzy_partition(varnames = c(MenDSemanas= 20, MaiDSemanas = 30, Diario = 70, DuasxDia=90), sd = 10),
  DebitoExp = fuzzy_partition(varnames = c(CinqOiten = 20, TrintTCinqCin = 30,  MaisTrintT = 70),  sd = 10),
  Classificacao = fuzzy_partition(varnames = c(Moderada = 20, AgudaGrave=40  , RiscoVida = 60), sd=10)
)

## Aqui nos estamos criando nossas váriaveis para o caso ficticio de diagnostico de asma sendo que estes dados também são ficticios

## O importante não é saber o que cada cado significa, mas basicamente são:

## Frequencia = Quantidade de vezes que uma pessoa da crise de asma na semana

## SABA = Quantidade de vezes que a pessoa usa a bombinha (O remédio chama SABA) por semana

## DebitoExp = Quantidade de falta de oxigêngio que a pessoa tem na semana

## Classificação = A classificação da asma da pessoa com base nas váriaveis anteriores

## Perceba que estamos criando as váriaveis na função (set) e com o método (fuzzy_partition)



regras <-
  set(
    fuzzy_rule( Frequencia %is% MenDSemanas && SABA %is% MenDSemanas && DebitoExp %is% CinqOiten, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MenDSemanas && SABA %is% MenDSemanas && DebitoExp %is% TrintTCinqCin, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MenDSemanas && SABA %is% MenDSemanas && DebitoExp %is% MaisTrintT, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MenDSemanas && SABA %is% MaiDSemanas && DebitoExp %is% MaisTrintT, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MaiDSemanas && SABA %is% MenDSemanas && DebitoExp %is% CinqOiten, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MaiDSemanas && SABA %is% MenDSemanas && DebitoExp %is% MaisTrintT, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MaiDSemanas && SABA %is% MaiDSemanas && DebitoExp %is% CinqOiten, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% MaiDSemanas && SABA %is% MaiDSemanas && DebitoExp %is% TrintTCinqCin, Classificacao %is% Moderada ),
    fuzzy_rule( Frequencia %is% Diario && SABA %is% Diario && DebitoExp %is% TrintTCinqCin, Classificacao %is% AgudaGrave ),
    fuzzy_rule( Frequencia %is% Diario && SABA %is% Diario && DebitoExp %is% CinqOiten, Classificacao %is% AgudaGrave ),
    fuzzy_rule( Frequencia %is% Diario && SABA %is% DuasxDia && DebitoExp %is% TrintTCinqCin, Classificacao %is% AgudaGrave ),
    fuzzy_rule( Frequencia %is% Diario && SABA %is% DuasxDia && DebitoExp %is% MaisTrintT, Classificacao %is% AgudaGrave ),
    fuzzy_rule( Frequencia %is% Continuo && SABA %is% Diario && DebitoExp %is% TrintTCinqCin, Classificacao %is% RiscoVida ),
    fuzzy_rule( Frequencia %is% Continuo && SABA %is% Diario && DebitoExp %is% MaisTrintT, Classificacao %is% RiscoVida ),
    fuzzy_rule( Frequencia %is% Continuo && SABA %is% DuasxDia && DebitoExp %is% TrintTCinqCin, Classificacao %is% RiscoVida ),
    fuzzy_rule( Frequencia %is% Continuo && SABA %is% DuasxDia && DebitoExp %is% MaisTrintT, Classificacao %is% RiscoVida )
  )
## Aqui estamos estabelecendo as regras para dizer quando uma asma é grave, media moderada etc. 

## Aqui também estamos usando dados ficticios e eles não são importantes em si, é só para completar o exemplo

## Mas perceba que essas regras também são criadas usando a função (set) é o método (fuzzy_rule)

## Perceba que ao criarmos váriaveis usamos o método (fuzzy_partition) e ao criar as regras usámos o método (fuse_rule)



sistema <-fuzzy_system(variaveis, regras)## Aqui estamos criando nosso sistema passando as váriaveis e as regras

sistema ## Com o nosso sistema criado podemos ver ele agora e ele vai trazer nada mais nada menos que as variaveis e regras

plot(sistema) ## Podemos gerar um grafico com o nosso sistema para ficar mais visível 



inferencia <- fuzzy_inference(sistema, list(Frequencia = 80, SABA = 70, DebitoExp = 80))

## Uma vez criado o nosso (sistema) podemos fazer inferencias com base em (casos) imagine então

## Um paciente que apresentou estes dados (frequencia 80, saba 70, debitoExp = 80)

## Sendo assim criamos uma váriavel e usando o método (fuzzy_inference) passamos como parametro

## O nosso sistema e em forma de lista o (caso) a ser analisado


inferencia ## Assim podemos ver a inferencia do caso específico 

plot(inferencia) ## Bem como gerar um grafico dele


gset_defuzzify(inferencia, "centroid") ## Após tudo feito podemos gerar também um valor central

## Para isso usamos o método (gset_defuzzify) passando como parametro a nossa inferencia e "centroid"

## Para obtermos um valor central o método mais utilizado é o "centroid"


plot(sistema$variables$Classificacao)
lines(inferencia, col= "blue", lwd=4)

## Por fim estamos gerando um grafico com o nosso sistema variaveis e classificação tudo junto

## E passando a nossa inferencia como uma linha da cor azul e grossura 4 sobre o grafico 

## Sendo assim obteremos, no nosso (sistema), a visualização da situação da nossa (inferencia)


sets_options("universe", NULL) ## Por fim estamos limpando nosso universo