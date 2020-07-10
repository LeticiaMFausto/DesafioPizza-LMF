      *Divis�o de identifica��o do programa
       identification division.
       program-id. "DesafioPizza".
       author. "Let�cia M Fausto".
       installation. "PC".
       date-written. 10/07/2020.
       date-compiled. 10/07/2020.



      *Divis�o para configura��o do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *-----Declara��o dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *Declara��o de vari�veis
       data division.

      *----Variaveis de arquivos
       file section.


      *----Variaveis de trabalho
       working-storage section.

      *cria��o da tabela/ visualiza��o
       01  relatorio  occurs  20.
           05 nome                                 pic x(15)
                                                   value spaces.
           05 filler                               pic x(03)
              value " - ".
           05 diametro                             pic 9(03).
           05 filler                               pic x(03)
              value " - ".
           05 preco                                pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 preco_cm2                            pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 diferenca_rel                        pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".

           05 porcentagem                          pic 9(03)V99.

      *Vari�veis de controle
       77  ind                                     pic 9(02).
       77  controle                                pic x(10).
       77  aux                                     pic 9(04)V99.
       77  menu                                    pic X(01).
      *Vari�veis de calculo
       77  pi                                      Pic 9(01)V999999.
       77  raio                                    pic 9(02)V999.
       77  area_                                   pic 9(02)V999.





      *----Variaveis para comunica��o entre programas
       linkage section.


      *----Declara��o de tela
       screen section.


      *Declara��o do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      * Inicilizacao de variaveis, abertura de arquivos
      * procedimentos que serao realizados apenas uma vez

       inicializa section.

           move  3,141592   to     Pi
           move  'trocou'   to     controle
           move   'S'       to     menu

           .
       inicializa-exit.
           exit.


       processamento section.

      *La�o de controle para entrada de valores.
           move 0 to ind
           perform until ind > 20
                   or    menu <> 'S'

               display erase
               add 1 to ind
               display "---------- Variacao de Precos ----------"
      -        at 0120
               display " "

               if ind > 20 then
                   display "Vc atingiu o limite de 20 pizzas"
               else
      *Entrada de valores.
                   display "Informe o nome da " ind " pizza "
                   accept nome(ind)

                   display "Informe o diametro "
                   accept diametro(ind)

                   display "Informe o preco "
                   accept preco(ind)
               end-if

               display "Voc� deseja cadastrar mais alguma pizza? 'S'im o
      -                "u 'N'ao"
               accept menu


           end-perform



      *Calculo do 'raio' e da 'area' para descobrir preco_cm2
           perform calculo

      *Ordena��o de 'melhor para pior' sobre os pre�os das pizzas por
      *-cm2
           perform ordenacao

      *Calculo da 'diferenca_rel' para descobrir porcentagem de diferen�
      *-a entre pre�os

           perform calculo2

      *Apresenta��o dos indices, depois de ordenados.
           perform varying ind from 1 by 1 until ind > 20
                                              or nome(ind) = spaces

               display relatorio(ind)
           end-perform


           .
       processamento-exit.
           exit.

       calculo section.  .

           move 1 to ind
           perform until ind = 20
                   or    nome(ind) = spaces
      *Para calcular o preco_mc2 � necessario descobrir a �rea total do
      *circulo.
               compute raio  = diametro(ind) / 2
               compute area_ = pi * (raio * raio)

               compute preco_cm2(ind) = preco(ind) / area_

               add 1 to ind
               end-perform

           .
       calculo-exit.
           exit.


       calculo2 section.

           move 1 to ind
      *La�o de controle para quando o ind for maior que a tabela ou n�o
      *tiver nenhum valor na proxima posi��o da tabela.
           perform until ind = 20
                   or    nome(ind + 1)= spaces

               compute diferenca_rel(ind + 1) =
                       preco_cm2(ind + 1) - preco_cm2(ind)


               compute porcentagem(ind + 1) = (diferenca_rel(ind + 1)
                                              *100)/ preco_cm2(ind)

               add 1 to ind
           end-perform



           .
       calculo2-exit.
           exit.

       ordenacao section.

           perform until controle <> 'trocou'

               move 1 to ind
               move 'N trocou' to controle
      *Metodo Bolha de ordena��o.
               perform until ind = 20
                       or    nome(ind + 1) = space
                   if preco_cm2(ind) > preco_cm2(ind + 1) then
      *Aqui foi necess�rio criar uma vari�vel de controle chamada aux,
      *para n�o perder nenhum valor. Assim apenas foi tudo realocado
                       move preco_cm2(ind + 1) to aux
                       move preco_cm2(ind) to preco_cm2(ind + 1)
                       move aux to preco_cm2(ind)
      *Nesse move, se o cursor n�o entrar no la�o, ele ainda vai ser
      *igual a 'N trocou', quer dizer que tudo foi ordenado e saira do
      *laco, caso n�o tenha suprido nenhuma das condi��es vindas depois
                       move 'trocou' to controle
                   end-if
                   add 1 to ind
               end-perform
           end-perform



           .
       ordenacao-exit.
           exit.

       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.













