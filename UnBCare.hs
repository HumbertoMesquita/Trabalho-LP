module UnBCare where

import ModeloDados

{-
 

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.


comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento = undefined


-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento medicamentoAdicionar quantidadeAdicionar estoqueMedicamentos
    | existeNoEstoque medicamentoAdicionar estoqueMedicamentos = adicionarAoEstoque (medicamentoAdicionar, quantidadeAdicionar) estoqueMedicamentos
    | otherwise = (medicamentoAdicionar, quantidadeAdicionar) : estoqueMedicamentos
    where
        existeNoEstoque :: Medicamento -> EstoqueMedicamentos -> Bool
        existeNoEstoque _ [] = False
        existeNoEstoque medicamento ((medicamentoAtual, _) : restanteEstoque)
            | medicamento == medicamentoAtual = True
            | otherwise = existeNoEstoque medicamento restanteEstoque

        adicionarAoEstoque :: (Medicamento, Quantidade) -> EstoqueMedicamentos -> EstoqueMedicamentos
        adicionarAoEstoque _ [] = []
        adicionarAoEstoque (medicamento, quantidade) ((medicamentoAtual, quantidadeAtual) : restanteEstoque)
            | medicamento == medicamentoAtual = (medicamento, quantidadeAtual + quantidade) : restanteEstoque
            | otherwise = (medicamentoAtual, quantidadeAtual) : adicionarAoEstoque (medicamento, quantidade) restanteEstoque



{-comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento medADD qntADD [] = [(medADD, qntADD)]
comprarMedicamento medADD qntADC (estoque@(med, qnt) : restante)
    | med == medADD = (med, qnt + qntADC) : restante
    | otherwise     = estoque : comprarMedicamento medADD qntADC restante
-}
{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento medicamento estoqueMedicamentos
    | existeNoEstoque medicamento estoqueMedicamentos = Just (removerDoEstoque medicamento estoqueMedicamentos)
    | otherwise = Nothing
    where
        existeNoEstoque :: Medicamento -> EstoqueMedicamentos -> Bool
        existeNoEstoque _ [] = False
        existeNoEstoque medicamento ((medicamentoAtual, _) : restanteEstoque)
            | medicamento == medicamentoAtual = True
            | otherwise = existeNoEstoque medicamento restanteEstoque

        removerDoEstoque :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
        removerDoEstoque _ [] = []
        removerDoEstoque medicamento ((medicamentoAtual, quantidadeAtual) : restanteEstoque)
            | quantidadeAtual == 0 = (medicamentoAtual, quantidadeAtual) : restanteEstoque
            | medicamento /= medicamentoAtual = (medicamentoAtual, quantidadeAtual) : removerDoEstoque medicamento restanteEstoque
            | medicamento == medicamentoAtual = (medicamento, quantidadeAtual - 1) : restanteEstoque


{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento = undefined
-}


consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento medicamento ((medicamentoAtual, quantidadeAtual) : restanteEstoque)
    | medicamento == medicamentoAtual = quantidadeAtual
    | otherwise = consultarMedicamento medicamento restanteEstoque


{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos = undefined


-}


demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos (receitaAtual : restanteReceituario) =
    ordenarPorMedicamento ((medicamentoAtual, quantidadeDemandada) : restanteDemanda)
  where
    medicamentoAtual = primeiroElemento receitaAtual
    quantidadeDemandada = comprimento (segundoElemento receitaAtual)
    restanteDemanda = demandaMedicamentos restanteReceituario

    ordenarPorMedicamento :: EstoqueMedicamentos -> EstoqueMedicamentos
    ordenarPorMedicamento [] = []
    ordenarPorMedicamento (x : xs) =
        let menores = [y | y <- xs, primeiroElemento y <= primeiroElemento x]
            maiores = [y | y <- xs, primeiroElemento y > primeiroElemento x]
        in ordenarPorMedicamento menores ++ [x] ++ ordenarPorMedicamento maiores

    primeiroElemento :: (a, b) -> a
    primeiroElemento (x, _) = x

    segundoElemento :: (a, b) -> b
    segundoElemento (_, y) = y

    comprimento :: [a] -> Int
    comprimento [] = 0
    comprimento (_ : xs) = 1 + comprimento xs


    

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:


receituarioValido :: Receituario -> Bool
receituarioValido = undefined

planoValido :: PlanoMedicamento -> Bool
planoValido = undefined

import HelpT2

import Data.List(sortOn, sort)
 -}




ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (pivo : restante) =
    ordenar [x | x <- restante, x <= pivo]
    ++ [pivo]
    ++ ordenar [x | x <- restante, x > pivo]

receituarioValido :: Receituario -> Bool
receituarioValido receituario
    | not (or (remediosRepetidos receituario)) &&
      (receituario == ordenarPorMedicamento receituario) &&
      not (horariosRepetidos receituario) &&
      (listaDeHorarios receituario == ordenarHorarios (listaDeHorarios receituario)) = True
    | otherwise = False
    where
        remediosRepetidos :: Receituario -> [Bool]
        remediosRepetidos [] = []
        remediosRepetidos (prescricaoAtual : restanteReceituario) =
            contemMedicamentoIgual (fst prescricaoAtual) (listaDeMedicamentos restanteReceituario)
            : remediosRepetidos restanteReceituario

        listaDeMedicamentos :: Receituario -> [Medicamento]
        listaDeMedicamentos [] = []
        listaDeMedicamentos (prescricaoAtual : restanteReceituario) =
            fst prescricaoAtual : listaDeMedicamentos restanteReceituario

        contemMedicamentoIgual :: Medicamento -> [Medicamento] -> Bool
        contemMedicamentoIgual _ [] = False
        contemMedicamentoIgual medicamento (medicamentoAtual : restanteMedicamentos)
            | medicamento == medicamentoAtual = True
            | otherwise = contemMedicamentoIgual medicamento restanteMedicamentos

        horariosRepetidos :: Receituario -> Bool
        horariosRepetidos [] = False
        horariosRepetidos receituario = or (contemHorarioIgual (listaDeHorarios receituario))

        listaDeHorarios :: Receituario -> [[Horario]]
        listaDeHorarios [] = []
        listaDeHorarios (prescricaoAtual : restanteReceituario) =
            snd prescricaoAtual : listaDeHorarios restanteReceituario

        contemHorarioIgual :: [[Horario]] -> [Bool]
        contemHorarioIgual [] = []
        contemHorarioIgual (horarioAtual : restanteHorarios) =
            horarioRepetido horarioAtual : contemHorarioIgual restanteHorarios

        horarioRepetido :: [Horario] -> Bool
        horarioRepetido [] = False
        horarioRepetido (horarioAtual : restanteHorarios) =
            horarioAtual `elem` restanteHorarios || horarioRepetido restanteHorarios

        ordenarHorarios :: [[Horario]] -> [[Horario]]
        ordenarHorarios [] = []
        ordenarHorarios (horarioAtual : restanteHorarios) =
            ordenar horarioAtual : ordenarHorarios restanteHorarios

        ordenarPorMedicamento :: Receituario -> Receituario
        ordenarPorMedicamento [] = []
        ordenarPorMedicamento (pivo : restante) =
            ordenarPorMedicamento [x | x <- restante, fst x <= fst pivo]
            ++ [pivo]
            ++ ordenarPorMedicamento [x | x <- restante, fst x > fst pivo]

planoValido :: PlanoMedicamento -> Bool
planoValido plano
    | not (or (horariosRepetidos plano)) &&
      (plano == ordenarPorHorario plano) &&
      not (remediosRepetidos plano) &&
      (listaDeMedicamentos plano == ordenarMedicamentos (listaDeMedicamentos plano)) = True
    | otherwise = False
    where
        horariosRepetidos :: PlanoMedicamento -> [Bool]
        horariosRepetidos [] = []
        horariosRepetidos (planoAtual : restantePlano) =
            contemHorarioIgual (fst planoAtual) (listaDeHorarios restantePlano)
            : horariosRepetidos restantePlano

        listaDeHorarios :: PlanoMedicamento -> [Horario]
        listaDeHorarios [] = []
        listaDeHorarios (planoAtual : restantePlano) =
            fst planoAtual : listaDeHorarios restantePlano

        contemHorarioIgual :: Horario -> [Horario] -> Bool
        contemHorarioIgual _ [] = False
        contemHorarioIgual horarioAtual (horario : restanteHorarios)
            | horarioAtual == horario = True
            | otherwise = contemHorarioIgual horarioAtual restanteHorarios

        remediosRepetidos :: PlanoMedicamento -> Bool
        remediosRepetidos [] = False
        remediosRepetidos plano =
            or (contemMedicamentoIgual (listaDeMedicamentos plano))

        listaDeMedicamentos :: PlanoMedicamento -> [[Medicamento]]
        listaDeMedicamentos [] = []
        listaDeMedicamentos (planoAtual : restantePlano) =
            snd planoAtual : listaDeMedicamentos restantePlano

        contemMedicamentoIgual :: [[Medicamento]] -> [Bool]
        contemMedicamentoIgual [] = []
        contemMedicamentoIgual (medicamentoAtual : restanteMedicamentos) =
            medicamentoRepetido medicamentoAtual : contemMedicamentoIgual restanteMedicamentos

        medicamentoRepetido :: [Medicamento] -> Bool
        medicamentoRepetido [] = False
        medicamentoRepetido (medicamentoAtual : restanteMedicamentos) =
            medicamentoAtual `elem` restanteMedicamentos || medicamentoRepetido restanteMedicamentos

        ordenarMedicamentos :: [[Medicamento]] -> [[Medicamento]]
        ordenarMedicamentos [] = []
        ordenarMedicamentos (medicamentoAtual : restanteMedicamentos) =
            ordenar medicamentoAtual : ordenarMedicamentos restanteMedicamentos

        ordenarPorHorario :: PlanoMedicamento -> PlanoMedicamento
        ordenarPorHorario [] = []
        ordenarPorHorario (pivo : restante) =
            ordenarPorHorario [x | x <- restante, fst x <= fst pivo]
            ++ [pivo]
            ++ ordenarPorHorario [x | x <- restante, fst x > fst pivo]



{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:


plantaoValido :: Plantao -> Bool
plantaoValido = undefined

import HelpT2

import Data.List (sortOn,sort)
 -}

plantaoValido :: Plantao -> Bool
plantaoValido plantao
    | not (ou (horariosDistintos plantao)) &&
      (listaHorarios plantao == ordenar (listaHorarios plantao)) &&
      e (ordemMedicar plantao) &&
      not (ou (verificarRepeticao plantao)) = True
    | otherwise = False
    where
        horariosDistintos :: Plantao -> [Bool]
        horariosDistintos [] = []
        horariosDistintos (plantaoAtual : restantePlantao) =
            temHorarioIgual (fst plantaoAtual) (listaHorarios restantePlantao) : horariosDistintos restantePlantao

        listaHorarios :: Plantao -> [Horario]
        listaHorarios [] = []
        listaHorarios (plantaoAtual : restantePlantao) =
            fst plantaoAtual : listaHorarios restantePlantao

        temHorarioIgual :: Horario -> [Horario] -> Bool
        temHorarioIgual _ [] = False
        temHorarioIgual horarioAtual (horario : restanteHorarios)
            | horarioAtual == horario = True
            | otherwise = temHorarioIgual horarioAtual restanteHorarios

        ordemMedicar :: Plantao -> [Bool]
        ordemMedicar [] = []
        ordemMedicar (plantaoAtual : restantePlantao) =
            verificarListaMedicar (listaMedicamentos (snd plantaoAtual)) : ordemMedicar restantePlantao

        listaMedicamentos :: [Cuidado] -> [Medicamento]
        listaMedicamentos [] = []
        listaMedicamentos ((Medicar medicamento) : restanteCuidados) =
            medicamento : listaMedicamentos restanteCuidados
        listaMedicamentos ((Comprar medicamento _) : restanteCuidados) =
            medicamento : listaMedicamentos restanteCuidados

        verificarListaMedicar :: [Medicamento] -> Bool
        verificarListaMedicar [] = True
        verificarListaMedicar lista = ordenar lista == lista

        verificarRepeticao :: Plantao -> [Bool]
        verificarRepeticao [] = []
        verificarRepeticao (plantaoAtual : restantePlantao) =
            medicamentosRepetidos (listaMedicamentos (snd plantaoAtual)) : verificarRepeticao restantePlantao

        medicamentosRepetidos :: [Medicamento] -> Bool
        medicamentosRepetidos [] = False
        medicamentosRepetidos (medicamentoAtual : restanteMedicamentos) =
            medicamentoAtual `elem` restanteMedicamentos || medicamentosRepetidos restanteMedicamentos

        ordenar :: Ord a => [a] -> [a]
        ordenar [] = []
        ordenar (pivo : restante) =
            ordenar [x | x <- restante, x <= pivo]
            ++ [pivo]
            ++ ordenar [x | x <- restante, x > pivo]

        ou :: [Bool] -> Bool
        ou [] = False
        ou (x : xs) = x || ou xs

        e :: [Bool] -> Bool
        e [] = True
        e (x : xs) = x && e xs


{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario receituario = ordenarPlano (geraPlanoVazio receituario)

geraPlanoVazio :: Receituario -> PlanoMedicamento
geraPlanoVazio receituario = agrupaPorHorario (criaPlanosVazios receituario)

criaPlanosVazios :: Receituario -> PlanoMedicamento
criaPlanosVazios [] = []
criaPlanosVazios ((medicamento, horarios) : restanteReceituario) =
  concatenarPlanos (criaPlanosVaziosMedicamento medicamento horarios) (criaPlanosVazios restanteReceituario)

criaPlanosVaziosMedicamento :: Medicamento -> [Horario] -> PlanoMedicamento
criaPlanosVaziosMedicamento _ [] = []
criaPlanosVaziosMedicamento medicamento (horario : restanteHorarios) =
  (horario, [medicamento]) : criaPlanosVaziosMedicamento medicamento restanteHorarios

concatenarPlanos :: PlanoMedicamento -> PlanoMedicamento -> PlanoMedicamento
concatenarPlanos = (++)

agrupaPorHorario :: PlanoMedicamento -> PlanoMedicamento
agrupaPorHorario [] = []
agrupaPorHorario ((horario, medicamentos) : restantePlano) =
  let medicamentosMesmoHorario = [med | (h, med) <- restantePlano, h == horario]
      medicamentosAgrupados = medicamentos ++ concat medicamentosMesmoHorario
      restanteFiltrado = [(h, med) | (h, med) <- restantePlano, h /= horario]
  in (horario, medicamentosAgrupados) : agrupaPorHorario restanteFiltrado

ordenarPlano :: PlanoMedicamento -> PlanoMedicamento
ordenarPlano [] = []
ordenarPlano (pivo : restante) =
  ordenarPlano [x | x <- restante, fst x <= fst pivo]
  ++ [pivo]
  ++ ordenarPlano [x | x <- restante, fst x > fst pivo]


{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano plano =
    ordenarReceituario (concatMap agrupaPrescricoes (removerDuplicados (agrupaPorMedicamento plano)))

agrupaPorMedicamento :: PlanoMedicamento -> [(Medicamento, [Horario])]
agrupaPorMedicamento [] = []
agrupaPorMedicamento ((horario, medicamentos) : restantePlano) =
    [(medicamento, horario : horariosDuplicados medicamento restantePlano) | medicamento <- medicamentos]
    ++ agrupaPorMedicamento restantePlano
  where
    horariosDuplicados :: Medicamento -> PlanoMedicamento -> [Horario]
    horariosDuplicados _ [] = []
    horariosDuplicados medicamento ((h, meds) : resto)
        | medicamento `elem` meds = h : horariosDuplicados medicamento resto
        | otherwise = horariosDuplicados medicamento resto

agrupaPrescricoes :: (Medicamento, [Horario]) -> [Prescricao]
agrupaPrescricoes (medicamento, horarios) = [(medicamento, horarios)]

removerDuplicados :: [(Medicamento, [Horario])] -> [(Medicamento, [Horario])]
removerDuplicados [] = []
removerDuplicados (x : xs) =
    x : removerDuplicados [y | y <- xs, fst y /= fst x]

ordenarReceituario :: Receituario -> Receituario
ordenarReceituario [] = []
ordenarReceituario (pivo : restante) =
    ordenarReceituario [x | x <- restante, fst x <= fst pivo]
    ++ [pivo]
    ++ ordenarReceituario [x | x <- restante, fst x > fst pivo]


{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] estoque = Just estoque
executaPlantao ((horario, cuidados) : restantePlantao) estoqueAtual =
    case executarCuidados cuidados estoqueAtual of
        Nothing -> Nothing
        Just novoEstoque -> executaPlantao restantePlantao novoEstoque

executarCuidados :: [Cuidado] -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executarCuidados [] estoque = Just estoque
executarCuidados (cuidado : restanteCuidados) estoqueAtual =
    case executarCuidado cuidado estoqueAtual of
        Nothing -> Nothing
        Just estoqueAposCuidado -> executarCuidados restanteCuidados estoqueAposCuidado

executarCuidado :: Cuidado -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executarCuidado (Comprar medicamento quantidade) estoque =
    Just (adicionarAoEstoque medicamento quantidade estoque)
executarCuidado (Medicar medicamento) estoque =
    consumirDoEstoque medicamento estoque

adicionarAoEstoque :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
adicionarAoEstoque medicamento quantidade [] = [(medicamento, quantidade)]
adicionarAoEstoque medicamento quantidade ((medicamentoAtual, quantidadeAtual) : restanteEstoque)
    | medicamento == medicamentoAtual =
        (medicamento, quantidadeAtual + quantidade) : restanteEstoque
    | otherwise =
        (medicamentoAtual, quantidadeAtual) : adicionarAoEstoque medicamento quantidade restanteEstoque

consumirDoEstoque :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
consumirDoEstoque _ [] = Nothing
consumirDoEstoque medicamento ((medicamentoAtual, quantidadeAtual) : restanteEstoque)
    | medicamento == medicamentoAtual && quantidadeAtual > 0 =
        Just ((medicamentoAtual, quantidadeAtual - 1) : restanteEstoque)
    | medicamento == medicamentoAtual && quantidadeAtual == 0 =
        Nothing
    | otherwise =
        case consumirDoEstoque medicamento restanteEstoque of
            Nothing -> Nothing
            Just estoqueAtualizado ->
                Just ((medicamentoAtual, quantidadeAtual) : estoqueAtualizado)

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}
satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoqueInicial =
    case executaPlantao plantao estoqueInicial of
        Nothing -> False
        Just estoqueFinal -> verificarCorrespondencia plantao plano

verificarCorrespondencia :: Plantao -> PlanoMedicamento -> Bool
verificarCorrespondencia plantao plano =
    let medicamentosPlantao = medicamentosDoPlantao plantao
        medicamentosPlano = medicamentosDoPlano plano
    in all (`elem` medicamentosPlantao) medicamentosPlano

medicamentosDoPlantao :: Plantao -> [(Horario, Medicamento)]
medicamentosDoPlantao [] = []
medicamentosDoPlantao ((horario, cuidados) : restantePlantao) =
    [(horario, medicamento) | Medicar medicamento <- cuidados] ++ medicamentosDoPlantao restantePlantao

medicamentosDoPlano :: PlanoMedicamento -> [(Horario, Medicamento)]
medicamentosDoPlano [] = []
medicamentosDoPlano ((horario, medicamentos) : restantePlano) =
    [(horario, medicamento) | medicamento <- medicamentos] ++ medicamentosDoPlano restantePlano


{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.




plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto plano estoqueInicial = construirPlantao plano estoqueInicial []

construirPlantao :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao -> Plantao
construirPlantao [] _ plantao = plantao
construirPlantao ((horario, medicamentos) : restantePlano) estoqueAtual plantaoAtual =
    let (cuidados, novoEstoque) = gerarCuidados horario medicamentos estoqueAtual
    in construirPlantao restantePlano novoEstoque (plantaoAtual ++ [(horario, cuidados)])

gerarCuidados :: Horario -> [Medicamento] -> EstoqueMedicamentos -> ([Cuidado], EstoqueMedicamentos)
gerarCuidados horario [] estoque = ([], estoque)
gerarCuidados horario (medicamento : restanteMedicamentos) estoque =
    case verificarEstoque medicamento estoque of
        True ->
            let (cuidadosRestantes, estoqueAtualizado) = gerarCuidados horario restanteMedicamentos (consumirMedicamento medicamento estoque)
            in (Medicar medicamento : cuidadosRestantes, estoqueAtualizado)
        False ->
            let estoqueComComprado = adicionarMedicamento medicamento 1 estoque
                (cuidadosRestantes, estoqueAtualizado) = gerarCuidados horario restanteMedicamentos (consumirMedicamento medicamento estoqueComComprado)
            in (Comprar medicamento 1 : Medicar medicamento : cuidadosRestantes, estoqueAtualizado)

verificarEstoque :: Medicamento -> EstoqueMedicamentos -> Bool
verificarEstoque _ [] = False
verificarEstoque medicamento ((medicamentoAtual, quantidade) : restanteEstoque)
    | medicamento == medicamentoAtual && quantidade > 0 = True
    | otherwise = verificarEstoque medicamento restanteEstoque

adicionarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
adicionarMedicamento medicamento quantidade [] = [(medicamento, quantidade)]
adicionarMedicamento medicamento quantidade :r
((medicamentoAtual, quantidadeAtual) : restanteEstoque)
    | medicamento == medicamentoAtual =
        (medicamentoAtual, quantidadeAtual + quantidade) : restanteEstoque
    | otherwise =
        (medicamentoAtual, quantidadeAtual) : adicionarMedicamento medicamento quantidade restanteEstoque

consumirMedicamento :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
consumirMedicamento _ [] = []
consumirMedicamento medicamento ((medicamentoAtual, quantidadeAtual) : restanteEstoque)
    | medicamento == medicamentoAtual && quantidadeAtual > 0 =
        (medicamentoAtual, quantidadeAtual - 1) : restanteEstoque
    | otherwise =
        (medicamentoAtual, quantidadeAtual) : consumirMedicamento medicamento restanteEstoque


-}
executarPlantao [] estoque = Just estoque
executarPlantao ((_, cuidados) : restantePlantao) estoque =
    foldl (\acc cuidado -> acc >>= (`realizarCuidado` cuidado)) (Just estoque) cuidados
        >>= executarPlantao restantePlantao

realizarCuidado estoque (Comprar medicamento quantidade) =
    Just $ map (\(m, q) -> if m == medicamento then (m, q + quantidade) else (m, q)) estoque ++
           [(medicamento, quantidade) | notElem medicamento (map fst estoque)]
realizarCuidado [] (Medicar _) = Nothing
realizarCuidado estoque (Medicar medicamento) =
    case lookup medicamento estoque of
        Just q | q > 0 -> Just $ map (\(m, q) -> if m == medicamento then (m, q - 1) else (m, q)) estoque
        _ -> Nothing

verificaSatisfacao plantao plano estoque =
    maybe False (\estoqueFinal -> all (`elem` medicamentosPlantao plantao) (medicamentosPlano plano)) $
    executarPlantao plantao estoque

medicamentosPlantao :: Plantao -> [Medicamento]
medicamentosPlantao = concatMap (\(_, cuidados) -> [m | Medicar m <- cuidados])

medicamentosPlano :: PlanoMedicamento -> [Medicamento]
medicamentosPlano = concatMap snd

plantaoCorreto plano estoque =
    fst $ foldl (\(plantao, estoqueAtual) (horario, medicamentos) ->
        let (cuidados, novoEstoque) = determinarCuidados medicamentos estoqueAtual
        in (plantao ++ [(horario, cuidados)], novoEstoque)
    ) ([], estoque) plano

determinarCuidados medicamentos estoque =
    foldl (\(cuidados, estoqueAtual) medicamento ->
        let (acao, novoEstoque) = if maybe 0 id (lookup medicamento estoque) > 0
                                   then ([Medicar medicamento], atualizarEstoqueMedicamento medicamento (-1) estoqueAtual)
                                   else ([Comprar medicamento 1, Medicar medicamento], atualizarEstoqueMedicamento medicamento 0 (adicionarMedicamentoEstoque medicamento 1 estoqueAtual))
        in (cuidados ++ acao, novoEstoque)
    ) ([], estoque) medicamentos

adicionarMedicamentoEstoque medicamento quantidade estoque =
    estoque ++ [(medicamento, quantidade) | notElem medicamento (map fst estoque)]

atualizarEstoqueMedicamento medicamento delta estoque =
    map (\(m, q) -> if m == medicamento then (m, q + delta) else (m, q)) estoque
