module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let valueHi = 51.0
let valueLo = 50.0

let probInitValueHi = 0.5
let probInitValueLo = 0.5

let probInf = 0.5
let probUnf = 1.0 - probInf

let probInfSellValueHi = 0.0
let probInfSellValueLo = 1.0
let probInfTakeValueHi = 1.0
let probInfTakeValueLo = 0.0

let probUnfSellValueHi = 0.5
let probUnfSellValueLo = 0.5
let probUnfTakeValueHi = 0.5
let probUnfTakeValueLo = 0.5

let probSellValueHi = (probInf * probInfSellValueHi) + (probUnf * probUnfSellValueHi)
let probSellValueLo = (probInf * probInfSellValueLo) + (probUnf * probUnfSellValueLo)
let probTakeValueHi = (probInf * probInfTakeValueHi) + (probUnf * probUnfTakeValueHi)
let probTakeValueLo = (probInf * probInfTakeValueLo) + (probUnf * probUnfTakeValueLo)

//-------------------------------------------------------------------------------------------------

let private computeBid pHi pLo =

    let probValueHiSell = (pHi * probSellValueHi) / ((pHi * probSellValueHi) + (pLo * probSellValueLo))
    let probValueLoSell = (pLo * probSellValueLo) / ((pHi * probSellValueHi) + (pLo * probSellValueLo))

    (valueHi * probValueHiSell) + (valueLo * probValueLoSell)

let private computeAsk pHi pLo =

    let probValueHiTake = (pHi * probTakeValueHi) / ((pHi * probTakeValueHi) + (pLo * probTakeValueLo))
    let probValueLoTake = (pLo * probTakeValueLo) / ((pHi * probTakeValueHi) + (pLo * probTakeValueLo))

    (valueHi * probValueHiTake) + (valueLo * probValueLoTake)

let private executeSell (pHi, pLo) =

    let pHi' = pHi * probSellValueHi
    let pLo' = pLo * probSellValueLo

    let bid = computeBid pHi' pLo'
    let ask = computeAsk pHi' pLo'

    (bid, ask, pHi', pLo')

let private executeTake (pHi, pLo) =

    let pHi' = pHi * probTakeValueHi
    let pLo' = pLo * probTakeValueLo

    let bid = computeBid pHi' pLo'
    let ask = computeAsk pHi' pLo'

    (bid, ask, pHi', pLo')

//-------------------------------------------------------------------------------------------------

type ExecutionPolicy =
    | AlwaysTakeHi
    | AlwaysSellLo
    | StochasticHi
    | StochasticLo

let private matchStochasticHi random state =
    match Sample.continuousUniform 0.0 1.0 random with
    | sample when sample < probTakeValueHi
        -> executeTake state
    | _ -> executeSell state

let private matchStochasticLo random state =
    match Sample.continuousUniform 0.0 1.0 random with
    | sample when sample < probSellValueLo
        -> executeSell state
    | _ -> executeTake state

let private matchExecutionPolicy random = function
    | AlwaysTakeHi -> valueHi, executeTake
    | AlwaysSellLo -> valueLo, executeSell
    | StochasticHi -> valueHi, matchStochasticHi random
    | StochasticLo -> valueLo, matchStochasticLo random

let generateResults random executionPolicy =

    let pHi = probInitValueHi
    let pLo = probInitValueLo

    let bid = computeBid pHi pLo
    let ask = computeAsk pHi pLo

    let value, execute = matchExecutionPolicy random executionPolicy

    let generator (pHi, pLo) =
        let (bid, ask, pHi, pLo) = execute (pHi, pLo)
        Some ((value, bid, ask), (pHi, pLo))

    seq {
        yield (value, bid, ask)
        yield! Seq.unfold generator (pHi, pLo)
    }
