module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let valueHi = 51.0
let valueLo = 50.0

let probInitValueHi = 0.5
let probInitValueLo = 0.5

let probInf = 0.5
let probUnf = 1.0 - probInf

let probInfTakeValueHi = 1.0
let probInfTakeValueLo = 0.0
let probInfSellValueHi = 0.0
let probInfSellValueLo = 1.0

let probUnfTakeValueHi = 0.5
let probUnfTakeValueLo = 0.5
let probUnfSellValueHi = 0.5
let probUnfSellValueLo = 0.5

let probTakeValueHi = (probInf * probInfTakeValueHi) + (probUnf * probUnfTakeValueHi)
let probTakeValueLo = (probInf * probInfTakeValueLo) + (probUnf * probUnfTakeValueLo)
let probSellValueHi = (probInf * probInfSellValueHi) + (probUnf * probUnfSellValueHi)
let probSellValueLo = (probInf * probInfSellValueLo) + (probUnf * probUnfSellValueLo)

//-------------------------------------------------------------------------------------------------

let private computeProbValueHi takes sells =

    let takes = float takes
    let sells = float sells

    let pHi = probInitValueHi * (probTakeValueHi ** takes) * (probSellValueHi ** sells)
    let pLo = probInitValueLo * (probTakeValueLo ** takes) * (probSellValueLo ** sells)

    pHi / (pHi + pLo)

let private computeProbValueLo takes sells =

    let takes = float takes
    let sells = float sells

    let pHi = probInitValueHi * (probTakeValueHi ** takes) * (probSellValueHi ** sells)
    let pLo = probInitValueLo * (probTakeValueLo ** takes) * (probSellValueLo ** sells)

    pLo / (pHi + pLo)

let private computeBid takes sells =
    
    let probValueHiSell = computeProbValueHi takes (sells + 1)
    let probValueLoSell = computeProbValueLo takes (sells + 1)

    (valueHi * probValueHiSell) + (valueLo * probValueLoSell)

let private computeAsk takes sells =

    let probValueHiTake = computeProbValueHi (takes + 1) sells
    let probValueLoTake = computeProbValueLo (takes + 1) sells

    (valueHi * probValueHiTake) + (valueLo * probValueLoTake)

let private executeTake (takes, sells) =

    let takes' = takes + 1
    let sells' = sells

    let bid = computeBid takes' sells'
    let ask = computeAsk takes' sells'

    (bid, ask, takes', sells')

let private executeSell (takes, sells) =

    let takes' = takes
    let sells' = sells + 1

    let bid = computeBid takes' sells'
    let ask = computeAsk takes' sells'

    (bid, ask, takes', sells')

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

    let takes = 0
    let sells = 0

    let bid = computeBid takes sells
    let ask = computeAsk takes sells

    let value, execute = matchExecutionPolicy random executionPolicy

    let generator (takes, sells) =
        let (bid, ask, takes, sells) = execute (takes, sells)
        Some ((value, bid, ask), (takes, sells))
    
    seq {
        yield (value, bid, ask)
        yield! Seq.unfold generator (takes, sells)
    }
