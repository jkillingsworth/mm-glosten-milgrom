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

let private computeProbValueHi sells takes =

    let sells = float sells
    let takes = float takes

    let pHi = probInitValueHi * (probSellValueHi ** sells) * (probTakeValueHi ** takes)
    let pLo = probInitValueLo * (probSellValueLo ** sells) * (probTakeValueLo ** takes)

    pHi / (pHi + pLo)

let private computeProbValueLo sells takes =

    let sells = float sells
    let takes = float takes

    let pHi = probInitValueHi * (probSellValueHi ** sells) * (probTakeValueHi ** takes)
    let pLo = probInitValueLo * (probSellValueLo ** sells) * (probTakeValueLo ** takes)

    pLo / (pHi + pLo)

let private computeBid sells takes =
    
    let probValueHiSell = computeProbValueHi (sells + 1) takes
    let probValueLoSell = computeProbValueLo (sells + 1) takes

    (valueHi * probValueHiSell) + (valueLo * probValueLoSell)

let private computeAsk sells takes =

    let probValueHiTake = computeProbValueHi sells (takes + 1)
    let probValueLoTake = computeProbValueLo sells (takes + 1)

    (valueHi * probValueHiTake) + (valueLo * probValueLoTake)

let private executeSell (sells, takes) =

    let sells' = sells + 1
    let takes' = takes

    let bid = computeBid sells' takes'
    let ask = computeAsk sells' takes'

    (bid, ask, sells', takes')

let private executeTake (sells, takes) =

    let sells' = sells
    let takes' = takes + 1

    let bid = computeBid sells' takes'
    let ask = computeAsk sells' takes'

    (bid, ask, sells', takes')

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

    let sells = 0
    let takes = 0

    let bid = computeBid sells takes
    let ask = computeAsk sells takes

    let value, execute = matchExecutionPolicy random executionPolicy

    let generator (sells, takes) =
        let (bid, ask, sells, takes) = execute (sells, takes)
        Some ((value, bid, ask), (sells, takes))
    
    seq {
        yield (value, bid, ask)
        yield! Seq.unfold generator (sells, takes)
    }
