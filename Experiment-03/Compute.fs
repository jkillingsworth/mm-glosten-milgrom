module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let valueHi = 51.00
let value75 = 50.75
let value50 = 50.50
let value25 = 50.25
let valueLo = 50.00

let probInitValueHi = 0.2
let probInitValue75 = 0.2
let probInitValue50 = 0.2
let probInitValue25 = 0.2
let probInitValueLo = 0.2

let probInf = 0.5
let probUnf = 1.0 - probInf

let probUnfSellValueHi = 0.5
let probUnfSellValue75 = 0.5
let probUnfSellValue50 = 0.5
let probUnfSellValue25 = 0.5
let probUnfSellValueLo = 0.5

let probUnfTakeValueHi = 0.5
let probUnfTakeValue75 = 0.5
let probUnfTakeValue50 = 0.5
let probUnfTakeValue25 = 0.5
let probUnfTakeValueLo = 0.5

//-------------------------------------------------------------------------------------------------

let private computePosteriorSell (pHi, p75, p50, p25, pLo) =

    let estimate
        = (pHi * valueHi)
        + (p75 * value75)
        + (p50 * value50)
        + (p25 * value25)
        + (pLo * valueLo)

    let probInfSellValueHi = if valueHi < estimate then 1.0 else 0.0
    let probInfSellValue75 = if value75 < estimate then 1.0 else 0.0
    let probInfSellValue50 = if value50 < estimate then 1.0 else 0.0
    let probInfSellValue25 = if value25 < estimate then 1.0 else 0.0
    let probInfSellValueLo = if valueLo < estimate then 1.0 else 0.0

    let probSellValueHi = (probInf * probInfSellValueHi) + (probUnf * probUnfSellValueHi)
    let probSellValue75 = (probInf * probInfSellValue75) + (probUnf * probUnfSellValue75)
    let probSellValue50 = (probInf * probInfSellValue50) + (probUnf * probUnfSellValue50)
    let probSellValue25 = (probInf * probInfSellValue25) + (probUnf * probUnfSellValue25)
    let probSellValueLo = (probInf * probInfSellValueLo) + (probUnf * probUnfSellValueLo)

    let probSell
        = (pHi * probSellValueHi)
        + (p75 * probSellValue75)
        + (p50 * probSellValue50)
        + (p25 * probSellValue25)
        + (pLo * probSellValueLo)

    let pHi' = (pHi * probSellValueHi) / probSell
    let p75' = (p75 * probSellValue75) / probSell
    let p50' = (p50 * probSellValue50) / probSell
    let p25' = (p25 * probSellValue25) / probSell
    let pLo' = (pLo * probSellValueLo) / probSell

    (pHi', p75', p50', p25', pLo')

let private computePosteriorTake (pHi, p75, p50, p25, pLo) =

    let estimate
        = (pHi * valueHi)
        + (p75 * value75)
        + (p50 * value50)
        + (p25 * value25)
        + (pLo * valueLo)

    let probInfTakeValueHi = if valueHi > estimate then 1.0 else 0.0
    let probInfTakeValue75 = if value75 > estimate then 1.0 else 0.0
    let probInfTakeValue50 = if value50 > estimate then 1.0 else 0.0
    let probInfTakeValue25 = if value25 > estimate then 1.0 else 0.0
    let probInfTakeValueLo = if valueLo > estimate then 1.0 else 0.0

    let probTakeValueHi = (probInf * probInfTakeValueHi) + (probUnf * probUnfTakeValueHi)
    let probTakeValue75 = (probInf * probInfTakeValue75) + (probUnf * probUnfTakeValue75)
    let probTakeValue50 = (probInf * probInfTakeValue50) + (probUnf * probUnfTakeValue50)
    let probTakeValue25 = (probInf * probInfTakeValue25) + (probUnf * probUnfTakeValue25)
    let probTakeValueLo = (probInf * probInfTakeValueLo) + (probUnf * probUnfTakeValueLo)

    let probTake
        = (pHi * probTakeValueHi)
        + (p75 * probTakeValue75)
        + (p50 * probTakeValue50)
        + (p25 * probTakeValue25)
        + (pLo * probTakeValueLo)

    let pHi' = (pHi * probTakeValueHi) / probTake
    let p75' = (p75 * probTakeValue75) / probTake
    let p50' = (p50 * probTakeValue50) / probTake
    let p25' = (p25 * probTakeValue25) / probTake
    let pLo' = (pLo * probTakeValueLo) / probTake

    (pHi', p75', p50', p25', pLo')

//-------------------------------------------------------------------------------------------------

let private computeBid (pHi, p75, p50, p25, pLo) =
    
    let (pHi', p75', p50', p25', pLo') = computePosteriorSell (pHi, p75, p50, p25, pLo)

    let value
        = (valueHi * pHi')
        + (value75 * p75')
        + (value50 * p50')
        + (value25 * p25')
        + (valueLo * pLo')

    value

let private computeAsk (pHi, p75, p50, p25, pLo) =

    let (pHi', p75', p50', p25', pLo') = computePosteriorTake (pHi, p75, p50, p25, pLo)

    let value
        = (valueHi * pHi')
        + (value75 * p75')
        + (value50 * p50')
        + (value25 * p25')
        + (valueLo * pLo')

    value

//-------------------------------------------------------------------------------------------------

let private executeSell (pHi, p75, p50, p25, pLo) =

    let (pHi', p75', p50', p25', pLo') = computePosteriorSell (pHi, p75, p50, p25, pLo)

    let bid = computeBid (pHi', p75', p50', p25', pLo')
    let ask = computeAsk (pHi', p75', p50', p25', pLo')

    (bid, ask, pHi', p75', p50', p25', pLo')

let private executeTake (pHi, p75, p50, p25, pLo) =

    let (pHi', p75', p50', p25', pLo') = computePosteriorTake (pHi, p75, p50, p25, pLo)

    let bid = computeBid (pHi', p75', p50', p25', pLo')
    let ask = computeAsk (pHi', p75', p50', p25', pLo')

    (bid, ask, pHi', p75', p50', p25', pLo')

let private executeNone (pHi, p75, p50, p25, pLo) =

    let pHi' = pHi
    let p75' = p75
    let p50' = p50
    let p25' = p25
    let pLo' = pLo

    let bid = computeBid (pHi', p75', p50', p25', pLo')
    let ask = computeAsk (pHi', p75', p50', p25', pLo')

    (bid, ask, pHi', p75', p50', p25', pLo')

//-------------------------------------------------------------------------------------------------

type ExecutionPolicy =
    | StochasticHi
    | Stochastic75
    | Stochastic50
    | Stochastic25
    | StochasticLo

let private getValue = function
    | StochasticHi -> valueHi
    | Stochastic75 -> value75
    | Stochastic50 -> value50
    | Stochastic25 -> value25
    | StochasticLo -> valueLo

let private getExecuteFunc random (value, bid, ask) =
    match Sample.continuousUniform 0.0 1.0 random with
    | sample when (sample < probInf) && (value > ask) -> executeTake
    | sample when (sample < probInf) && (value < bid) -> executeSell
    | sample when (sample < probInf) -> executeNone
    | sample when (sample < probInf + probUnf * 0.50)
        -> executeTake
    | _ -> executeSell

let generateResults random executionPolicy =

    let pHi = probInitValueHi
    let p75 = probInitValue75
    let p50 = probInitValue50
    let p25 = probInitValue25
    let pLo = probInitValueLo

    let bid = computeBid (pHi, p75, p50, p25, pLo)
    let ask = computeAsk (pHi, p75, p50, p25, pLo)

    let value = getValue executionPolicy

    let generator (bid, ask, pHi, p75, p50, p25, pLo) =
        let execute = getExecuteFunc random (value, bid, ask)
        let (bid, ask, pHi, p75, p50, p25, pLo) = execute (pHi, p75, p50, p25, pLo)
        Some ((value, bid, ask), (bid, ask, pHi, p75, p50, p25, pLo))

    seq {
        yield (value, bid, ask)
        yield! Seq.unfold generator (bid, ask, pHi, p75, p50, p25, pLo)
    }
