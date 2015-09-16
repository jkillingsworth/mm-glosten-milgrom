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

let probInfTakeValueHi = 1.00
let probInfTakeValue75 = 0.75
let probInfTakeValue50 = 0.50
let probInfTakeValue25 = 0.25
let probInfTakeValueLo = 0.00

let probInfSellValueHi = 0.00
let probInfSellValue75 = 0.25
let probInfSellValue50 = 0.50
let probInfSellValue25 = 0.75
let probInfSellValueLo = 1.00

let probUnfTakeValueHi = 0.5
let probUnfTakeValue75 = 0.5
let probUnfTakeValue50 = 0.5
let probUnfTakeValue25 = 0.5
let probUnfTakeValueLo = 0.5

let probUnfSellValueHi = 0.5
let probUnfSellValue75 = 0.5
let probUnfSellValue50 = 0.5
let probUnfSellValue25 = 0.5
let probUnfSellValueLo = 0.5

let probTakeValueHi = (probInf * probInfTakeValueHi) + (probUnf * probUnfTakeValueHi)
let probTakeValue75 = (probInf * probInfTakeValue75) + (probUnf * probUnfTakeValue75)
let probTakeValue50 = (probInf * probInfTakeValue50) + (probUnf * probUnfTakeValue50)
let probTakeValue25 = (probInf * probInfTakeValue25) + (probUnf * probUnfTakeValue25)
let probTakeValueLo = (probInf * probInfTakeValueLo) + (probUnf * probUnfTakeValueLo)

let probSellValueHi = (probInf * probInfSellValueHi) + (probUnf * probUnfSellValueHi)
let probSellValue75 = (probInf * probInfSellValue75) + (probUnf * probUnfSellValue75)
let probSellValue50 = (probInf * probInfSellValue50) + (probUnf * probUnfSellValue50)
let probSellValue25 = (probInf * probInfSellValue25) + (probUnf * probUnfSellValue25)
let probSellValueLo = (probInf * probInfSellValueLo) + (probUnf * probUnfSellValueLo)

//-------------------------------------------------------------------------------------------------

let private computeBid (pHi, p75, p50, p25, pLo) =

    let probSell
        = (pHi * probSellValueHi)
        + (p75 * probSellValue75)
        + (p50 * probSellValue50)
        + (p25 * probSellValue25)
        + (pLo * probSellValueLo)

    let probValueHiSell = (pHi * probSellValueHi) / probSell
    let probValue75Sell = (p75 * probSellValue75) / probSell
    let probValue50Sell = (p50 * probSellValue50) / probSell
    let probValue25Sell = (p25 * probSellValue25) / probSell
    let probValueLoSell = (pLo * probSellValueLo) / probSell

    let value
        = (valueHi * probValueHiSell)
        + (value75 * probValue75Sell)
        + (value50 * probValue50Sell)
        + (value25 * probValue25Sell)
        + (valueLo * probValueLoSell)

    value

let private computeAsk (pHi, p75, p50, p25, pLo) =

    let probTake
        = (pHi * probTakeValueHi)
        + (p75 * probTakeValue75)
        + (p50 * probTakeValue50)
        + (p25 * probTakeValue25)
        + (pLo * probTakeValueLo)

    let probValueHiTake = (pHi * probTakeValueHi) / probTake
    let probValue75Take = (p75 * probTakeValue75) / probTake
    let probValue50Take = (p50 * probTakeValue50) / probTake
    let probValue25Take = (p25 * probTakeValue25) / probTake
    let probValueLoTake = (pLo * probTakeValueLo) / probTake

    let value
        = (valueHi * probValueHiTake)
        + (value75 * probValue75Take)
        + (value50 * probValue50Take)
        + (value25 * probValue25Take)
        + (valueLo * probValueLoTake)

    value

let private executeTake (pHi, p75, p50, p25, pLo) =

    let pHi' = pHi * probTakeValueHi
    let p75' = p75 * probTakeValue75
    let p50' = p50 * probTakeValue50
    let p25' = p25 * probTakeValue25
    let pLo' = pLo * probTakeValueLo

    let bid = computeBid (pHi', p75', p50', p25', pLo')
    let ask = computeAsk (pHi', p75', p50', p25', pLo')

    (bid, ask, pHi', p75', p50', p25', pLo')

let private executeSell (pHi, p75, p50, p25, pLo) =

    let pHi' = pHi * probSellValueHi
    let p75' = p75 * probSellValue75
    let p50' = p50 * probSellValue50
    let p25' = p25 * probSellValue25
    let pLo' = pLo * probSellValueLo

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
