module Compute

open MathNet.Numerics.Distributions
open MathNet.Numerics.Integration

//-------------------------------------------------------------------------------------------------

let valueUpper = 51.0
let valueLower = 50.0

let probInit value = ContinuousUniform.PDF(valueLower, valueUpper, value)

let probInf = 0.5
let probUnf = 1.0 - probInf

let probInfTake value = ContinuousUniform.CDF(valueLower, valueUpper, value)
let probInfSell value = 1.0 - probInfTake value

let probUnfTake value = 0.5
let probUnfSell value = 0.5

let probTake value = (probInf * probInfTake value) + (probUnf * probUnfTake value)
let probSell value = (probInf * probInfSell value) + (probUnf * probUnfSell value)

//-------------------------------------------------------------------------------------------------

let private integrate f lower upper =
    let f = System.Func<_,_>(f)
    let partitions = 1000
    NewtonCotesTrapeziumRule.IntegrateComposite(f, valueLower, valueUpper, partitions)

let private computeBid p =

    let f value = (p value) * (probSell value)
    let probSellOverall = integrate f valueLower valueUpper
    let probValueGivenSell value = (f value) / probSellOverall

    let f value = value * (probValueGivenSell value)
    integrate f valueLower valueUpper

let private computeAsk p =

    let f value = (p value) * (probTake value)
    let probTakeOverall = integrate f valueLower valueUpper
    let probValueGivenTake value = (f value) / probTakeOverall

    let f value = value * (probValueGivenTake value)
    integrate f valueLower valueUpper

let private executeTake p =

    let p' value = (p value) * (probTake value)

    let bid = computeBid p'
    let ask = computeAsk p'

    (bid, ask, p')

let private executeSell p =

    let p' value = (p value) * (probSell value)

    let bid = computeBid p'
    let ask = computeAsk p'

    (bid, ask, p')

let private executeNone p =

    let p' = p

    let bid = computeBid p'
    let ask = computeAsk p'

    (bid, ask, p')

//-------------------------------------------------------------------------------------------------

type ExecutionPolicy =
    | Stochastic of float

let private getValue = function
    | Stochastic value -> value

let private getExecuteFunc random (value, bid, ask) =
    match Sample.continuousUniform 0.0 1.0 random with
    | sample when (sample < probInf) && (value > ask) -> executeTake
    | sample when (sample < probInf) && (value < bid) -> executeSell
    | sample when (sample < probInf) -> executeNone
    | sample when (sample < probInf + probUnf * 0.50)
        -> executeTake
    | _ -> executeSell

let generateResults random executionPolicy =

    let p = probInit

    let bid = computeBid p
    let ask = computeAsk p

    let value = getValue executionPolicy

    let generator (bid, ask, p) =
        let execute = getExecuteFunc random (value, bid, ask)
        let (bid, ask, p) = execute p
        Some ((value, bid, ask), (bid, ask, p))

    seq {
        yield (value, bid, ask)
        yield! Seq.unfold generator (bid, ask, p)
    }
