module Compute

open MathNet.Numerics.Distributions
open MathNet.Numerics.Integration

//-------------------------------------------------------------------------------------------------

let private integrate f lower upper =
    let f = System.Func<_,_>(f)
    let partitions = 10000
    NewtonCotesTrapeziumRule.IntegrateComposite(f, lower, upper, partitions)

//-------------------------------------------------------------------------------------------------

let valueUpper = 100.0
let valueLower = 0.0

let probInit value = Normal.PDF(50.50, 0.5, value)

let probInf = 0.5
let probUnf = 1.0 - probInf

let probInfSell value estimate = 1.0 - Normal.CDF(estimate, 0.1, value)
let probInfTake value estimate = 0.0 + Normal.CDF(estimate, 0.1, value)

let probUnfSell value = 0.5
let probUnfTake value = 0.5

//-------------------------------------------------------------------------------------------------

let private computePosteriorSell p =

    let estimate =
        let f value = (p value) * value
        integrate f valueLower valueUpper

    let probSell value
        = (probInf * (probInfSell value estimate))
        + (probUnf * (probUnfSell value))

    let f value = (p value) * (probSell value)
    let probSellOverall = integrate f valueLower valueUpper
    let p' value = (f value) / probSellOverall

    p'

let private computePosteriorTake p =

    let estimate =
        let f value = (p value) * value
        integrate f valueLower valueUpper

    let probTake value
        = (probInf * (probInfTake value estimate))
        + (probUnf * (probUnfTake value))

    let f value = (p value) * (probTake value)
    let probTakeOverall = integrate f valueLower valueUpper
    let p' value = (f value) / probTakeOverall

    p'

//-------------------------------------------------------------------------------------------------

let private computeBid p =

    let p' = computePosteriorSell p
    let f value = (p' value) * value
    integrate f valueLower valueUpper

let private computeAsk p =

    let p' = computePosteriorTake p
    let f value = (p' value) * value
    integrate f valueLower valueUpper

//-------------------------------------------------------------------------------------------------

let private executeTake p =

    let p' = computePosteriorTake p

    let bid = computeBid p'
    let ask = computeAsk p'

    (bid, ask, p')

let private executeSell p =

    let p' = computePosteriorSell p

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
        Some ((value, bid, ask, p), (bid, ask, p))

    seq {
        yield (value, bid, ask, p)
        yield! Seq.unfold generator (bid, ask, p)
    }
