module Compute

open MathNet.Numerics.Distributions

//-------------------------------------------------------------------------------------------------

let probInf = 0.5
let probUnf = 1.0 - probInf
let probEta = 0.4

let mu = 50.50
let sigma = 0.5

let range = 5.0
let scale = 100
let count = int (2.0 * range * float scale)

let vInc = 2.0 * range * sigma / float count
let vMin = mu - (range * sigma)
let vMax = mu + (range * sigma)

let values = Array.init count (fun i -> vMin + (float i * vInc))

let probInitInitializer i =
    let i = vInc * (float i)
    let a = Normal.CDF(0.0, sigma, (-range * sigma) + i)
    let b = Normal.CDF(0.0, sigma, (-range * sigma) + i + vInc)
    b - a

let probInit = Array.init count probInitInitializer

//-------------------------------------------------------------------------------------------------

let private computeProbSell (p : float[]) iBid =

    let mutable acc = 0.0

    for i = 0 to count - 1 do
        acc <- acc + p.[i] * probUnf * probEta

    for i = 0 to iBid - 1 do
        acc <- acc + p.[i] * probInf

    acc

let private computeProbTake (p : float[]) iAsk =

    let mutable acc = 0.0

    for i = 0 to count - 1 do
        acc <- acc + p.[i] * probUnf * probEta

    for i = iAsk + 1 to count - 1 do
        acc <- acc + p.[i] * probInf

    acc

let private computeProbNone (p : float[]) iBid iAsk =

    let mutable acc = 0.0

    for i = 0 to count - 1 do
        acc <- acc + p.[i] * probUnf * (1.0 - 2.0 * probEta)

    for i = iBid to iAsk do
        acc <- acc + p.[i] * probInf

    acc

//-------------------------------------------------------------------------------------------------

let private normalizePosterior (p : float[]) =

    let mutable acc = 0.0

    for i = 0 to count - 1 do
        let weight = p.[i]
        let value = vMin + (float i) * vInc
        let x = weight * value
        acc <- acc + x

    let mu' = acc

    let mutable acc = 0.0

    for i = 0 to count - 1 do
        let weight = p.[i]
        let value = vMin + (float i) * vInc
        let x = weight * ((mu' - value) ** 2.0)
        acc <- acc + x

    let sigma' = sqrt acc

    let probInitializer i =
        let i = vInc * (float i)
        let a = Normal.CDF(mu' - mu, sigma', (-range * sigma) + i)
        let b = Normal.CDF(mu' - mu, sigma', (-range * sigma) + i + vInc)
        b - a

    Array.init count probInitializer

let private computePosteriorSell p iBid =

    let pSell = computeProbSell p iBid

    let mapper i pv =
        let x = (probUnf * probEta) + if i < iBid then probInf else 0.0
        (pv * x) / pSell

    p
    |> Array.mapi mapper
    |> normalizePosterior

let private computePosteriorTake p iAsk =

    let pTake = computeProbTake p iAsk

    let mapper i pv =
        let x = (probUnf * probEta) + if i > iAsk then probInf else 0.0
        (pv * x) / pTake

    p
    |> Array.mapi mapper
    |> normalizePosterior

let private computePosteriorNone p iBid iAsk =

    let pNone = computeProbNone p iBid iAsk

    let mapper i pv =
        let x = (probUnf * (1.0 - 2.0 * probEta)) + if i >= iBid && i <= iAsk then probInf else 0.0
        (pv * x) / pNone

    p
    |> Array.mapi mapper
    |> normalizePosterior

//-------------------------------------------------------------------------------------------------

let private computePricesBid p iBid =

    let pSell = computeProbSell p iBid

    let mutable acc = 0.0

    for i = 0 to iBid - 1 do
        acc <- acc + values.[i] * p.[i] * (probInf + (probUnf * probEta))

    for i = iBid to count - 1 do
        acc <- acc + values.[i] * p.[i] * (probUnf * probEta)

    acc / pSell

let private computePricesAsk p iAsk =

    let pTake = computeProbTake p iAsk

    let mutable acc = 0.0

    for i = 0 to iAsk do
        acc <- acc + values.[i] * p.[i] * (probUnf * probEta)

    for i = iAsk + 1 to count - 1 do
        acc <- acc + values.[i] * p.[i] * (probInf + (probUnf * probEta))

    acc / pTake

let private computeBid p =

    let initializer i = (i, values.[i], computePricesBid p i)

    initializer
    |> Seq.init count
    |> Seq.minBy (fun (_, _, v) -> v)

let private computeAsk p =

    let initializer i = (i, values.[i], computePricesAsk p i)

    initializer
    |> Seq.init count
    |> Seq.maxBy (fun (_, _, v) -> v)

//-------------------------------------------------------------------------------------------------

let private executeSell p (bid, ask) =

    let iBid, _, _ = bid
    let p' = computePosteriorSell p iBid

    let bid = computeBid p'
    let ask = computeAsk p'

    (bid, ask, p')

let private executeTake p (bid, ask) =

    let iAsk, _, _ = ask
    let p' = computePosteriorTake p iAsk

    let bid = computeBid p'
    let ask = computeAsk p'

    (bid, ask, p')

let private executeNone p (bid, ask) =

    let iBid, _, _ = bid
    let iAsk, _, _ = ask
    let p' = computePosteriorNone p iBid iAsk

    let bid = computeBid p'
    let ask = computeAsk p'

    (bid, ask, p')

//-------------------------------------------------------------------------------------------------

type ExecutionPolicy =
    | Stochastic of float

let private getValue = function
    | Stochastic value -> value

let private getExecuteFunc random (value, bid, ask) =
    let _, _, vBid = bid
    let _, _, vAsk = ask
    match Sample.continuousUniform 0.0 1.0 random with
    | sample when (sample < probInf) && (value > vAsk) -> executeTake
    | sample when (sample < probInf) && (value < vBid) -> executeSell
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
        let (bid, ask, p) = execute p (bid, ask)
        Some ((value, bid, ask, p), (bid, ask, p))

    seq {
        yield (value, bid, ask, p)
        yield! Seq.unfold generator (bid, ask, p)
    }
