module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()
let policy = Compute.Stochastic 50.75
let result = Compute.generateResults random policy |> Seq.take (25 + 1) |> Seq.toArray

Chart.renderPrices @"..\..\..\Experiment-07-Prices.png" result
Chart.renderBelief @"..\..\..\Experiment-07-Belief.png" result.[25]

for (value, bid, ask, p) in result do
    let _, _, vBid = bid
    let _, _, vAsk = ask
    printfn "(%f, %f) %f" vBid vAsk (vAsk - vBid)
