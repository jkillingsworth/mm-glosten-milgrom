module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()
let policy = Compute.Stochastic75
let result = Compute.generateResults random policy |> Seq.take (25 + 1) |> Seq.toArray

Chart.renderPrices @"..\..\..\Experiment-03-Prices.png" result
Chart.renderBelief @"..\..\..\Experiment-03-Belief.png" result.[25]

for (value, bid, ask, p) in result do
    printfn "(%f, %f) %f" bid ask (ask - bid)
