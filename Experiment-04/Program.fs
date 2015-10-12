module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()
let policy = Compute.Stochastic 50.75
let result = Compute.generateResults random policy |> Seq.take (25 + 1) |> Seq.toArray

Chart.renderPrices @"..\..\..\Experiment-04-Prices.png" result
Chart.renderBelief @"..\..\..\Experiment-04-Belief.png" result.[25]

for (value, bid, ask, p) in result do
    printfn "(%f, %f) %f" bid ask (ask - bid)
