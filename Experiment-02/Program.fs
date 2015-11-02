module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()
let policy = Compute.StochasticHi
let result = Compute.generateResults random policy |> Seq.take (25 + 1) |> Seq.toArray

Chart.renderPrices @"..\..\..\Experiment-02-Prices.png" result
Chart.renderBelief @"..\..\..\Experiment-02-Belief.png" result.[10]

for (value, bid, ask, p) in result do
    printfn "(%f, %f) %f" bid ask (ask - bid)
