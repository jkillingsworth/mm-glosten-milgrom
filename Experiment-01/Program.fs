module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()
let policy = Compute.StochasticHi
let result = Compute.generateResults random policy |> Seq.take (25 + 1) |> Seq.toArray

Chart.renderChart @"..\..\..\Experiment-01.png" result

for (value, bid, ask) in result do
    printfn "(%f, %f) %f" bid ask (ask - bid)
