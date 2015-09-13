module Program

open System

//-------------------------------------------------------------------------------------------------

let random = Random()
let policy = Compute.AlwaysTakeHi
let result = Compute.generateResults random policy |> Seq.take (10 + 1) |> Seq.toArray

Chart.renderChart (sprintf @"..\..\..\01-%A.png" policy) result

for (value, bid, ask) in result do
    printfn "(%f, %f) %f" bid ask (ask - bid)
