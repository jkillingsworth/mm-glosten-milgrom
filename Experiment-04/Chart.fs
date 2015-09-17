module Chart

open OxyPlot
open OxyPlot.Axes
open OxyPlot.Series

//-------------------------------------------------------------------------------------------------

let private exportToPng path w h model =

    use writeStream = System.IO.File.OpenWrite(path)
    let pngExporter = OxyPlot.WindowsForms.PngExporter()
    pngExporter.Width <- w
    pngExporter.Height <- h
    pngExporter.Export(model, writeStream)

let private defaultColorsToUseForPlots =

    [| OxyColors.Red
       OxyColors.Blue
       OxyColors.Green
       OxyColors.Orange
       OxyColors.Purple
       OxyColors.Teal |]

//-------------------------------------------------------------------------------------------------

let renderChart path data =

    let model = PlotModel()

    model.DefaultColors <- defaultColorsToUseForPlots
    model.LegendBackground <- OxyColors.White
    model.LegendBorder <- OxyColors.Gray
    model.LegendBorderThickness <- 1.0
    model.LegendPlacement <- LegendPlacement.Inside
    model.LegendPosition <- LegendPosition.RightTop
    model.PlotMargins <- OxyThickness(nan, nan, 10.0, nan)

    let axis = LinearAxis()
    axis.Title <- "Time"
    axis.Position <- AxisPosition.Bottom
    axis.Minimum <- 0.0
    axis.Maximum <- float (Array.length data - 1)
    axis.MajorStep <- 1.0
    axis.MinorStep <- 1.0
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    model.Axes.Add(axis)

    let axis = LinearAxis()
    axis.Title <- "Price"
    axis.Position <- AxisPosition.Left
    axis.Minimum <- Compute.valueLower - 0.05
    axis.Maximum <- Compute.valueUpper + 0.05
    axis.MajorStep <- 0.25
    axis.MinorStep <- 0.05
    axis.MajorGridlineColor <- OxyColors.LightGray
    axis.MajorGridlineStyle <- LineStyle.Dot
    axis.MinorGridlineColor <- OxyColors.LightGray
    axis.MinorGridlineStyle <- LineStyle.Dot
    axis.StringFormat <- "F2"
    axis.AxisTitleDistance <- 9.0
    model.Axes.Add(axis)

    let series = LineSeries()
    series.Title <- "True price"
    series.StrokeThickness <- 1.0
    data
    |> Array.mapi (fun i (value, bid, ask) -> DataPoint(float i, value))
    |> Array.iter series.Points.Add
    model.Series.Add(series)

    let series = AreaSeries()
    series.Title <- "MM price"
    series.StrokeThickness <- 1.0
    data
    |> Array.mapi (fun i (value, bid, ask) -> DataPoint(float i, bid))
    |> Array.iter series.Points.Add
    data
    |> Array.mapi (fun i (value, bid, ask) -> DataPoint(float i, ask))
    |> Array.iter series.Points2.Add
    model.Series.Add(series)

    model |> exportToPng path 700 400
