// Learn more about F# at http://fsharp.org

open System
open System.Drawing
open FSharp.Charting

type CSV = FSharp.Data.CsvProvider< @"add.csv", Separators = ";">

[<EntryPoint>]
let main argv =
    let folder = @"C:\Users\Schorsch\Desktop\bb"

    let mutable nextColor = 0
    let allColors = 
        [
            Color.FromArgb 0xffe6194b
            Color.FromArgb 0xff3cb44b
            Color.FromArgb 0xffffe119
            Color.FromArgb 0xff4363d8
            Color.FromArgb 0xfff58231
            Color.FromArgb 0xff911eb4
            Color.FromArgb 0xff46f0f0
            Color.FromArgb 0xfff032e6
            Color.FromArgb 0xffbcf60c
            Color.FromArgb 0xfffabebe
            Color.FromArgb 0xff008080
            Color.FromArgb 0xffe6beff
            Color.FromArgb 0xff9a6324
            Color.FromArgb 0xfffffac8
            Color.FromArgb 0xff800000
            Color.FromArgb 0xffaaffc3
            Color.FromArgb 0xff808000
            Color.FromArgb 0xffffd8b1
            Color.FromArgb 0xff000075
            Color.FromArgb 0xff808080
        ]

    let mutable colors = Map.empty

    let getColor (name : string) =
        let key = 
            if name.Contains "_" then name.Substring(0, name.IndexOf "_").ToLower()
            else name.ToLower()

        match Map.tryFind key colors with
        | Some c -> c
        | None ->
            let c = allColors.[nextColor]
            nextColor <- nextColor + 1
            colors <- Map.add key c colors
            c


    for file in System.IO.Directory.GetFiles(folder, "*.csv") do
        let data = FSharp.Data.CsvFile.Load(file, separators = ";")

        let series =
            data.Headers.Value |> Array.skip 1 |> Array.map (fun h ->
                h, 
                data.Rows |> Seq.map (fun r -> 
                    let n = r.["N"]
                    float n, float r.[h] / float n
                )
            )
        let c = 
            Chart.Combine [
                for (name, data) in series do
                    let key = 
                        if name.Contains "_" then name.Substring(0, name.IndexOf "_")
                        else name

                    yield Chart.Line(data, key, Color = getColor name)
            ]
            |> Chart.WithXAxis(true, "N", 30000.0, 10.0, true)
            |> Chart.WithYAxis(true, "time [ns]", Min = 0.0)
            |> Chart.WithLegend(true, Docking = ChartTypes.Docking.Bottom, InsideArea = false)
            
            |> Chart.WithTitle(System.IO.Path.GetFileNameWithoutExtension file)

        //Chart.Show c
        Chart.Save (System.IO.Path.ChangeExtension(file, ".png")) c
    0 // return an integer exit code
