namespace Exotic_Pricing.Util

open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Drawing
open System.Windows.Forms


module Util = 

    //Printing results
    let printoptionvalue optionName optiondata =
        let (optionvalue,optionstd) = optiondata
        printfn "Value of %s" optionName
        printfn "$%.4f" optionvalue
        let stringCI = sprintf "Interval of confidence: [%.4f,%.4f]" (optionvalue - 1.96 * optionstd) (optionvalue + 1.96 * optionstd)
        printfn "%s" stringCI
        printfn ""

    //Getting standard deviation
    //No sqrt on denominator to avoid extra calculation in CI computation
    let samplestd firstordermoment values =
        (values |> Array.map (fun x -> (x - firstordermoment) ** 2.0) |> Array.sum |> sqrt) / (values |> Array.length |> float )

    //Time for performance
    let performancetime f =
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnvalue = f()
        printfn "Time elapsed: %ims" (timer.ElapsedMilliseconds)
        printfn ""
        returnvalue


    let indextime nPeriod tmin tmax time =
        //Warning: nPeriod is not necessarily the simulation time
        //For instance, while pricing an Asian option ignited in a previous period, nPeriod = nPeriodSimulation + nPeriodBefore
        let deltat = (tmax - tmin) / (float nPeriod)
        match time with
        | t when t < tmax -> Some (((time - tmin) / deltat |> int) - 1)
        |_ -> None
    
    

    (****************************************************************************************************************)
    (*                                              Plot Functions                                                  *)
    (****************************************************************************************************************)

    let plot (assetPath:List<float>) =
        let myChart = assetPath
                        |> Chart.Line |> Chart.WithYAxis(Title="Test")
        let myChartControl = new ChartControl(myChart, Dock=DockStyle.Fill)
        let lbl = new Label(Text="my label")
        let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
        form.Controls.Add lbl
        form.Controls.Add(myChartControl)
        do Application.Run(form) |> ignore







