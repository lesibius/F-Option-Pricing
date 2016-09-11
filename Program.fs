(****************************************************************************************************************)
(*                                      NameSpaces Declaration                                                  *)
(****************************************************************************************************************)

open System
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
//Charting doesn't work
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Drawing
open System.Windows.Forms

(****************************************************************************************************************)
(*                                              Util Functions                                                  *)
(****************************************************************************************************************)

    
let printoptionvalue optionName optionvalue =
    printfn "Value of %s" optionName
    printfn "%A" optionvalue
    printfn ""


(****************************************************************************************************************)
(*                                              Plot Functions                                                  *)
(****************************************************************************************************************)

let plot (assetPath:list<float>) =
    let myChart = assetPath
                    |> Chart.Line |> Chart.WithYAxis(Title="Test")
    let myChartControl = new ChartControl(myChart, Dock=DockStyle.Fill)
    let lbl = new Label(Text="my label")
    let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
    form.Controls.Add lbl
    form.Controls.Add(myChartControl)
    do Application.Run(form) |> ignore



(****************************************************************************************************************)
(*                                      Basic Option Functions                                                  *)
(****************************************************************************************************************)
(*
The main basic option function is payoff (list<float>->float)-> List<float>->float
The first argument is a high order function (generally obtained by currying) that takes as an argument a list of value
*)

let payoff payoffFun (assetPath:list<float>) = 
    payoffFun assetPath

//Plain vanilla option
let callpayoff strike = fun price -> max (price - strike) 0.0
let putpayoff strike = fun price -> max (strike - price) 0.0

//European options
let europeanoptionpayoff payoffFun = fun assetPath -> assetPath |> List.last |> payoffFun

//Asian options
//let asiaarithmeticpayoff payoffFun strike = fun assetPath -> assetPath |> List.average |> payoffFun

//Barriers
//If two barrier, pass the whole function through this two times
let barrier barrierfun payoffNoBarrier payoffBarrier =
    fun assetPath -> 
        let action = assetPath |> List.tryFind barrierfun
        match action with
        |Some value -> payoffBarrier assetPath
        |None -> payoffNoBarrier assetPath

//High order function for barrierfun argument
let upbarrier barrier = fun x -> x > barrier
let downbarrier barrier = fun x -> x < barrier

(****************************************************************************************************************)
(*                                          Asset Path Functions                                                *)
(****************************************************************************************************************)

let getrandomnormal nIter =
    let norm = new Normal(0.0,5.0)
    (Seq.take  nIter (norm.Samples())) |> Seq.toList

let bachelierprocess nIter tmin tmax S0 rf sigma =
    let rnd = new Normal(0.0,1.0)
    let deltat = (tmax - tmin) / (float nIter)
    let sigmabar = sqrt(deltat) * sigma
    let drift = exp (rf * deltat)
    let rec loop S t listacc =
        match t with 
        | n when n > tmax -> listacc
        | _ -> 
            let templist = List.append listacc ((S * (drift + sigmabar * rnd.Sample())) |> List.singleton )
            loop (templist |> List.last) (t+deltat) templist  
    loop S0 0.0 List.Empty


(****************************************************************************************************************)
(*                                          Monte Carlo Pricing                                                 *)
(****************************************************************************************************************)  

let optionpricing payoffFun (setOfPaths:list<list<float>>) rf =
    let valuesatexpiry = setOfPaths |> List.map payoffFun
    (valuesatexpiry |> List.average) * exp (-rf)
  
(****************************************************************************************************************)
(*                                                  Main                                                        *)
(****************************************************************************************************************)



[<EntryPoint>]
let main argv = 
    
    // Option characteristics
    let K = 55.0                        //Plain vanilla strike
    let KObarrierUp = 70.0              //Knock-out barrier up
    let KObarrierDown = 50.0            //Knock-out barrier down
    let tmin = 0.0
    let tmax = 1.0
    

    //Underlying characteristics
    let rf = 0.0
    let S0 = 60.0
    let sigma = 0.1095

    //Monte carlo settings
    let nPeriod = 100
    let nSimul = 100000
    

    //Creating the paths
    let setOfPaths = (fun x -> bachelierprocess nPeriod tmin tmax S0 rf sigma) |> List.init nSimul

    //Defining payoffs
    let plainvanillacall = (callpayoff K) |> europeanoptionpayoff
    let plainvanillaput = (putpayoff K) |> europeanoptionpayoff
    
    let KOPayoffMeanHoldAsset x = (x |> List.last) - S0                         //Knock-out = payoff of holding asset from inception
    let KOPayoff x = 0.0
    let callwithknockoutbarrier = barrier (upbarrier KObarrierUp) plainvanillacall KOPayoff
    let capitalprotectionwithknockoutbarrier = barrier (downbarrier KObarrierDown) plainvanillacall KOPayoffMeanHoldAsset

    printoptionvalue "plain vanilla call" (optionpricing plainvanillacall setOfPaths rf)
    printoptionvalue "plain vanilla put" (optionpricing plainvanillaput setOfPaths rf)
    printoptionvalue "plain vanilla call with knock-out up barrier (KO = 0 payoff)" (optionpricing callwithknockoutbarrier setOfPaths rf)
    printoptionvalue "capital protection with knock-out down barrier (KO = hold asset)" (optionpricing capitalprotectionwithknockoutbarrier setOfPaths rf)

    (setOfPaths.Item 12) |> plot
        
    0 // retourne du code de sortie entier
