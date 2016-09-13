(****************************************************************************************************************)
(*                                      NameSpaces Declaration                                                  *)
(****************************************************************************************************************)

open System
open MathNet.Numerics.Distributions
open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Drawing
open System.Windows.Forms

(****************************************************************************************************************)
(*                                              Util Functions                                                  *)
(****************************************************************************************************************)

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



(****************************************************************************************************************)
(*                                      Basic Option Functions                                                  *)
(****************************************************************************************************************)
(*
The main basic option function is payoff (list<float>->float)-> List<float>->float
The first argument is a high order function (generally obtained by currying) that takes as an argument a list of value
*)

let payoff payoffFun assetPath = 
    payoffFun assetPath

//Underlying payoff
let underlyingpayoff = fun assetPath -> (assetPath |> Array.last) - (assetPath |> Array.item(0))


//Plain vanilla option
let callpayoff strike = fun price -> max (price - strike) 0.0
let putpayoff strike = fun price -> max (strike - price) 0.0

//European options
let europeanoptionpayoff payoffFun = fun assetPath -> assetPath |> Array.last |> payoffFun

//Asian options
//These options requires both the number of divisions (e.g. average over 4 periods) and the number of periods per simulation
let asianpayoff payoffFun (priceTime:list<int>) (priceWeigth:list<float>) =
    fun assetPath ->
        let relevantPrices = List.init (priceTime.Length) (fun i ->  (assetPath |> Array.item(priceTime.Item(i)))) 
        let weightedPrices = List.map2 (fun P w -> w * P) relevantPrices priceWeigth
        weightedPrices |> List.sum |> payoffFun

let asianarithmeticpayoff payoffFun nDivision =
    fun assetPath -> 
        let nPeriod = assetPath |> Array.length
        let priceTime = List.init nDivision (fun i -> ((i + 1) * nPeriod / nDivision)-1)
        let priceWeight = List.init nDivision (fun i -> 1.0 / (float nDivision))
        asianpayoff payoffFun priceTime priceWeight assetPath

//Barriers
//If two barrier, pass the whole function through this two times
let barrier barrierFun payoffNoBarrier payoffBarrier =
    fun assetPath ->
        let action = assetPath |> barrierFun
        match action with
        |Some value -> assetPath |> payoffBarrier
        |None -> assetPath |> payoffNoBarrier


//High order function for barrierfun argument
let upbarrier barrier = fun assetPath -> assetPath |>  Array.tryFind (fun x -> x > barrier)
let downbarrier barrier = fun assetPath -> assetPath |> Array.tryFind (fun x -> x < barrier)




(****************************************************************************************************************)
(*                                      Structured Product Functions                                            *)
(****************************************************************************************************************)

let participationcontract participationFactor payoffFun = 
    fun assetPath -> participationFactor * (payoffFun assetPath)

(****************************************************************************************************************)
(*                                          Asset Path Functions                                                *)
(****************************************************************************************************************)

//Arithmetic brownian motion
let bachelierprocess nIter tmin tmax S0 rf sigma =
    let rnd = new Normal(0.0,1.0)
    let deltat = (tmax - tmin) / (float nIter)
    let sigmabar = sqrt(deltat) * sigma
    let dS = fun x ->  x * sigmabar
    let St = fun s ds -> s * ((exp (rf * deltat)) + ds)
    let dSArray = rnd.Samples() |> (Seq.take (nIter - 1)) |>  Seq.toArray |> (Array.map dS)
    Array.scan St S0 dSArray

    

//Geometric brownian motion
let blackscholeprocess nIter tmin tmax S0 rf sigma =
    let rnd = new Normal(0.0,1.0)
    let deltat = (tmax - tmin) / (float nIter)
    let sigmabar = sqrt(deltat) * sigma
    let dW = fun x -> rnd.Sample() * sigmabar
    let St = fun s dw -> s * exp (rf * deltat - ((sigmabar ** 2.0) / 2.0 ) + dw)
    let dSArray = Array.init (nIter - 1) dW
    Array.scan St S0 dSArray


//Shifted lognormal motion
//Warning: this is not a martingale
(*
let shiftedlognormalprocess nIter tmin tmax S0 rf sigma q =
    let rnd = new Normal(0.0,1.0)
    let deltat = (tmax - tmin) / (float nIter)
    let sigmabar = sqrt(deltat) * sigma
    let drift = exp (rf * deltat)
WIP *)


//SABR Model (stochastic volatility)
let sabrprocess nIter tmin tmax S0 rf sigma0 alpha beta rho = 
    let rnd = new Normal(0.0,1.0)
    let deltat = (tmax - tmin) / (float nIter)
    //Wiener processes
    let dV = rnd.Samples() |> (Seq.take (nIter - 1)) |>  Seq.toArray
    let dW = rnd.Samples() |> (Seq.take (nIter - 1)) |>  Seq.toArray
    //Stochastic volatility
    let makeSigma = fun sigma dv -> sigma + alpha * sigma * dv * sqrt(deltat)
    let SigmaArray = (Array.scan makeSigma sigma0 dV).[1..]                 //Don't take SigmaArray[0] -> dimension issues with dV and dW to compute dSArray
    //Infinitesimal underlying price
    let dS sigma dv dw = fun S ->  S0 * ((S / S0) ** beta) * sigma * (rho * dv + (sqrt (1.0 - (rho ** 2.0))) * dw) * sqrt(deltat)
    let dSArray = Array.map3 dS SigmaArray dV dW
    Array.scan (fun s ds -> s + ds s) S0 dSArray



let sabrprocess2 nIter tmin tmax S0 rf sigma0 alpha beta rho =
    let rnd = new Normal(0.0,1.0)
    let deltat = (tmax - tmin) / (float nIter)
    let dS S sigma dV dW = S0 * ((S / S0) ** beta) * sigma * (rho * dV + (sqrt (1.0 - rho ** 2.0)) * dW)
    let dSigma sigma dV = alpha * sigma * dV
    let rec loop S sigma t listacc = 
        match t with
        | n when n > tmax -> listacc
        | _ -> 
            let dV = (sqrt deltat) * rnd.Sample()
            let dW = (sqrt deltat) * rnd.Sample()
            let dSigmaT = dSigma sigma dV
            let sigmat = sigma + dSigmaT
            let dSt = dS S sigmat dV dW
            let St = S + dSt
            loop St sigmat (t+deltat) (List.append listacc (St |> List.singleton))
    loop S0 sigma0 tmin (S0 |> List.singleton)


(****************************************************************************************************************)
(*                                          Monte Carlo Pricing                                                 *)
(****************************************************************************************************************)  

let optionpricing payoffFun setOfPaths rf tmin tmax =
    let valuesatexpiry = setOfPaths |> Array.map payoffFun
    let avg = valuesatexpiry |> Array.average
    let std = valuesatexpiry |> samplestd avg
    (avg * exp (-rf*(tmax - tmin)), std * exp (-rf*(tmax - tmin)))

  
(****************************************************************************************************************)
(*                                                  Main                                                        *)
(****************************************************************************************************************)



[<EntryPoint>]
let main argv = 
    
    // Option characteristics
    let K = 55.0                        //Plain vanilla strike
    let KObarrierUp = 65.0              //Knock-out barrier up
    let KObarrierDown = 50.0            //Knock-out barrier down
    let tmin = 0.0
    let tmax = 1.0
    let nDivision = 4                  //Number of periods to average for asian options
    let participationFactor = 0.4
    

    //Underlying characteristics
    let rf = 0.01
    let S0 = 60.0
    let sigma = 0.1095

    let alpha = 0.1884
    let beta = 0.0
    let rho = -0.93

    //Monte carlo settings
    let nPeriod = 1000
    let nSimul = 100000
    

    //Creating the paths
    Console.WriteLine("Generating {0} trajectories ({1} points)", nSimul, nSimul * nPeriod)
    //let setOfPaths = performancetime (fun () ->  (fun x -> sabrprocess nPeriod tmin tmax S0 rf sigma alpha beta rho) |> Array.init nSimul) //SABR Model
    let setOfPaths = performancetime (fun () -> (fun x -> bachelierprocess nPeriod tmin tmax S0 rf sigma) |> Array.init nSimul)


    //Defining payoffs
    let plainvanillacall = (callpayoff K) |> europeanoptionpayoff
    let plainvanillaput = (putpayoff K) |> europeanoptionpayoff

    let asiancall = asianarithmeticpayoff (callpayoff K) nDivision
    let asianput = asianarithmeticpayoff (putpayoff K) nDivision

    let participationcall = plainvanillacall |> participationcontract participationFactor

    let KOPayoffMeanHoldAsset = underlyingpayoff                            //Knock-out = payoff of holding asset from inception
    let KOPayoff x = 0.0
    let callwithknockoutbarrier = barrier (upbarrier KObarrierUp) plainvanillacall KOPayoff
    let putwithknockoutbarrier = barrier (downbarrier KObarrierDown) plainvanillaput KOPayoff
    let capitalprotectionwithknockoutbarrier = barrier (downbarrier KObarrierDown) plainvanillacall KOPayoffMeanHoldAsset
    
    

    let doubleloose = barrier (downbarrier KObarrierDown) (barrier (upbarrier KObarrierUp) plainvanillacall asiancall) KOPayoffMeanHoldAsset

    //Get Results

    printoptionvalue "plain vanilla call" (optionpricing plainvanillacall setOfPaths rf tmin tmax)
    printoptionvalue "plain vanilla put" (optionpricing plainvanillaput setOfPaths rf tmin tmax)
    printoptionvalue "participation contract (call)" (optionpricing participationcall setOfPaths rf tmin tmax)
    printoptionvalue "plain vanilla call with knock-out up barrier (KO = 0 payoff)" (optionpricing callwithknockoutbarrier setOfPaths rf tmin tmax)
    printoptionvalue "plain vanilla out with knock-out up barrier (KO = 0 payoff)" (optionpricing putwithknockoutbarrier setOfPaths rf tmin tmax)
    printoptionvalue "capital protection with knock-out down barrier (KO = hold asset)" (optionpricing capitalprotectionwithknockoutbarrier setOfPaths rf tmin tmax)
    printoptionvalue "Asian call" (optionpricing asiancall setOfPaths rf tmin tmax)
    printoptionvalue "Asian put" (optionpricing asianput setOfPaths rf tmin tmax)
    //I don't think this type of asset exists, but what it does is: if you go below the bottom barrier, then your investment is lost
    //else, if you go above the up barrier, your call becomes an Asian call, averaging high values with low values
    //To have the full possibility of a call, the underlying must stay below these two barriers (and above the strike)
    printoptionvalue "strange kind of call" (optionpricing doubleloose setOfPaths rf tmin tmax)

    //Uncomment to get plot of one trajectory
    //setOfPaths  |> Array.item(12) |> Array.toList |> plot

    0 // retourne du code de sortie entier
