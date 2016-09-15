namespace Exotic_Pricing.Payoff


(****************************************************************************************************************)
(*                                      Basic Option Functions                                                  *)
(****************************************************************************************************************)
(*
The main basic option function is payoff (Array<float>->float)-> Array<float>->float
The first argument is a high order function (generally obtained by currying) that takes as an argument a list of value
*)


module Payoff =


    let payoff payoffFun assetPath = 
        payoffFun assetPath

    //Underlying payoff
    let underlyingpayoff = fun (assetPath:float []) -> (assetPath |> Array.last) - (assetPath |> Array.item(0))


    //Plain vanilla option
    let callpayoff strike = fun price -> max (price - strike) 0.0
    let putpayoff strike = fun price -> max (strike - price) 0.0

    //European options
    let europeanoptionpayoff payoffFun timeindex = 
        fun assetPath -> 
            match timeindex with
            |Some value -> assetPath |> Array.item(value) |> payoffFun
            |None -> assetPath |> Array.last |> payoffFun
        

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





