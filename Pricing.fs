namespace Exotic_Pricing.Pricing


module Pricing =
    
    open Exotic_Pricing.Util.Util

(****************************************************************************************************************)
(*                                          Monte Carlo Pricing                                                 *)
(****************************************************************************************************************)  

    let optionpricing payoffFun setOfPaths rf tmin tmax =
        let valuesatexpiry = setOfPaths |> Array.map payoffFun
        let avg = valuesatexpiry |> Array.average
        let std = valuesatexpiry |> samplestd avg
        (avg * exp (-rf*(tmax - tmin)), std * exp (-rf*(tmax - tmin)))

