namespace Exotic_Pricing.Paths

open MathNet.Numerics.Distributions
(****************************************************************************************************************)
(*                                          Asset Path Functions                                                *)
(****************************************************************************************************************)



module Paths =
    
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
    let blackscholesprocess nIter tmin tmax S0 rf sigma =
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




    

