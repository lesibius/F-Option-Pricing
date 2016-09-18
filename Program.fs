(****************************************************************************************************************)
(*                                      NameSpaces Declaration                                                  *)
(****************************************************************************************************************)
namespace Exotic_Pricing.Main

open System
  
(****************************************************************************************************************)
(*                                                  Main                                                        *)
(****************************************************************************************************************)

module MainPricing = 
    
    open Exotic_Pricing.Payoff.Payoff
    open Exotic_Pricing.Paths.Paths
    open Exotic_Pricing.Pricing.Pricing
    open Exotic_Pricing.Util.Util

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
        let rf = -0.01
        let S0 = 60.0
        let sigma = 0.1095

        let alpha = 0.1884
        let beta = 0.0
        let rho = -0.93

        //Monte carlo settings
        let nPeriod = 100
        let nSimul = 100000

        //Creating the paths
        Console.WriteLine("Generating {0} trajectories ({1} points)", nSimul, nSimul * nPeriod)
        //let setOfPaths = performancetime (fun () ->  (fun x -> sabrprocess nPeriod tmin tmax S0 rf sigma alpha beta rho) |> Array.init nSimul) //SABR Model
        let setOfPaths = performancetime (fun () -> (fun x -> bachelierprocess nPeriod tmin tmax S0 rf sigma) |> Array.init nSimul)


        //Defining payoffs
        //Call with one month maturity, put with one year maturity
        let maturityindex = indextime nPeriod tmin tmax (1.0/12.0)
        let plainvanillacall = maturityindex |> ((callpayoff K) |> europeanoptionpayoff) //Provide index for specific maturity
        let plainvanillaput = None |> ((putpayoff K) |> europeanoptionpayoff) //Provide None to declare tmax as maturity

        let asiancall = asianarithmeticpayoff (callpayoff K) nDivision
        let asianput = asianarithmeticpayoff (putpayoff K) nDivision

        let KOPayoffMeanHoldAsset = underlyingpayoff                            //Knock-out = payoff of holding asset from inception
        let KOPayoff x = 0.0
        let callwithknockoutbarrier = barrier (upbarrier KObarrierUp) plainvanillacall KOPayoff
        let putwithknockoutbarrier = barrier (downbarrier KObarrierDown) plainvanillaput KOPayoff
        let capitalprotectionwithknockoutbarrier = barrier (downbarrier KObarrierDown) plainvanillacall KOPayoffMeanHoldAsset
    
    

        let strangecall = barrier (downbarrier KObarrierDown) (barrier (upbarrier KObarrierUp) plainvanillacall asiancall) KOPayoffMeanHoldAsset

        //Get Results

        printoptionvalue "plain vanilla call" (optionpricing plainvanillacall setOfPaths rf tmin tmax)
        printoptionvalue "plain vanilla put" (optionpricing plainvanillaput setOfPaths rf tmin tmax)
        printoptionvalue "plain vanilla call with knock-out up barrier (KO = 0 payoff)" (optionpricing callwithknockoutbarrier setOfPaths rf tmin tmax)
        printoptionvalue "plain vanilla out with knock-out up barrier (KO = 0 payoff)" (optionpricing putwithknockoutbarrier setOfPaths rf tmin tmax)
        printoptionvalue "capital protection with knock-out down barrier (KO = hold asset)" (optionpricing capitalprotectionwithknockoutbarrier setOfPaths rf tmin tmax)
        printoptionvalue "Asian call" (optionpricing asiancall setOfPaths rf tmin tmax)
        printoptionvalue "Asian put" (optionpricing asianput setOfPaths rf tmin tmax)
        //I don't think this type of asset exists, but what it does is: if you go below the bottom barrier, then your investment is lost
        //else, if you go above the up barrier, your call becomes an Asian call, averaging high values with low values
        //To have the full possibility of a call, the underlying must stay below these two barriers (and above the strike)
        printoptionvalue "strange kind of call" (optionpricing strangecall setOfPaths rf tmin tmax)


        //Uncomment to get plot of one trajectory
        //setOfPaths  |> Array.item(12) |> Array.toList |> plot

        0 // retourne du code de sortie entier
