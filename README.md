# F#-Option-Pricing

Monte-Carlo pricing with F#

Author: Clémentin Castellano


## Comments

This code has been produced for self entertainment. It allows to price various options (plain vanilla, Asian, barriers...), provided that the payoff can be viewed as a function of the underlying path.

The generic steps to price an option are the following:
<li>Generate option payoffs. These variables have the following form: float [] -> float, where the float [] input represents the path of the underlying asset for one iteration and the float output is the derivative's value at expiry.
<li>Generate underlying trajectories. These paths are set beforehand in order to be used multiple times and avoid to compute new trajectories for different options having the same underlying. For now, the european payoff uses the Array.last value, but in future refinement of the program, an item index will be calculated using tmin and tmax to use the same paths for various maturities. Trajectories are stored as float [] [] and are accessed as float [] by payoff functions
<li>Price the options using the payoff functions and the paths generated. The current pricing () function returns a float * float value with the following form: (average value, sample standard deviation / sqrt(sample size))

The only goal that I set forth for myself was that the program should be only coded using functional programming features of F#. 

In term of speed, the first version of this code used list<float> and was quite slow. The latest version use immutable arrays, although my own constraint forbad me to use this feature. It takes approximately 5 minutes to generate 100'000 trajectories of 1'000 asset prices (thus 100m points) with an arithmetic brownian motion.

For now, ~~three~~ two models are available to price options:
<li>The Bachelier process (arithmetic brownian motion)
<li>The Black-Scholes process (geometric brownian motion)
<li>~~The SABR model (stochastic volatility)~~

I noticed a slight mistake in the SABR model on which some work is necessary. I will correct this later on as I need to go back to the literature to make sure my computations are correct.

## Latest Changes

Initially, all functions were put in the same file. Functions are now located in different files for a matter of readability of the program.

If you wish to test this application, it is advised to put the Util.fs file before other files in your Visual Studio environment.

## Further Improvement

Paths' calibration is on-going.

In future versions, I would like to add an Excel module that would use a pre-formatted workbook to allow the user to change the parameters (strike, type, number of trajectories...) directly from Excel.

Another potentially interesting development would be to increase the computation speed. Although moving to float [] [] from list\<list\<float\>\> helped, it can be improved. As of today, I am considering implementing the path generation in Fortran. The main value added of F# remains its flexibility in defining the derivatives' payoff and keeping this part within F# while letting Fortran calculating the paths seems a good idea.

Some other motion are under consideration for addition, such as the Heston model, jump processes (e.g. the Bates model) or the Dupire model.

I wish to add a module to implement scenario analysis, by pricing the option at tmin and pricing it again a t (not necessarily tmax) under different scenarios and comparing their characteristics (gain/losses, hedging cost...).

