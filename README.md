# F#-Option-Pricing

Monte-Carlo pricing with F#

Author: Clémentin Castellano


## Comments

This code has been produced for self entertainment. It allows to price various options (plain vanilla, asian, barriers...), provided that the payoff can be viewed as a function of the underlying path.

The generic steps to price an option are the following:
<li>Generate option payoffs. These variables have the following form: T' -> (float [] -> float)
<li>Generate underlying trajectories. These paths are set beforehand in order to be used multiple times and avoid to compute new trajectories for different options having the same underlying. For now, the european payoff uses the Array.last value, but in future refinement of the program, an item index will be calculated using tmin and tmax to use the same paths for various maturities. Trajectories are stored as float [] [] and are accessed as float [] by payoff functions
<li>Price the options using the payoff functions and the paths generated.

The only goal that I set forth for myself was that the program should be only coded using functional programming features of F#. 

In term of speed, the first version of this code used list<float> and was quite slow. The latest version use immutable arrays, although my own constraint forbad me to use this feature. It takes approximately 5 minutes to generate 100'000 trajectories of 1'000 asset prices (thus 100m points).

For now, three models are available to price options:
<li>The Bachelier process (arithmetic brownian motion)
<li>The Black-Scholes process (geometric brownian motion)
<li>The SABR model (stochastic volatility)

## Latest Changes

Initially, all functions were put in the same file. Functions are now located in different files for a matter of readability of the program.

## Further Improvement

In future versions, I would like to add an Excel module that would use a pre-formatted workbook to allow the user to change the parameters (strike, type, number of trajectories...) directly from Excel.

