---
output: html_document
---
## <span style="color:brown">Theory:</span>


### <span style="color:brown">We transform our TS:</span>

$$ NewTimeSerie = diff(log(TimeSerie)) $$

### <span style="color:brown">If we do stress prediction:</span>

We apply rolling volatilty multiplier. Basically we roll over our TS and calculate vollatilty on smaller time frames. For example if we choose window of length 10, then we will calculate volatilty of $ts_1,...,ts_{10}, ts_2,...,ts_{11}$ to $ts_{n-9},...,ts_n$, where $n$ is the lentgh of our time serie. Let's call this TS $RollVollTS$.

Now we multiply correct our TS with:

$$ NewTimeSerie = NewTimeSerie  *  \frac {Quantile(\alpha,RollVollTS)}{Vollatilty(NewTimeSerie)} $$

where $\alpha$ can be $\in [0.5,1]$.

###  <span style="color:brown">Simulating value at time {t + k}:</span>

We choose $k$, which means how many incremnets ahead we want to simulate. We randomly choose k values of our TS and then compute:

$$tail(TS,1)* \exp(\sum_1^k{sample(k,TS)} - k*Mean(TS) - k*0.5*Vollatilty(TS))   $$

Where tail represents the last value of our $TS$. We repeat that $N$ times for each $TS$ in our portfolio. Right now our simulations are still independent.

### <span style="color:brown">Correlating simulations:</span>

This is relatively easy trick. If we think what does copula return? 

It gives us correlated probabilities. On this probabilities we can apply arbitrary distributions, like LogNormal or Gamma or Pareto... Or, we can empirically estimate the distribution of our simulations and take the appropriate quantile. We apply that to each $TS$ we have simulated.

Copulas come in handy if we simulate 2 or more $TS$.

### <span style="color:brown">Last steps:</span>

Now we have to evaluate each simulation (we have $N$ of them) in terms of portfolio value and then take the wanted quantile of simulations. Notation is as usual in formulating Options (read an example). 

Call options: $max(S_{t+k} - K, 0) - Premium$ 

Put options: $max( -S_{t+k} + K, 0) - Premium$

$S_T$ : $\frac{TS_{T+k}}{TS_T} * FaceValue$


If we want stress estimation we will take something around $0.05$, for exact estimation something around $0.50$.











