---
output: html_document
---

### Sign in:

If you sign in as admin users tab will be unloncked. 

### Data Refresh:

To collect up to date data simply go to the Data tab and click "Collect new data". You can examine and plot your time series aswell (recommended).

### Setting parameters:

Choose the currencies you want to invest in. For example US dollar and Britsih pound. For each currency you will have to fulfill some parameters.

### Type:

We will go long in both currencies, so we select Call both times.

$$ CallOption = max(S_T-K,0) $$ where $S_T = \frac{Currency_T}{EUR_T} * FaceValue$ and $K$ is our Strike.

$$ PutOption = max(K - S_T,0) $$

### Face Value:

Face value of the option. When calculating portfolio value (for multiple currencies) we can think of Face value as how much we want to invest in each currency. We have to adjust Strike accordingly. We set our Face values to $10000$ in USD and $1000$ GBP.

### Strike Value:

Strike value depends on market expectations of future. If today $\frac{USD}{EUR} = 1.10$ and our Face value is set to $10000$ and market expects that the currency ratio will rise in 1 year to $\frac{USD}{EUR} = 1.21$ $(10\%)$ fair value of Strike would be $11000$ (in that case premium would be 0).

So if we set Strike to $11000$ for USD and premium is 0, the currency ratio need to rise more than $10\%$ in order for us to be in money. If for example if ratio after 1 year is $1.42$ our option value is $\frac{1.42}{1.10}*FaceValue - Strike - Premium= 1909$. 

We set Strike to $10500$ for USD and $1000$ for GBP.

### Premium:

How much we have to pay to take an options like we determined above. Market expectaionts for change in ratio $\frac{GPB}{EUR}$ are $-2\%$. For $\frac{USD}{EUR}$ is $10\%$.

So we have to pay $500$ to take USD option and for GBP we set premium to $10$. Because the $\frac{Strike}{FaceValue}=0\%$ and the market expectaions are $-2\%$, we are actually taking more risk with GBP option than with USD.

Remember that with options we cannot lose more than premium.

After computation the program will compute the fair premium that one needs to pay for chosen portfolio.

### Rolling vollatility:

We do not expect GBP to be any more volatile that it is, so with GBP we choose "No".

But we do not expect the same for USD. Because of the recents events we believe that USD will be more volatile than in the past. So we choose "Yes" to RV and we leave alpha to its default value at $0.8$. Higher $\alpha$ mean higher expectaions in increase of volatility.

### Smoothing factor:

With it we can control Bootstrapping probability. $N$ is the number of the past observations, where N is the most distant one. $\beta$ is smoothing factor.

$$ prob_1 = 1  $$
$$ prob_i = prob_{i-1}*\beta $$
$$ i \in \{ 2,...,N\}$$
$$ P = \frac{[prob1,...,prob_N] }{ sum(prob_1 + ...+ prob_N)} $$
We choose $0.99$, since we believe that recent events holds more value than distant past.


### Time period in days:

Number of bussines days that you intend to hold your portfolio of options.

We will choose $66$ because we agreed on 3 months duration.

### Final quantile:

If $M$ is number of simulations, you will retrieve $M$ possible portfolio values. Quantile will determine which one will be your result. It can be changed afterwards.

We choose $0.5$ since we are predicting expected value.

We leave Number of simulations at 100000 and confirm our choice.

We are redirected to the computation page where we manually start the calculations. We can close modal and wait for result to appear.

### Results:

Program will display expected return at chosen quantile, aswell as density of possible returns. We can expect a lot of density mass concentrated at total premium cost, because we do not execute options if they are not in our favour. Relative profit is also computed. Maximum loss is of course sum of singular premiums.

We can change quantile parameter or export result at export page. 

Below the density of option returns is density for futures with same parameters. The difference here is that if the value of portfolio drops below $0$, we are obligated to pay the difference, so returns can be negative. I have set premiums to $0$ for each of the futures.

### Remark:

It is interesting to observe at which quantile futures return outperform options. We pay premium for safety. For example if we set quantile to $1$, we can see that futures return is exactly for total premium higher than options return. It make sense because in the good scenario none of our options closed "out of the money", so we paid premium for nothing. Of course this scenario is highly unlikely, yet still possible.



