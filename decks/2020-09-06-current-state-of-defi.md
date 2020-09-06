---
title: Current state of DeFi
reveal: true
theme: white
summary: |
    Decentralized Finance or DeFi all the rage now pulling in billions of dollars into shaky smart contracts over the period of hours. Keeping track of food-finance protocols is certinaly a full time job. I spent a weekend to get a lay of the land and this here is the result. Enjoy.
featureimage: 2020/09/current-state-of-defi/defi.png
---

<section>
<section data-background-image="defi.png" data-background-size="strech" data-background-repeat="no-repeat" style="color: white">
<h2 style="color: darkorange">
Current state of DeFi
</h2>

Gísli Kristjánsson
<small>

September 6, 2020
</small>
</section>

<section>
The main idea is to extend the use-cases of public blockchains to not only offer cryptocurrency as a form of money but to encompass more complex financial transactions
</section>

<section>
#### Use cases

- Stable coins
- Spot and derivates trading
- Credit facilities and money markets
- Insurance
- Prediction markets
- Staking
</section>

<section>
DeFi is still highly experimental and there are _huge_ risks associated with various platforms, people will lose money and there will be crying 😭
</section>

<section>
However, it might also bring real innovation, just like not all the ICO projects turned out to be scams ☠️
</section>
</section>

<section>
<section>
## Liquidity mining ⛏
</section>

<section>
The central idea is that you earn yield by providing liquidity to other participants
</section>

<section>
Early protocols used order books but automatic market makers are winning

- Dharma vs Compound
- 0x vs Uniswap
</section>

<section>
Liquidity is provided by locking tokens in a pool 🎱

\[ token_A * token_B = K \]
</section>

<section>
![bonding curve](bonding-curve.png)
\[ y  = \frac{k}{x} \implies \Delta P = \frac{dy}{dx} = - \frac{k}{x^2}  \]
</section>

<section>
Arbitrageurs are incentivized to keep the the pool balanced 
<p class="fragment">
LPs receive liquidity tokens
</p>
<p class="fragment">
Optionally LPs receive a governance token
</p>
</section>

<section>
The main metric to watch is the Total Value Locked

| 2020 | TVL $MM |
|------|---------|
| Jan  | 690     |
| Feb  | 1,000   |
| July | 2,000   |
| Sept | 9,000+  |
</section>
</section>

<section>
<section>
## Building blocks 🧱
</section>

<section>
### Pegged tokens

| &nbsp; |   |
|-----| ------ |
| wBTC <a href="https://wbtc.network"><img style="vertical-align: middle" height="100" src="wbtc.png" /></a> |   BTCB <a href="https://info.binance.com/en/currencies/bitcoin-bep2"><img style="vertical-align: middle" height="100" src="btcb.png" /></a>   |
| tBTC <a href="https://tbtc.network/"><img style="vertical-align: middle" height="100" src="tbtc.png" /></a> | renBTC <a href="https://renproject.io/"><img style="vertical-align: middle" height="100" src="renbtc.png" /></a> |
| &nbsp;  |  |
</section>

<section>
### Oracles
Chainlink <a href="https://chain.link"><img style="vertical-align: middle" height="100" src="chainlink.png" /></a>
</section>
</section>

<section>
<section>
## Protocols  🧬
</section>

<section>
### MakerDAO <a href="https://makerdao.com"><img style="vertical-align: middle" height="100" src="makerdao.png" /></a>
<img class="r-strech" src="maker-tvl.png" alt="">
</section>

<section>
### DAI <a href="https://oasis.app/"><img style="vertical-align: middle" height="100" src="dai.png" /></a>
$DAI is decentralized stable coin with an associated governance token $MKR
</section>

<section>
### DAI <a href="https://oasis.app/"><img style="vertical-align: middle" height="100" src="dai.png" /></a>
$DAI is a debt which is minted when a supported token is locked in a CDP
<p class="fragment">
The collateral is released when the debt is paid back
</p>
<p class="fragment">
Debtors must maintain a 150% margin
</p>
</section>

<section>
### Synthetix <a href="https://synthetix.exchange/"><img style="vertical-align: middle" height="100" src="synthetix.png" /></a>
Synthetix is a generalized version of $DAI
<p class="fragment">
Synths can track any asset
</p>
<p class="fragment">
LPs receive the $SNX governance token
</p>
</section>

<section>
### Ampleforth <a href="https://www.ampleforth.org/"><img style="vertical-align: middle" height="100" src="ampleforth.png" /></a>
$AMPL is a digital currency that adjusts supply daily based on market conditions
</section>

<section>
### Uniswap <a href="https://uniswap.org/"><img style="vertical-align: middle" height="100"  src="uniswap.png" /></a>
Exchange AMM where pools contain 50/50 split of tokens in a pair
<p class="fragment">
Pools can source liquidity from other pools
</p>
<p class="fragment">
V1 not upgradable but V2 has admin key for fees
</p>
<p class="fragment">
Native price oracles but initial versions attackable
</p>
</section>

<section>
### Uniswap <a href="https://uniswap.org/"><img style="vertical-align: middle" height="100"  src="uniswap.png" /></a>
<img class="r-stretch" src="uniswap-vs-coinbase.png" alt="Uniswap vs. Coinbase">
</section>

<section>
### Balancer <a href="https://balancer.finance/"><img style="vertical-align: middle" height="100" src="balancer.png" /></a>
Generalized version of Uniswap
<p class="fragment">
Pools contain up to 8 tokens with any distribution
</p>
<p class="fragment">
LPs receive $BAL token
</p>
</section>

<section>
### Mooniswap <a href="https://mooniswap.exchange/#/swap"><img style="vertical-align: middle" height="100" src="mooniswap.png" /></a>
Exchange AMM protocol by 1inch
<p class="fragment">
Shares slippage fees with LPs
</p>
</section>

<section>
### Curve <a href="https://www.curve.fi/"><img style="vertical-align: middle" height="100" src="curve.png" /></a>
Specialized exchange for stablecoins
<p class="fragment">
The sBTC, WBTC, renBTC, BTCB are "stablecoins" amongst themselves
</p>
<p class="fragment">
Beats most CEXes on liquidity
</p>
<p class="fragment">
Liquidity is supplied to Compound, Synthetix or yEarn where it generates more income for LPs
</p>
</section>

<section>
<img class="r-stretch" src="bonding-curve2.png" alt="Bonding curves">
</section>

<section>
### 1inch <a href="https://1inch.exchange/"><img style="vertical-align: middle" height="100" src="1inch.png" /></a>
DEX aggregator
<p class="fragment">
Best rates by splitting orders among multiple DEXes
</p>
</section>

<section>
### Compound <a href="https://compound.finance/"><img style="vertical-align: middle" height="100" src="compound.png" /></a>
Compound is a money market AMM
<p class="fragment">
LPs get the $COMP governance token
</p>
</section>

<section>
### AAVE <a href="https://aave.com/"><img style="vertical-align: middle" height="100" src="aave.png" /></a>
Compound is a
Similar to compound
<p class="fragment">
Pioneered flash loans
</p>
</section>

<section>
### dydx <a href="https://dydx.exchange/"><img style="vertical-align: middle" height="100" src="dydx.png" /></a>
Derivaties exchange and margin trading
</section>
  
<section>
### yEarn <a href="https://yearn.finance/"><img style="vertical-align: middle" height="100" src="yearn.png" /></a>
DeFi yield aggregator

<p class="fragment">
The most current optimal strategy to maximize returns
</p>

<p class="fragment">
Roboadvisor
</p>

<p class="fragment">
Profit switching lender to optimize lending yields 
</p>
</section>

<section>
### yearn <a href="https://yearn.finance/"><img style="vertical-align: middle" height="100" src="yearn.png" /></a>
The yearn.finance ecosystem is controlled by the $YFI token

<p class="fragment">
$YFI a completely valueless 0 supply token. We reiterate, it has 0 financial value
</p>

<p class="fragment">
$YFI rallying by more than 4,000% within days of release. 
</p>
</section>
</section>

<section>
<section>
## Yield farming 👨‍🌾
</section>

<section>
> the degens are just yolo-ing in!

<p class="fragment">
The names are more meme  🚀
</p>
<p class="fragment">
Rube Goldberg machine for money  🤯
</p>
<p class="fragment">
Mostly unaudited code 😱
</p>
<p class="fragment">
Often anonymous teams 🤡
</p>
<p class="fragment">
The power of composability 🧩
</p>

</section>

<section>
### YAM <a href="https://yam.finance/"><img style="vertical-align: middle" height="100" src="yam.png" /></a>
A stablizing reserve currency protocol (see Ampleforth)
<p class="fragment">
Fair distribution
</p>
<p class="fragment">
$YAM uses yCRV as the reserve currency, which is roughly a $1 peg
</p>
<p class="fragment">
$500MM TVL in 24h
</p>
<p class="fragment">
Smart contract bug caused hyperinflation (off by \( 10^18 \)) within 48h
</p>
</section>

<section>
### YAM <a href="https::/yam.finance/"><img style="vertical-align: middle" height="100" src="yam.png" /></a>
<img class="r-stretch" src="yam-price.jpg" alt="YAM price">
</section>

<section>
### SushiSwap <a href="https://sushiswap.org/"><img style="vertical-align: middle" height="100" src="sushiswap.png" /></a>
Promoted itself as an "evolution" of Uniswap
<p class="fragment">
$700MM TVL in 72h and then $1.1B
</p>
<p class="fragment">
Chef Nomi exits with $13MM of Sushi's development fund
</p>
<p class="fragment">
Sold half of the $27MM dev fund that he said he wouldn't touch without community approval
</p>
<p class="fragment">
Ironically uses Uniswap to convert $SUSHI to $ETH
</p>
</section>

<section>
### SushiSwap <a href="https://sushiswap.org/"><img style="vertical-align: middle" height="100" src="sushiswap.png" /></a>
<img class="r-stretch" src="chef-nomi.png" alt="Chef Nomi on Twitter">
</section>

<section>
### Kimchi <a href="https://kimchi.finance/"><img style="vertical-align: middle" height="100" src="kimchi.png" /></a>
<img class="r-stretch" src="kimchi-explained.png" alt="Kimchi explained">
</section>

<section>
### Cream <a href="https://cream.finance/"><img style="vertical-align: middle" height="100" src="cream.png" /></a>
<p class="fragment">
Based on Compound
</p>
<p class="fragment">
Deployed to Binance Smart Chain
</p>
</section>

<section>
### yETH <a href="https://yearn.finance/"><img style="vertical-align: middle" height="100" src="yeth.png" /></a>
<img class="r-stretch" src="yeth-tweet.png" alt="yEth tweet">
</section>

<section>
### yETH <a href="https://yearn.finance/"><img style="vertical-align: middle" height="100" src="yeth.png" /></a>
Latest vault from yEarn
<p class="fragment">
$14MM TVL in 48 hours
</p>
<p class="fragment">
Uses MakerDAO's OSM to delay liquidation by 1 hour
</p>
</section>

<section data-background-image="yeth-flow.jpg" data-background-size="contain" data-background-repeat="no-repeat">
</section>
</section>

<section>
<section>
## Staking 🥩
</section>

<section>
Proof of stake is a type of consensus algorithm
<p class="fragment">
Next block is chosen via combinations of random selection and stake
</p>
<p class="fragment">
Nothing at stake problem solved with slashing
</p>
<p class="fragment">
Security relies on staking rewards
</p>
</section>

<section>
1/3<sup>rd</sup> of stake needed to attack the network 🦖 
<p class="fragment">
Attacker could offer stakers more attractive yield than the PoS protocol
</p>
<p class="fragment">
DeFi lending markets directly compete with staking
</p>
</section>
</section>

<section data-background-image="thats-a-wrap.gif" data-background-size="contain" data-background-repeat="no-repeat">

</section>
