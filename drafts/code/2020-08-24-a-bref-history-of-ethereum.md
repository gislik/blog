---
title: A brief history of Ethereum
---

Soon after Bitcoin launched ideas around tracking other assets started to emerge. The initial attempts included efforts around forking the Bitcoin codebase and create coin-specific blockchains, so-called alt-coins[^1]. This was a tedious process often taking months or years of development depending on how much the alt-coin's implementation deviated from Bitcoin. Later attempts include colored coins where the idea was that certain bitcoins would be repurposed to carry a second meaning. Colored coins would theoretically reduce development time and rely in part on the security of Bitcoin as opposed to the alt-coins having to compete for the security of its network with other blockchains.

The fundamental insight which the creators of Ethereum had was that Bitcoin were inherently replicated state machines with *fixed set* of transformation rules[^2]. The network participants came to consensus by validating each transformation in sequence until they arrived at the current state. Ethereum's core contribution, originally laid out in the whitepaper, was to define low-level operations for a virtual machine which formed the rules for a valid computation from one state to another. The functionality of such a hypothetical blockchain would be extendible by uploading a bytecode for the virtual machine in a contract creation transaction. Once the bytecode had been added to the replicated state it could be invoked in a contract call transaction instructing the miners to compute a new state by executing the bytestring on the current state. Such extensions would later be branded as smart contracts.

> _At the start of the sale and for fourteen days the price was set so that one bitcoin bought 2,000 ether. At the end of the 14-day period the amount would decline linearly to a final rate of 1,337 ether, which meant that one ether was worth 0.0007479 bitcoin or about 30 cents at bitcoin prices in September 2014._

To fund the development of Ethereum a pre-sale was conducted where bitcoin holders were invited exchange their bitcoins for a private key on Ethereum. The initial account state of pre-allocated ether[^3] would be derived from the amount of bitcoin raised and the private key controlled an address containing an amount of ether proportional to the investor's share of the the pre-sale. The original purpose of Ether was to pay for computation on the Ethereum network akin to pre-paid credits with a cloud vendur -- giving Ether its perceived value. During the pre-sale 60 million ether were auctioned off for the equivilent of $18.3 million worth of bitcoins. After a successful deployment of the _Olympic_ testnet, the _Frontier_ mainnet, was launched on July 30^th^, 2015.

A few years earlier independent developer studio Zynga had released a number of online games on the Facebook platform. After launching in 2009, FarmVille became the most popular game on the Facebook's platform generating hundreds of millions of dollars in revenue. Zynga, attributing much of its early success in marketing and distributing their games to the Facebook platform, started to worry about the risks stemming from Facebook's ability to change the rules post-factum. This kind of risk, later referred to ask platform risk, materialized as Facebook started to pull the rug out from under Zynga's feet. Ethereum, the smart contract platform, started to attract the attention of regular developers who suddenly had an alternative to the centralized platforms to build on where the rules were encoded from the get go into the protocol and enforced collectively by the decentralized group maintainers of the network. As with any public blockchain anyone could add to the security of Ethereum and since any vialation of the network rules would have a negative effect on the price of Ether, maintainers were incentivized to enforce the rules.

> _A large portion of our business is dependent upon, and our bookings and revenues are derived from, the Facebook platform, and Facebook in many cases has the unilateral ability to interpret its policies and terms and conditions for applications and developers._[^4]

Tokens were the first class of smart contracts to gain mass adoption. They were a new take on the old idea of alt-coins and colored coins. Tokens could be launched in days or weeks reducing development efforts by many orders of magnitude and relied purely on Ethereum for its security. Originally envisioned to track the ownership and transfers of other financial instruments tokens became an asset class by itself in the ICO[^5] mania where teams would collect hundreds of million of dollars worth of ether in a few minutes on the back of a whitepaper outlining the scope and design of the project. The tokens were neither equity nor debt in the capital structure of a company but in many cases were designed to have a closer relationship with the project's underlying protocol. This included access to it once it was released similar to a prepaid membership token, participation in the governance of the project, or a mechanism to collect fees generated by the protocol. Many of the projects turned out to be blatant scams others would spend the next couple of years silently researching and building a product and a community around them slowly accruing value to the tokens to become among the top 20 coins by market cap.

# A fork in the road

DAO[^6]

The DAO hack

Stablecoins
defi


[^1]: The first blockchain to launch off a forked version of Bitcoin was Litecoin ... namecoin. Alt-coins had to bootstrap the network ...
[^2]: Bitcoin script ... 
[^3]: Ether is the native cryptocurrency of Ethereum
[^4]: Zynga: [Quarterly report which provides a continuing view of a company's financial position](https://investor.zynga.com/static-files/4e78216b-6e1c-42ed-a3ed-f9d7b0752b97)
[^5]: Initial Coin Offering (ICO) is a spin on it's real-world Initial Public Offering (IPO)
[^6]: Decentralized Autonomous Organization (DAO) ...
