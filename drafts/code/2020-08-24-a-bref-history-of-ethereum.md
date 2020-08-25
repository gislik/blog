---
title: A brief history of Ethereum
---

Soon after Bitcoin launched ideas around tracking other assets started to emerge. The initial attempts included efforts around forking the Bitcoin codebase and create coin-specific blockchains, so-called alt-coins[^1]. This was a tedious process often taking months or years of development depending on how much the alt-coin's implementation deviated from Bitcoin. Later attempts include colored coins where the idea was that certain bitcoins would be repurposed to carry a second meaning. Colored coins would theoretically reduce development time and rely in part on the security of Bitcoin as opposed to the alt-coins having to compete for the security of its network with other blockchains.

The fundamental insight which the creators of Ethereum had was that Bitcoin were inherently replicated state machines with *fixed set* of transformation rules[^2]. The network participants came to consensus by validating each transformation in sequence until they arrived at the current state. Ethereum's core contribution, originally laid out in the whitepaper, was to define low-level operations for a virtual machine which formed the rules for a valid computation from one state to another. The functionality of such a hypothetical blockchain would be extendible by uploading a bytecode for the virtual machine in a contract creation transaction. Once the bytecode had been added to the replicated state it could be invoked in a contract call transaction instructing the miners to compute a new state by executing the bytestring on the current state. Such extensions would later be branded as smart contracts.

To fund the development of Ethereum a pre-sale was conducted where bitcoin holders were invited exchange their bitcoins for a private key on Ethereum. The initial account state of pre-allocated ether[^3] would be derived from the amount of bitcoin raised and the private key controlled an address containing an amount of ether proportional to the investor's share of the the pre-sale. After a successful deployment of a testnet, Ethereum mainnet was launched on ....

Tokens were the first class of smart contracts to gain mass adoption. They were a new take on the old idea of alt-coins and colored coins. Tokens were launched in days or weeks reducing development efforts by many orders of magnitude and relied purely on Ethereum for its security. 

[^1]: The first blockchain to launch off a forked version of Bitcoin was Litecoin ... namecoin. Alt-coins had to bootstrap the network
[^2]: Bitcoin script
