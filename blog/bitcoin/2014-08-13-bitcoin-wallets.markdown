---
title: Bitcoin Wallets 
---

In this blog post I'm going to concentrate on [Bitcoin wallets](https://bitcoin.org/en/choose-your-wallet) which will be our foundation for a more developer-oriented discussion. Wallets are analogous to web browsers in that they are the main interface for end users to interact with the network. In its simplest form it is a file which contains a collection of [private keys](https://en.bitcoin.it/wiki/Private_key). In addition the wallet software usually takes care of communicating with peers, managing the blockchain and keeping records of wallet transactions.

<div style="float: right;"> ![Screenshot of Bitcoin-Qt](/img/bitcoin-qt-send.png)</div>

The reference wallet implementation, [`bitcoind`](https://github.com/bitcoin/bitcoin), is as [previously discussed](http://gisli.hamstur.is/2014/08/bitcoin-from-a-developers-perspective/) at the same time a full node which means it can take several days to sync with the rest of the network and the copy of every transaction from the inception of Bitcoin till present time consumes between 20 and 30 GB of hard disks. 

The blockchain management is both impractical and/or impossible - for example in the case of mobile wallets. Without mobile wallets Bitcoin's adoption is severely diminished. This has lead to two different proposals which are quite different:

<div style="clear: both"></div>

- [Simple Payment Verification](https://en.bitcoin.it/wiki/Thin_Client_Security#Simplified_Payment_Verification_.28SPV.29) (SPV) aims to reduce the amount of blockchain data needed to verify transactions. This is mostly achieved by storing block headers only for transactions which are not managed by the wallet and reduces the data to roughly 8 MB. This seems to be the [preferred methodology of the Bitcoin core developers](https://bitcointalk.org/index.php?topic=88974.msg986297#msg986297) but there is still a significant overhead in managing the peer to peer connections. [Multibit](https://multibit.org/) is probably the most popular SPV wallet implementation.

- [Stratum wire protocol](https://docs.google.com/document/d/17zHy1SUlhgtCMbypO8cHgpWH73V5iUQKk_0rWvMqSNs/edit?hl=en_US) which outsources the management to a node containing the full blockchain - a stratum server - and allows the wallet to communicate with the node using [JSON-RPC](http://www.jsonrpc.org/) over multiple transport layers. Historically the protocol was used by the [Electrum](https://electrum.org/) lightweight wallet only and it's just recently that the protocol is being [proposed formally](https://github.com/bitcoin/bips). This method sacrifices a bit of anonymity and security for convenience. However client implementations can connect to multiple stratum servers and only accept blockchain data if the majority of the stratum servers agree.

There are other types to consider like web wallets which usually are full node wallets implemented as a service. This implies a complete trust between the wallet owner and the service provider. Another interesting type of wallets are "wallet protection services" that are springing to life after the [M-of-N Standard Transactions](https://github.com/bitcoin/bips/blob/master/bip-0011.mediawiki) proposal was accepted.

In my future blog posts I'll be exploring different implementations but first things first; next up bootstrapping our development environment.
