---
title: Bitcoin Wallets 
---

In this blog post I'm going to concentrate on Bitcoin wallets from a developer's perspective. Wallets are analogous to web browsers in that they are the main interface for end users to interact with the network. In its simplest form it is a file which contains a collection of private keys. In addition the wallet software usually takes care of communicating with peers, managing the blockchain and keeping records of wallet transactions.

The reference wallet implementation, `bitcoind`, is as previously discussed at the same time a full node which means it can take several at the current data volume to sync with the rest of the network and the copy of every transaction from the inception of Bitcoin consumes between 20 and 30 GB of hard disks (depending on the file system used). 

The blockchain management is both impractical and in some cases impossible for example in the case of mobile wallets. Without mobile wallets Bitcoin's adoption is severely diminished. This has lead to two different Bitcoin Improvement Proposals (BIPS). The proposals are quite different:

- Simple Payment Verification (SPV) aims to reduce the amount of blockchain data needed to verify transactions. This is mostly achieved by storing block headers only (as opposed to the full blockchain data) for transactions which are not managed by the wallet and reduces the data to roughly 8 MB. This seems to be the preferred methodology of the Bitcoin core developers but there is still a significant overhead in managing the peer to peer connections. Multibit is probably the most popular SPV wallet implementation.

- Stratum wire protocol which outsources the management to a node containing the full blockchain (stratum server) and allows the wallet to communicate with the node using JSON-RPC over multiple transport layers. Historically the protocol was used by the Electrum lightweight wallet only and it's just recently that the protocol is being proposed formally. This method scarifies a bit of anonymity and security for convenience. However client implementations can connect to multiple stratum servers and only accept blockchain data if the majority of the stratum servers agree.

There are other types to consider like web wallets which usually are full node wallets implemented as a service. This implies a complete trust between the wallet owner and the service provider. Another interesting type of wallets are "wallet protection services" that are springing to life after the [M-of-N Standard Transactions](https://github.com/bitcoin/bips/blob/master/bip-0011.mediawiki) proposal was accepted.

In my future blog posts I'll be exploring different implementations but first things first; next up bootstrapping our development environment.
