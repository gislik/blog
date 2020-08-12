---
title: Bitcoin from a developer's perspective
---

[Bitcoin](https://bitcoin.org/en/) is an exciting new technology invented by the mysterious [Satoshi](http://en.wikipedia.org/wiki/Satoshi_Nakamoto) in 2009 which has been gaining some serious traction in the last few months. Although there are many introductory blog posts about Bitcoin there aren't necessarily many which tackle the subject from a developer's perspective.

I am a bit of a late-comer to the Bitcoin scene only to have started to pickup interest in late 2013 and then purely from a functional perspective which means I didn't care so much about Bitcoin as a store-of-value but more as a transfer-of-value kind of mechanism. Since then I have been absorbing every piece of Bitcoin information I can get my hands on. Slowly I have started to realize that its true potential is far beyond the promise of being able to send money securely and cheaply from A to B. If this potential materializes I'm guessing that the monetary value of bitcoins will increase by a factor of 10 - 50 over the coming decade (from its current $600 value).

To further explore the possibilities of Bitcoin I'm going to be experimenting with it by writing some [Haskell](http://www.haskell.org/haskellwiki/Haskell) code and blogging about the experience. My choice of using Haskell will of course limit the number of readers but I hope to advance the Bitcoin ecosystem for Haskell a bit along the way. I would like to set the scene by defining some basic terminology in a paragraph or two.

Bitcoin is a distributed peer-to-peer database which collects [transactions](https://en.bitcoin.it/wiki/Transaction) into an ordered chain of blocks, called the [blockchain](https://en.bitcoin.it/wiki/Blockchain). Transactions are signed and verified using public key cryptography. I am going to assume basic knowledge of public key cryptography and only mention that the creators of Bitcoin chose [elliptic curve algorithms](http://en.wikipedia.org/wiki/Elliptic_curve_cryptography) as the basis for deriving the public key from the private key instead of its more common counterpart, [DSA](http://en.wikipedia.org/wiki/Digital_Signature_Algorithm)/[RSA](http://en.wikipedia.org/wiki/RSA_(cryptosystem)).

In general there are three different kinds of peers running the Bitcoin network; [miners](https://en.bitcoin.it/wiki/Mining), [nodes](https://en.bitcoin.it/wiki/Node) and [wallets](https://en.bitcoin.it/wiki/Wallet). To complicate matters peers can be a linear combination of those peer types, e.g. the original [`bitcoind`](https://en.bitcoin.it/wiki/Bitcoind) client is a full node and a wallet which can be run as a miner as well.

Since the database is distributed where miners and nodes keep a full copy of the database there must be a way to agree on the contents of the database. In Bitcoin this is achieved by letting the miners vote (a.k.a. [proof of work](https://en.bitcoin.it/wiki/Proof_of_work)) and the number of votes is proportional to the number of CPU cycles available to the miner. This means that the database can become temporarily inconsistent but as long as no single party controls the majority of the CPU power in the network the database will safely converge to a unified version.

Nodes receive blocks and transactions from peers. Once proven cryptographically valid the node stores and relays the block or the transaction to its peers. The purpose is to improve the time taken to distribute data within the network, which usually happens within seconds. 

In [part 2](/2014/08/bitcoin-wallets/) I will be taking a closer look at wallets, their purpose and different implementations.
