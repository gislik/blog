---
title: Bitcoin from a developer's perspective
author: Gísli Kristjánsson
---

Bitcoin is an exciting new technology invented by the mysterious Satoshi in 2009 which has been gaining some serious traction in the last few months. Although there are many introductory blog posts about Bitcoin there aren't necessarily many which tackle the subject from a developers perspective.

I am a bit of a late-comer to the Bitcoin scene only to have started to pickup interest in late 2013 and then purely from a functional perspective which means I didn't care so much about Bitcoin as a store-of-value but more as a transfer-of-value kind of mechanism. Since then I have been absorbing every piece of Bitcoin information I can get my hands on. Slowly I have started to realize that its true potential is far beyond the promise of being able to send money securely and cheaply from A to B. If this potential materializes I'm guessing that the monetary value of bitcoins will increase by a factor of 10-50 over the coming decade (from its $600 value).

To further explore the possibilities of Bitcoin I'm going to be experimenting with it by writing some Haskell code and bloging about the experience. My choice of using Haskell will of course limit the number of readers but I hope to advance the Bitcoin ecosystem for Haskell a bit along the way. I would like to set the scene by defining some basic terminology and recap Bitcoin's history in a paragraph or two.

Bitcoin is a distributed peer-to-peer database which collects transactions into an ordered chain of blocks, called the blockchain. Transactions are signed and verified using public key cryptography. I am going to assume basic knowledge of public key cryptography and only mention that the creators of Bitcoin chose elliptic curve algorithms as the basis for deriving the public key from the private key instead of its more common counterpart, DSA/RSA.

In general there are three different kinds of peers running the Bitcoin network; miners, nodes and wallets (SPV). To complicate matters peers can be a linear combination of those peer types, e.g. the original Bitcoin-Qt client is a full node and a wallet which can be run as a miner as well.

Since the database is distributed where miners and nodes keep a full copy of the database there must be a way to agree on the contents of the database. In Bitcoin this is achieved by letting the miners vote and the number of votes is proportional to the number of CPU cycles available to the miner. This means that the database can become temporarily inconsistent but as long as no single party controls the majority of the CPU power in the network the database will safely converge to a unified version.

<!--more-->

Nodes receive blocks and transactions from peers. Once proven cryptographically valid the node stores and relays the block or the transaction to other miners, blocks and wallets. The purpose is to improve the time taken to distribute data within the network, which usually happens within seconds. 

Wallets are analogues to web browsers in that they are the main interface for end users to interact with the network. The reference wallet implementation, Bitcoin-Qt, is as previously discussed at the same time a full participant which means it can take over 24 hours at the current data volume to sync with the rest of the network and the copy of every transaction from the inception of Bitcoin consumes between 20 and 30 GB of hard disks (depending on the filesystem used). The next step in the evolution of wallets was a simple payments verification wallet (SPV). While sacrificing a bit of anonymity and security by depending on peers for validation of blocks the minimum wallet offers significant advantages to normal users by allowing the wallet to sync to the Bitcoin network nearly instantly and with reduced memory requirements allows wallets to be run on memory constraint devices like as mobile phones.

Soon after people started using the wallets a number of issues were identified which limited their usage and security:

- If a mobile wallet containing the only copy of a private key got lost or damaged all the bitcoins associated with that wallet would also be lost.

- Access to your bitcoins is limited to what you have in your mobile wallet since you don't want to keep all your bitcoins in the same wallet (see previous reason). This can however be seen as an advantage in case of a robbery. It also directly translates to real world usage of bills and coins. I will however argue that we should be aiming for an better solution.

To combat this .... web wallets...

The Bitcoin Protocol is evolving and the process is controlled by the Bitcoin Improvment Proposals (BIPS). One of the more interesting features to be accepted is the [M-of-N Standard Transactions](https://github.com/bitcoin/bips/blob/master/bip-0011.mediawiki) which sparked the creation of wallets secured by "wallet protection services". These new wallets are essentially hybrids between
