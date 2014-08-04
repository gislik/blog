---
title: Bitcoin Wallets
---

Wallets are comparable to web browsers in that they are the main interface for end users to interact with the network. The reference wallet implementation, Bitcoin-Qt, is as previously discussed at the same time a full participant which means it can take over 24 hours at the current data volume to sync with the rest of the network and the copy of every transaction from the inception of Bitcoin consumes between 20 and 30 GB of hard disks (depending on the file system used). The next step in the evolution of wallets was a simple payments verification wallet (SPV). While sacrificing a bit of anonymity and security by depending on peers for validation of blocks the minimum wallet offers significant advantages to normal users by allowing the wallet to sync to the Bitcoin network nearly instantly and with reduced memory requirements allows wallets to be run on memory constraint devices like as mobile phones.

Soon after people started using the wallets a number of issues were identified which limited their usage and security:

- If a mobile wallet containing the only copy of a private key got lost or damaged all the bitcoins associated with that wallet would also be lost.

- Access to your bitcoins is limited to what you have in your mobile wallet since you don't want to keep all your bitcoins in the same wallet (see previous reason). This can however be seen as an advantage in case of a robbery. It also directly translates to real world usage of bills and coins. I will however argue that we should be aiming for an better solution.

To combat this .... web wallets...

The Bitcoin Protocol is evolving and the process is controlled by the Bitcoin Improvement Proposals (BIPS). One of the more interesting features to be accepted is the [M-of-N Standard Transactions](https://github.com/bitcoin/bips/blob/master/bip-0011.mediawiki) which sparked the creation of wallets secured by "wallet protection services". These new wallets are essentially hybrids between

