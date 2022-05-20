---
title: The Holy Trinity
image: /img/2021/venn-diagram.png
summary: |
  Blockchains solve the problem of double-spending and thereby allows financial ledgers to be distributed among peers who maintain the invariants collectively without resorting to entrusting a centralized entity with the ledger. At the same time technical limitations still remain, hindering greater adoption of the technology. We identify the biggest unsolved challenges and what is being done to crack the nut. 🥜
---

Not all ledgers are born equal. Ledgers recording financial value must uphold certain invariants for them to represent ownership in a coherent manner. An important invariant is e.g. that for every account debited an equal amount must be credited to another account or else we risk that either some value goes unaccounted for or that value is overstated. 

The main innovation of blockchains[^1] is enabling non-trusting network of participants to come to a consensus on the state of a ledger without relying on a centralized entity responsible for upholding the agreed upon rules for ledger updates. This innovation has been used to enable digital contracts which are enforced by the same mechanism. 

> On the blockchain nobody knows you’re a fridge 👀

This is a remarkable achievement because it allows strangers to enter into contracts across geographic borders where enforcing a contract is either too expensive or is simply impossible due to unclear legal jurisdiction. While most nations are members of the World Trade Organization (WTO) the dispute resolution process only deals with disputes between nation states and not a private contract that you and I enter into. Contracts on a blockchain are many orders of magnitude cheaper, involve less human coordination, and result in deterministic outcomes. 

While blockchains offer an unprecedented common ground to build the future of financial services on, they are not perfect in their current incarnation. They are slow, hard to use, expensive (still) and offer no transaction privacy. In the current form, blockchains cannot serve the 4+ billion people roaming the digital world. Fortunately they provide the correct constructs to build layers on top of which fix the aforementioned shortcomings. 

[^1]: Open, public and permissionless blockchains.

# 1️⃣ &nbsp;Scalability

The Bitcoin network and other first-generation blockchains can handle tens of transactions per second which is a far cry from Visa's self-reported tens of thousands of transactions per second[^6]. While comparing the two networks is comparing apples and oranges[^2] it is obvious that we are still lacking at least two orders of magnitude in transaction throughput.

A second layer is being built on top of the slower but higher valued rails beneath. Multiple scaling proposals are being worked on simultaneously which include side-chains, payment/state channels, rollups, and faster blockchains[^3]. The proposals have different trade-offs but in general they seek to move traffic off the first layer and only transact there periodically and in aggregate. All transactions conducted on the second layer are secured by the underlying blockchain in the sense that if a dispute arises they can be intermediated by the blockchain. Since the results are deterministic, the outcome is known beforehand and therefore the only reason why transactions need to be enforced by the blockchain are because of negligence or malfeasance of either party. Such edge cases can be mitigated by game theory and several experiments[^4] are being carried out to figure out the most optimal solution.

The layering of blockchains resembles the layering of the current payment systems in a *single currency* where payments are settled in central bank reserves. A subtle but important difference is that most blockchains benefit from being able to track multiple tokens, represent other assets, which enables atomic swaps[^5] between them. In addition, payments and swaps are permission-less and censorship-resistant both from the execution perspective and from the asset perspective.

[^6]: In a [factsheet](https://usa.visa.com/dam/VCOM/download/corporate/media/visanet-technology/aboutvisafactsheet.pdf) published by VISA they claim to achieve 65,000 transactions per second.

[^2]: The Visa network only seeks to authorize a transaction, deferring the actual settlement for later. In contrast, Layer 1 blockchains authorize and settle at the very same time.

[^3]: While not built as a second layer, most achieve more throughput by sacrificing some decentralization. 

[^4]: A prevailing idea is to require peers participating in the consensus to put up a bond which can be “slashed” when misbehaving.

[^5]: Atomic swaps are delivery of one asset vs. a payment of another (delivery-vs-payment), executed peer-to-peer and without a central clearing house

# 2️⃣ &nbsp;Privacy

Full nodes are an important agent in blockchain networks. Their role is to validate that transactions abide by the network rules. The way most blockchains accomplish that is to broadcast all the transactions through a peer-to-peer network and mandate that the full nodes transform the state from the genesis state to the current state by applying each transaction in order, one by one. The side-effect is that the full transaction history, which includes payers, payees and amounts, is visible to everybody running a full node. 

In some cases full transparency is a major feature, like when public funds or donations are being allocated to stakeholders, in most business settings full transparency is not acceptable. For personal usage it may even be dangerous as wealthy individuals may become high-value targets for nefarious actors. Fortunately blockchains are pseudo-anonymous since payer and payee identifiers are not associable to individuals. Earliest attempts at solving the privacy issue included mixers, where all transactions over a certain time period are aggregated in such a way that payments are divided into multiple smaller payments, combined again and re-routed where direct linkage between the payer and the payee has been severed. 

Some projects deploy trusted execution environments (TEEs) which is a specialized hardware, with segregated CPU and memory, which guarantees that data contained in the TEE, like private a private key, is completely isolated from the outside. Encrypted data can there for be computed on, by sending it to the TEE, where it gets decrypted, computed on, and re-encrypted before it is returned again. A network of TEEs operated by untrusting peers can provide a censorship-resistant and private computing, if participants are willing to trust the centralized manufacturer of the TEE.

Another potential solution leverages zero-knowledge proofs (ZKP) to prove to validators that the current state is a result of valid transformations from the genesis block, without revealing transaction details. The theory behind zero-knowledge proofs stems from the 1980s but no practical implementations existed which rendered the technology unusable for real-world applications. A recent break-through offers hope for ZKPs and it has spurred more research which is yielding very positive results. 

Finally, homomorphic encryption, an advanced form of encryption, allows an algorithm to compute over encrypted values. As an example two encrypted elements added together would be equal to the encrypted value of their unencrypted sum. The addition would not understand the values but the encryption structure would maintain the invariant. While this technology is only practical for very simple operations and elements it would provide the benefits of TEEs without the trust assumption. A user would simply encrypt all inputs with a public key before sending them to be computed on. The result would still be encrypted and only be decryptable with the matching private key. 

# 3️⃣ &nbsp;Interoperability

We live in a multi-blockchain world. This is a feature not a bug. Endless innovation, new proposals for how things might be improved. Different assumptions, different parameters, different consensus mechanisms, different game theory. Most efforts will not yield anything new, others may fail flat out or never see the day of light. If we end up with a few networks, optimized for different use-cases, the question is how do they interoperate. 

There are two approaches being taken. One the one hand there are protocols for swapping an asset on one chain atomically for another asset on a different chain. This means that both users hold accounts on both chains and the assets move in tangent on their respective chains. On the other hand it is possible to represent an asset on a different chain by locking it on its native chain and minting a synthetic asset on the host chain. Synthetic assets can now be exchanged atomically on the host chain and then moved back by burning the synthetic asset and release it back to the new owner on the native chain. Message can be passed between networks to update state across network boundaries but the challenge remains of maintaining the atomicity, whereby either all updates succeed or none at all.
\
\
\
The three topics may seem unrelated but upon closer examination it turns out that they are highly related. If different networks, either layer ones, or layer twos, become interconnected and interoperable it may solve the scaling problem in one go. Usage of zero-knowledge proofs for privacy purposes is possibly also a solution to both interoperability between chains and the remedy for scaling them. Scalability, privacy and interoperability are tough nuts to crack but the open nature of blockchains invites anyone to propose solutions and reap the benefits if they work it out. I know there are a lot of sceptics out there but if history rhymes[^7] betting against the whole world 🌍 is a bet I would not take on. The internet and the open-source movement have shown that a large group of people who share the same ideals are a powerful force for innovation and problem solving.

[^7]: "History does not repeat itself, but it thymes" - Mark Twain.
