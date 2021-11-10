---
title: A super gentle introduction to zero-knowledge proofs 🍄
tags: zkp
image: /img/2021/mushroom.png
summary: |
  Once considered unrealistic moon math, zero-knowledge proofs, may hold the answer to some of the most pressing blockchain questions, like how will they scale and what about privacy? At the same time the zero-knowledge proofs are extremely ill-understood and inaccessible to most people. The aim is to develop an intuition for them, explain in simple terms how they can be used in the context of blockchains, and get you excited about their seemingly endless utility.
---

For close to four decades zero-knowledge proofs have existed in a theoretical vacuum but recent advancements in the field have made their application practical. A zero-knowledge proof is a mathematical proof with a few magical properties which can be used to perform *provable* private computation. For this reason it is plausible that zero-knowledge proofs are the key to [The Holy Trinity](/2021/01/the-holy-trinity/) of blockchains; privacy, scalability and interoperability. Before we can understand how zero-knowledge proofs can be used to solve all these problems we need to understand what they are fundamentally. Simplified, they are a tool which a prover can use to convince a verifier that a statement is true while not revealing *how* it is true. 

> “I am allowed to buy alcohol”

To prove this statement you can show the verifier your government issued ID, containing your birthday, and the verifier then verifies whether you are 20 years or older[^0]. That is how the statement is true. Similarly using a zero-knowledge proof the verifier can verify the statement, this time however without knowing how old you are or any other information such as your name or gender.

To gain an intuition how such a magical proof could be created let's imagine that you have broken a hash function, \( f(x) \), by finding its inverse function[^1], \( f^{-1}(y) \). To prove to someone that you have actually broken the hash function you could either reveal the inverse function for anyone to verify or you could keep it a secret and instead ask the verifier to give you a random number, \( r \). You would then calculate its pre-image \( s = f^{-1}(r) \) which the verifier is then able to verify that \( f(s) = r \). Doing this interaction with a single random number may not be able to fully convince the verifier because you may have cheated by hashing a range of numbers and with some likelihood the verifier will have chosen a random number which matches one of those hashes, without you knowing the inverse function. By repeating the process however, with random numbers \( r_0, r_1, \dots, r_n \), the verifier can be convinced with a arbitrary high degree of confidence that you have actually broken the hash function. Now we have the tools to convince a single verifier but a second verifier watching this interactive game play out may not be convinced because you and the original verifier may be colluding by using predetermined numbers \( r_0, r_1, \dots, r_n \). Fortunately, it is possible to convert the interactive proof into a non-interactive proof by hashing certain publicly available data and use it instead of the random numbers from the verifier[^2]. This is a pre-requisite for blockchains as we need to be able to prove once and multiple nodes need to be able to verify based only on the data in the blockchain.

With this background let's see how we can use zero-knowledge proofs to solve the three issues which are hindering mainstream adoption of blockchains.

# Scalability 🧑‍🚀

Blockchains are slow by design because every node in the network has to validate all blocks and transactions. This becomes even more of an issue when the blockchain supports, in addition to the predetermined consensus logic, arbitrary computation where all the nodes must perform the computation to be able to determine the validity of the transactions. To understand the implications, let us put this in another context. Imagine that the task is to sum all the integers between zero and a hundred trillion. To expedite the process, traditional systems will divide the range into, for example, ten smaller ranges and distribute them to ten other computers to be summed in parallel. The final sum is calculated by summing all the subtotals. In contrast, a blockchain will send the full range, from zero to a hundred trillion, and have all the ten computers calculate the whole sum simultaneously. This is naturally a much slower method to do the same calculation but it is much more resilient to calculation errors. For blockchains resiliency is more important than efficiency. 

> “I have applied the following transactions to the current state and the resulting state is valid according to the consensus rules”

Using zero-knowledge proofs in the blockchain model we can compress computation into a single step to get the best of both worlds, a resilient and efficient computation network. The miner, as the prover, can perform the computation needed to validate a block once and include a succinct validity proof with the resulting state. Subsequently, other nodes simply verify the constant-size proof and apply the state without needing to perform the computation themselves. With such a capability light clients, which can run inside a browsers, on mobile phones with limited computing resources, and inside a smart contact, could simply download the state, or the relevant parts of it, and start using it after verifying a simple zero knowledge proof.

# Privacy 🛩

In their current form blockchains expose the sender address, the receiver address, the token and the amount in clear-text so other nodes can validate that the consensus rules are upheld. Using zero-knowledge proofs we can design cryptographic systems which also preserve the privacy of a blockchain. Homomorphic encryption makes arithmetic over encrypted values possible and can be used to hide the amount. Nevertheless, it is critical that transactions with hidden amounts must not change the total supply of the token. For UTXO-based blockchains, this would mean that all the inputs of a transaction must be equal to its outputs. Without knowing anything about the values in the transaction it becomes possible to create new coins whilst upholding this equality by going into negative numbers. Let's say that you have two coins in your digital wallet which sum is 10. By exchanging those for two other coins where one has a negative value you now have more than 10.

\$$ 4x + 6x = 10x = 10y = -20y + 30y \$$

The inputs, \( x \), and outputs, \( y \), are equal but you now have a coin in your wallet which value is 30. Using zero-knowledge proofs you can prove that all the hidden amounts are non-negative and therefore no extra coins have been created by the transaction. Other techniques can be used to hide the addresses and which token is being transacted with to get full privacy. 

# Interoperability 📡

For one blockchain to be able to talk to another it must be able to understand what is happening on that chain, i.e. understand its consensus rules. For example, one way to perform a trustless auction of an NFT on Ethereum for bitcoin on the Bitcoin network, is to place the NFT[^3] into an escrow contract which automatically releases the NFT to the buyer's address on Ethereum upon receiving a proof that a minimum amount of bitcoins have been sent to an address, belonging to the seller, on the Bitcoin network. Ethereum is expressive enough and Bitcoin's consensus rules are simple enough to run a Bitcoin SPV client directly on the EVM[^4]. However communicating in the other direction is impossible because of the limited functionality of the Bitcoin network, which would need to embed the EVM to validate Ethereum transactions, which is unlikely to happen. Furthermore, new sets of primitives, and possibly new virtual machines, would be required for each interoperating blockchain. Alternatively, zero-knowledge proofs can be used as an interoperability construct between arbitrary blockchains. Instead of requiring the target chain to understand the consensus rules of the source chain, it would simply need to be able to validate a short zero-knowledge proof. That is scalable and much likelier to happen. 

<h2><center>Take away</center></h2>

Zero-knowledge proofs, once considered unrealistic moon math[^5], are one of the most exciting fields in cryptography today and their potential in the context of blockchains is enormous. Although it is still early days, this technology is fast becoming a crucial component of the $2.5 trillion blockchain ecosystem. At the very least, we now have a credible answer to crypto's most pressing issues.

[^0]: Yes, that's the legal age to buy alcoholic beverages in Iceland 🍻

[^1]: A key property of a hash function is that its inverse function, which may exists but must not be known.

[^2]: The [Fiat–Shamir heuristic](https://en.wikipedia.org/wiki/Fiat%E2%80%93Shamir_heuristic) is a technique for taking an interactive proof of knowledge and creating a digital signature based on it. More generally, the Fiat–Shamir heuristic may also be viewed as converting a public-coin interactive proof of knowledge into a non-interactive proof of knowledge. 

[^3]: Non-Fungible Tokens (NFT) are a method to represent ownership of digital objects.

[^4]: The world's first [cross-chain auction](https://medium.com/summa-technology/cross-chain-auction-technical-f16710bfe69f) was conducted on November 15, 2018 using this exact method.

[^5]: Zero knowledge proofs are quite complex and until recently they were often referred to as “moon math”.
