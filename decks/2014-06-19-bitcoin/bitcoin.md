---
title: Bitcoin
summary: |
  Bitcoin was the initial implementation of a blockchain protocol and although other protocols have emerged Bitcoin serves well as a foundational layer for understanding how these protocols work. I created this deck to use as talking points when ELI5 :) This is an interactive deck. Use the arrow buttons to go down and then to the right.
reveal: true
theme: league
featureimage: 2014/06/bitcoin.png
---
<section>
<section>
# Bitcoin 

Gísli Kristjánsson

19 October 2014
</section>

<section>
## What is bitcoin?

Bitcoin is a client/server platform, defined by a protocol, which implements a secure, distributed ledger.
</section>

<section>
## Why does Bitcoin have value?

Its value is derived from its scarcity and utility.
</section>
</section>

<section>
<section>
# Terminology
</section>

<section>
## Capital B or not?

- Bitcoin - protocol, software, and community

- bitcoin units of the currency

- also BTC
</section>

<section>
## Crypto

Techniques for secure communication in the presence of third parties
</section>

<section>
## Key pairs

- Private key is computed

- Public key is derived from the private key

- Form a crypto key pair
</section>

<section>
## Addresses

<br/>

1QHwNS1CCywHPA2nWEZ8XDpZmXySxopc7J

<br/>

- A (more) readable representation of the public key

- Identifier of 27-34 alphanumeric characters

- Represents a possible destination for a Bitcoin payment

- Can be generated at no cost 
</section>

<section>
## Transaction

- Input

- Output

- Broadcast to the network <br>
and collected into blocks
</section>

<section>
## Blocks

A collection of transactions
</section>

<section>
## Blockchain

- Ordered collection of blocks 

- A block points to the previous block <br>
creating a chain of blocks

- Prevents double spending 
with the help of miners
</section>

<section>
## Peers

Miner, nodes and wallets
</section>

<section>
## Miners

### a.k.a. weavers

Try to order blocks in the blockchain by solving math puzzles and are rewarded with new BTC

</section>

<section>
## Nodes

Verify blockchain and communicate with peers
</section>

<section>
## Wallets

- File that contains a collection of private keys

- Often refers to the software managing that file

</section>
</section>

<section>
<section>
# Details
</section>

<section>
## Security

The ledger is secured by having the nodes agree on the information stored in the blockchain.
</section>

<section>
## Consencus

- Agreement among the nodes is achieved by having the miners vote.

- The number of total votes is based on computing power.
</section>

<section>
## Anonymity

- Balance is simply associated with an address and its public-private key pair
</section>

<section>
## Mining

- Coins are slowly mined into existence

- Search for a solution to a very difficult math problem whose difficulty is precisely known

- Difficulty is automatically adjusted

- Block reward

- 13 million bitcoins (July 2014)

- No more than 21 million bitcoins will ever exist

- Happens in 2140
</section>

<section>
## Structure

<img src="Transaction.png" class="stretch" />

<img src="Blockchain.png" class="stretch" />
</section>


<section>
## How to get

- Mining

- Exchanges

- Face to face

- ATM
</section>

<section>
## Economics

While the number of bitcoins in existence will never exceed 21 million, the money supply of bitcoins can exceed 21 million due to Fractional-reserve Banking.
</section>

<section>
## Deflation

- Keynesian economists argue that deflation is bad

- The Austrian school of thought counters this criticism
</section>

<section>
## 51% attack

<img src="pools.png" class="stretch" />
</section>

<section>
## Blockchain fork

- Created from time to time when two blocks are created just a few seconds apart

- More serious forks have occurred after fixing bugs that required backward-incompatible changes
</section>
</section>

<section>
<section>
# Comparision
</section>

<section data-background="#ff4444" data-transition="linear">
## Nay-sayers 1/2


<blockquote class="fragment">[W]hen the Paris Exhibition closes electric light will close with it and no more be heard of.

<small> - Erasmus Wilson (1878) Professor at Oxford University</small></blockquote>

<blockquote class="fragment">This `telephone' has too many shortcomings to be seriously considered as a practical form of communication. The device is inherently of no value to us.

<small> - Western Union internal memo, 1878</small></blockquote>

<blockquote class="fragment">Well informed people know it is impossible to transmit the voice over wires and that were it possible to do so, the thing would be of no practical value.

<small>- Editorial in the Boston Post (1865)</small></blockquote>
</section>

<section data-background="#ff0000" data-transition="linear">
## Nay-sayers 2/2
<blockquote class="fragment">Radio has no future.

<small>- Lord Kelvin (1824-1907), British mathematician and physicist, ca. 1897.</small></blockquote>

<blockquote class="fragment">There is no reason for any individual to have a computer in their home.

<small>- Kenneth Olsen, president and founder of Digital Equipment Corp., 1977.</small></blockquote>
</section>

<section data-background="#8888ff" data-transition="linear">
## Early Internet 1/3

<blockquote class="fragment">Fine, you nerds can do what you want but normal people are never going to use this thing.</blockquote>
<blockquote class="fragment">It's completely decentralized, which means you can't trust it. No business is ever going to do anything on it because businesses won't work on an untrusted environment. There won't ever be any e-commerce.</blockquote>
<blockquote class="fragment">There will never be any internet payments. No one will put their credit card on the internet.</blockquote>
<blockquote class="fragment">It's an open-source kind of thing so there will be no Internet companies.</blockquote>
</section>

<section data-background="#4444ff" data-transition="linear">
## Early Internet 2/3

<blockquote class="fragment">It's got all these technical deficiencies. It's slow. It's unreliable. It doesn't work right. When you do a search, sometimes you get an answer back and sometimes you don't. Sometimes when you dial in you get a busy signal.</blockquote>

<blockquote class="fragment">What happen if your ISP goes out of business? Then you can't get back online.</blockquote> 

<blockquote class="fragment">Once you get on the internet, even assuming you get on the internet, there's nothing to do. There's no content. Time magazine isn't online, the New York Times isn't online. It's just a bunch of nerd stuff.</blockquote>
</section>

<section data-background="#0000ff" data-transition="linear">
## Early Internet 3/3

Basically every single criticism of the Internet ended up getting solved. Every single supposed fatal flaw got eliminated.
</section>

<section>
## HTTP

<center>
<table width="100%">
<tr>
  <th>Protocol</th>
  <td>HTTP</td>
  <td>Bitcoin</td>
</tr>

<tr>
  <th>Server</th>
  <td>Web server</td>
  <td>Node</td>
</tr>

<tr>
  <th>Client</th>
  <td>Browser</td>
  <td>Wallet</td>
</tr>

<tr>
  <th>Prog.lang.</th>
  <td>CGI</td>
  <td>Script</td>
</tr>

<tr>
  <th>Content</th>
  <td>HTML</td>
  <td>BTC</td>
</tr>

<tr>
  <th>Value</th>
  <td>Information</td>
  <td>Money</td>
</tr>

</table>
</center>
</section>


<section>
## Why now? 1/2

* The internet itself in the form of the ARPANET had been in existence for more than 20 years. 
  
* The ideas had been building. 
  
* They just hadn't been put together as a platform.
</section>

<section>
## Why now? 2/2

* Bitcoin is very sharply reminiscent of this. 
  
* It's not an overnight thing. 
  
* It's a result of 35 years of cryptography research, going back to the early 1970s with RSA. 
  
* Digital cash is something people have been working on for 15 years leading into the Bitcoin paper. 
  
* So it's this long-term development of a very powerful set of ideas that people have been working on for a long time.
</section>
</section>

<section>
<section>
# Possibilities
</section>

<section>
## Online payments for the rest of the world
</section>

<section>
## Remittances

- Expats sending money to their home countries

- $800 billion annually

- Average remittance fee 8.14% per $200 = $16.28

- Bitcoin transaction fee $0.004

- Bitcoin is 4000x cheaper on $200 transaction
</section>

<section>
## BIPS

Bitcoin Improvement Proposals

- Multi-Sig Transactions

- M-of-N Standard Transactions

- Pay To Script Hash

- Hierachical Deterministic Wallets (HD)

- Payment Protocol
</section>

<section>
## Crowd funding

### a.k.a. insurance contracts

Individuals pledge money to a project that is taken from them only if enough pledges are received to meet the target.

- Kickstarter

- Web page translation
</section>

<section>
## Smart property

- Car keys

- Key cards
</section>

<section>
## Micro payments

- Listening to Internet radio paid by the second
- Viewing web pages with a small tip for each ad not shown
- Buying bandwidth from a WiFi hotspot by the kilobyte
</section>

<section>
## Multi-signature accounts

Certain number of a defined group of persons agree to sign the transaction
</section>

<section>
## Dispute mediation

A 3-party can approve or reject a transaction in case of disagreement between the other parties without having control on their money
</section>

<!--
## Oracles
-->

<section>
## Financial instruments

- Trustless collaterized lending

- Bonds

- Funds with policies

- Exchanges (Ripple)
</section>

<section>
## Platform

* Like the internet, Bitcoin is a platform. 

* It's less what can it do today, more what can it do in the future. 
  
* There are thousands and thousands of things that it might make possible in the future. 
  
* Digital cash, digital keys, digital voting, digital stocks, digital bonds.
</section>
</section>

<section>
<section>
# Bitcoin today
</section>

<section>
## Trading

<img src="daily-value-of-transactions.jpg" style="float: right; width: 400px">

<div style="float: left; width: 500px">
- BTC/USD $600

- Market cap $7-8 billion

- 24 hour trading volume $8-9 million

</div>
</section>

<section>
## Volatility

<img src="volatility.png" class="stretch" />
</section>

<section>
## Key Metrics

<img src="bitcoin-today-metrics.png" class="stretch" />
</section>

<section>
## Price feeds

<img src="bitcoin-today-feeds.png" class="stretch" />
</section>

<section>
## Merchants

<img title="Approx. 63,000 merchants" src="bitcoin-today-merchants.png" class="stretch" />
</section>

<section>
## ATMs

<img title="Over 100 ATMs" src="bitcoin-today-atm.png" class="stretch" />
</section>
</section>

<section>
<section>
# Demo
</section>

<section>
## Early Internet
<img src="first-websites.jpg" class="stretch" />
</section>
</section>

<section>
# Questions?
</section>
