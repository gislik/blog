---
title: Receiving bitcoins
---

###### Testnet

Some of you might be aware of the "other" Bitcoin blockchain - *testnet3*. This blockchain is mined in parallel to the main Bitcoin blockchain - *mainnet* - and since coins on *testnet3* are entirely worthless we can develop safely without our mistakes being too costly.

> Testnet coins are worthless - but useful. They are useful because they are worthless.

To receive *testnet* coins we can either mine them or withdraw some from a faucet. Although the difficulty of mining on testnet is orders of magnitudes lower than on mainnet it still takes too long for our needs. A faucet is simply a website run by a bitcoin volunteer, who has mined some coins, where we can ask to be sent some coins to our wallets.

<img style="float: right" src="/img/testnet-splashscreen.png" width="400" />

Maybe not so surprisingly Xenog, one of the developers of Haskoin, is such a volunteer and using his faucet we can acquire some coins. This also gives us an oppertunity to experiment with the official Bitcoin client before we try our own luck. I requested 0.3 coins which I [received](http://tbtc.blockr.io/tx/info/3c14a1c339d83c949c9d0c17e47aca03f99f9b48147ec246b7232a2924a2a427) moments later to my wallet.

> Currently mainnet addresses begin with either `1` or `3` (although this might change in the future) and testnet addresses usually begin with `m` or `n`.

To access testnet we launch bitcoind or Bitcoin-Qt with `-testnet` as a command line parameter and a green splash screen is presented (as opposed to the normal orange) to remind us that we're on the "other" blockchain. If you want to make this a permanent change add `testnet=1` to your `bitcoin.conf` file.

Since there are not that many peers running constantly on the testnet peer discovery can be somewhat tricky. A standard trick is to use `-addnode` to seed our peeer connectivity a known peer which can notify us about other peers. Haskoin runs both an official mainnet and testnet nodes on ports 8333 and 18333, respectively. 

    $ ./Bitcoin-Qt -testnet -addnode=haskoin.com


###### keys, addresses and transactions

Bitcoin addresses are used to receive bitcoins but it is important to understand that the coins (a.k.a. satoshis) are neither stored in that address nor are they stored in the recipient's wallet. When Alice wants to send Bob satoshis she or her wallet on her behalf will construct a transaction and broadcast it to the peer network. If the transaction is valid it will included in the blockchain. 

Alice's transaction will include:

* a reference a prior transaction
* a value
* a destination address 
* a proof that she controls the funds being spent

<!-- The value is the amount of satoshis Alice wants to send to Bob and the destination address in this case is one of Bob's addresses. As a proof Alice signs the transaction with her private key which allows the peers to verify that she controls the address used as a destination address in the prior transaction. -->

The value is the amount of satoshis Alice wants to send to Bob and the destination address in this case is one of Bob's addresses. As a proof Alice signs the transaction with the private key (matching the address used as destination in the prior transaction) which allows the peers to verify that she controls the funds being spent.

> Seen in this light the blockchain is simply a journal of transactions &mdash; crediting a prior transaction and debeting an address &mdash; which allows satoshis to be spent by adding a new transaction to the journal and providing a signature proving the ownership of the referred address.


###### Base58Check and Wallet Import Format (WIF)

The private keys are 256 bit binary blogs which can be hard to move reliably between devices. Bitcoin uses a modified version of Base 58, called Base58Check, to encode both private keys and addresses in ASCII.  
The resulting Base58Check-encoded private key is said to be in Wallet Import Format (WIF).

The relationship between the private key, public key and the bitcoin address can be represented by:

TODO: Diagram

WIF private key <&mdash;> (base 58 check) <&mdash;> 256 bit private key &mdash;> (Elliptic Curve DSA) &mdash;> 512 bit public key with prefix &mdash;> (SHA&mdash;256 + RIPEM 160) &mdash;> 160 bit public key hash <&mdash;> (base 58 check) &mdash;> address


###### Simplest wallet possible

For the rest of the post our aim is to use Haskell to receive satoshis in as simple manner as possible. All we need to do is to generate a private key and derive from it an address. Using the address we can receive the coins and as long as we keep the key safe we can spend it later using any wallet which supports importing WIF keys.

    randomKey :: IO PrvKey
    randomKey = withSource devRandom genPrvKey

    bitcoinAddress :: PrvKey -> String
    bitcoinAddress = addrToBase58 . pubKeyAddr . derivePubKey

    main :: IO ()
    main = do
       arg <- listToMaybe <$> getArgs
       k   <- maybe randomKey return (fromWIF =<< arg)
       putStrLn $ "Key: " <> toWIF k
       putStrLn $ "Address: " <> bitcoinAddress k

TODO: Code markup

Without any arguments this program will randomly generate a private key and print it (in WIF) and its derived address. It is also possible to pass the private key as an argument in which case the address will be derived from that key.

    $ cabal run 
    Key: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    Address: n2qkBLCiU1tgpCeL46tqny4SMDQAGXwGNZ

TODO: Sending coins in Bitcoin-Qt

TODO: Printing transaction

TODO: Importing to Bitcoin-Qt
