---
title: Understanding Ethereum by studying the source code
tags: ethereum
withtoc: true
---

# From papers to code

The [Ethereum Whitepaper](https://ethereum.org/en/whitepaper/) was published in 2013 and a year later some of the implementation details were expanded on in the [Ethereum Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf). Originally there were three implementations of the Ethereum protocol written in Go, C++, and Python[^1]. To understand the core concepts I decided to read both of the papers (again), clone the official [Go implementation](https://github.com/ethereum/go-ethereum) `commit 017449971 (tag: v1.9.9)`, and study the relationship between the ideas and the actual implementation. The [Ethereum Wiki](https://eth.wiki/) turned out to be a handy resource as well.

In blockchains users broadcast transactions using a peer-to-peer protocol to the entire network of nodes. The transactions remain in the node's mempool -- which acts as a memory queue -- until miners dequeue transactions, marshal them into a block and perform the consensus protocol which orders the blocks, proposed by different miners, in a canonical sequence. Before appending a block to the chain the nodes verify that its validity and that the chain forms an uninterupted sequence of blocks linked together by cryptographic hashes of the previous block, all the way from the genesis block.

# Networks and their genesis

Different Ethereum networks are running simultaneously

`params/config.go`

~~~go
	// MainnetChainConfig is the chain parameters to run a node on the main network.
	MainnetChainConfig = &ChainConfig{
		ChainID:             big.NewInt(1),
		HomesteadBlock:      big.NewInt(1150000),
		DAOForkBlock:        big.NewInt(1920000),
		DAOForkSupport:      true,
		EIP150Block:         big.NewInt(2463000),
		EIP150Hash:          common.HexToHash("0x2086799aeebeae135c246c65021c82b4e15a2c451340993aacfd2751886514f0"),
		EIP155Block:         big.NewInt(2675000),
		EIP158Block:         big.NewInt(2675000),
		ByzantiumBlock:      big.NewInt(4370000),
		ConstantinopleBlock: big.NewInt(7280000),
		PetersburgBlock:     big.NewInt(7280000),
		IstanbulBlock:       big.NewInt(9069000),
		MuirGlacierBlock:    big.NewInt(9200000),
		Ethash:              new(EthashConfig),
	}
~~~

`core/genesis.go`

~~~go
// DefaultGenesisBlock returns the Ethereum main net genesis block.
func DefaultGenesisBlock() *Genesis {
	return &Genesis{
		Config:     params.MainnetChainConfig,
		Nonce:      66,
		ExtraData:  hexutil.MustDecode("0x11bbe8db4e347b4e8c937c1c8370e4b5ed33adb3db69cbdb7a38e1e50b1b82fa"),
		GasLimit:   5000,
		Difficulty: big.NewInt(17179869184),
		Alloc:      decodePrealloc(mainnetAllocData),
	}
}
~~~


`core/types/block.go`

~~~go
// Block represents an entire block in the Ethereum blockchain.
type Block struct {
	header       *Header
	uncles       []*Header
	transactions Transactions

	// caches
	hash atomic.Value
	size atomic.Value

	// Td is used by package core to store the total difficulty
	// of the chain up to and including the block.
	td *big.Int

	// These fields are used by package eth to track
	// inter-peer block relay.
	ReceivedAt   time.Time
	ReceivedFrom interface{}
}
~~~

~~~go
// Header represents a block header in the Ethereum blockchain.
type Header struct {
	ParentHash  common.Hash    `json:"parentHash"       gencodec:"required"`
	UncleHash   common.Hash    `json:"sha3Uncles"       gencodec:"required"`
	Coinbase    common.Address `json:"miner"            gencodec:"required"`
	Root        common.Hash    `json:"stateRoot"        gencodec:"required"`
	TxHash      common.Hash    `json:"transactionsRoot" gencodec:"required"`
	ReceiptHash common.Hash    `json:"receiptsRoot"     gencodec:"required"`
	Bloom       Bloom          `json:"logsBloom"        gencodec:"required"`
	Difficulty  *big.Int       `json:"difficulty"       gencodec:"required"`
	Number      *big.Int       `json:"number"           gencodec:"required"`
	GasLimit    uint64         `json:"gasLimit"         gencodec:"required"`
	GasUsed     uint64         `json:"gasUsed"          gencodec:"required"`
	Time        uint64         `json:"timestamp"        gencodec:"required"`
	Extra       []byte         `json:"extraData"        gencodec:"required"`
	MixDigest   common.Hash    `json:"mixHash"`
	Nonce       BlockNonce     `json:"nonce"`
}
~~~

`core/types/transaction.go`

~~~go
type Transaction struct {
	data txdata
	// caches
	hash atomic.Value
	size atomic.Value
	from atomic.Value
}
~~~

~~~go
type txdata struct {
	AccountNonce uint64          `json:"nonce"    gencodec:"required"`
	Price        *big.Int        `json:"gasPrice" gencodec:"required"`
	GasLimit     uint64          `json:"gas"      gencodec:"required"`
	Recipient    *common.Address `json:"to"       rlp:"nil"` // nil means contract creation
	Amount       *big.Int        `json:"value"    gencodec:"required"`
	Payload      []byte          `json:"input"    gencodec:"required"`

	// Signature values
	V *big.Int `json:"v" gencodec:"required"`
	R *big.Int `json:"r" gencodec:"required"`
	S *big.Int `json:"s" gencodec:"required"`

	// This is only used when marshaling to JSON.
	Hash *common.Hash `json:"hash" rlp:"-"`
}
~~~

- consensus mechanism
	- Ethereum relies on a Proof of Work-based consensus algorithm called _ethash_.
- Uncles and ommers
- UTXO vs. account based models
- patricia merkle trie
- smart contracts
- ethereum 2.0

[^1]: rust, javascript, ...
