---
title: Understanding Ethereum by studying the source code 🧬
tags: ethereum
withtoc: true
summary: >
    For deep understanding it helps to get your hands dirty. With the whitepaper, the yellow paper and the source code for the Go implementation we dive into the inner workings of Ethereum. In this Saturday blog post, we cover among other things the state model, the peer-to-peer network, the EVM and the consensus algorithm. Lastly we discuss the Ethereum ice-age and why it exists. Happy reading!
---

# From papers 🧻 to code 

The [Ethereum Whitepaper](https://ethereum.org/en/whitepaper/) was published in 2013 and a year later some of the implementation details were expanded on in the [Ethereum Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf). Originally there were three implementations of the Ethereum protocol written in Go, C++, and Python[^1]. To understand the core concepts I decided to read both of the papers (again), clone the official [Go implementation](https://github.com/ethereum/go-ethereum) repo `commit 979fc9689 (tag: v1.9.20)`, and study the relationship between the ideas and the actual implementation. The [Ethereum Wiki](https://eth.wiki/) turned out to be a handy resource as well.

In blockchains users broadcast transactions using a peer-to-peer protocol to the entire network of nodes. The transactions remain in the node's transaction pool -- which acts as a memory queue -- until miners dequeue transactions, marshal them into a block and perform the consensus protocol which orders the blocks, proposed by different miners, in a canonical sequence. Before appending a block to the chain the nodes verify that its validity and that the chain forms an uninterrupted sequence of blocks linked together by cryptographic hashes of the previous block, all the way from the genesis block.

# The state model 💾

In contrast to Bitcoin's UTXO state model, in Ethereum the state consists of accounts, identified by an address. Accounts exist in two forms, externally owned accounts and contract accounts. All accounts store an ether balance, denominated in wei[^5] and a nonce, guarding against replay attacks. Externally owned accounts are controlled by private keys but contract accounts are controlled by their contract code, stored in the account, which is able to read and write to an internal storage.

[`core/state/state_object.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/state/state_object.go#L100)

~~~go
// Account is the Ethereum consensus representation of accounts.
// These objects are stored in the main account trie.
type Account struct {
	Nonce    uint64
	Balance  *big.Int
	Root     common.Hash // merkle root of the storage trie
	CodeHash []byte
}
~~~

# The peer-to-peer network 🕸

The [specification](https://github.com/ethereum/devp2p) for the Ethereum peer-to-peer networking protocols includes specs for the ENR metadata format, a Node discovery protocol, a wire format, and the RLPx transport protocol. Ethereum nodes in the network are identified by a public key on the secp256k1 elliptic curve. The node address is the keccak256 hash of the uncompressed public key. Each node is expected to maintain a static private key which is saved and restored between sessions. 

The Ethereum Node Records (ENR) is an open format for peer-to-peer connectivity information. A node record usually contains the network endpoints of a node, i.e. the node's IP addresses and ports. It also holds information about the node's purpose on the network so others can decide whether to connect to it. The ENR includes the public key of the node publishing the record and a signature proving the authenticity of the record.

[`p2p/enr/enr.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/p2p/enr/enr.go#L84)

~~~go
// Record represents a node record. The zero value is an empty record.
type Record struct {
	seq       uint64 // sequence number
	signature []byte // the signature
	raw       []byte // RLP encoded record
	pairs     []pair // sorted list of all key/value pairs
}
~~~

`p2p/enode/node.go`

~~~go
// ID is a unique identifier for each node.
type ID [32]byte

// Node represents a host on the network.
type Node struct {
	r  enr.Record
	id ID
}

// Load retrieves an entry from the underlying record.
func (n *Node) Load(k enr.Entry) error {
	return n.r.Load(k)
}

// IP returns the IP address of the node. This prefers IPv4 addresses.
func (n *Node) IP() net.IP {
	var (
		ip4 enr.IPv4
		ip6 enr.IPv6
	)
	if n.Load(&ip4) == nil {
		return net.IP(ip4)
	}
	if n.Load(&ip6) == nil {
		return net.IP(ip6)
	}
	return nil
}

~~~

## Networks and their genesis

There are different Ethereum networks running simultaneously, mainnet and multiple testnets[^2], identified the chain identifier. The network configuration also includes parameters identifying specific blocks at which point a hardfork happened. Hardforks change the consensus rules in a backwards-incompatible way, such as altering the block reward or [confiscating ether from a heist](/2020/08/a-bref-history-of-ethereum/#a-fork-in-the-road).

[`params/config.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/params/config.go#L56)

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

All nodes in a network must agree on the genesis block which anchors the chain of trust. In addition to standard block fields, the initial ether allocation from the pre-sale is configured in the genesis. The data is hardcoded in [RLP](https://eth.wiki/en/fundamentals/design-rationale#rlp)-encoding which is decoded prior to account allocation.

[`core/genesis.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/genesis.go#L67)

~~~go
// GenesisAlloc specifies the initial state that is part of the genesis block.
type GenesisAlloc map[common.Address]GenesisAccount

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

func decodePrealloc(data string) GenesisAlloc {
	var p []struct{ Addr, Balance *big.Int }
	if err := rlp.NewStream(strings.NewReader(data), 0).Decode(&p); err != nil {
		panic(err)
	}
	ga := make(GenesisAlloc, len(p))
	for _, account := range p {
		ga[common.BigToAddress(account.Addr)] = GenesisAccount{Balance: account.Balance}
	}
	return ga
}
~~~

## Connecting via bootnodes

Nodes discover peers in the network with the assistance of bootnodes which can either be provided as a command line flag on startup or the node can rely on a default set of hardcoded bootnodes run by the Ethereum Foundation. As peers are discovered, the node tries to connect to them and initiates the handshake protocol. A list is maintained of successfully connected peers[^3].

[`params/bootnodes.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/params/bootnodes.go#L23)

~~~go
// MainnetBootnodes are the enode URLs of the P2P bootstrap nodes running on
// the main Ethereum network.
var MainnetBootnodes = []string{
	// Ethereum Foundation Go Bootnodes
	"enode://d860a01f9722d78051619d1e2351aba3f43f943f6f00718d1b9baa4101932a1f5011f16bb2b1bb35db20d6fe28fa0bf09636d26a87d31de9ec6203eeedb1f666@18.138.108.67:30303",   // bootnode-aws-ap-southeast-1-001
	"enode://22a8232c3abc76a16ae9d6c3b164f98775fe226f0917b0ca871128a74a8e9630b458460865bab457221f1d448dd9791d24c4e5d88786180ac185df813a68d4de@3.209.45.79:30303",     // bootnode-aws-us-east-1-001
	"enode://ca6de62fce278f96aea6ec5a2daadb877e51651247cb96ee310a318def462913b653963c155a0ef6c7d50048bba6e6cea881130857413d9f50a621546b590758@34.255.23.113:30303",   // bootnode-aws-eu-west-1-001
	"enode://279944d8dcd428dffaa7436f25ca0ca43ae19e7bcf94a8fb7d1641651f92d121e972ac2e8f381414b80cc8e5555811c2ec6e1a99bb009b3f53c4c69923e11bd8@35.158.244.151:30303",  // bootnode-aws-eu-central-1-001
	"enode://8499da03c47d637b20eee24eec3c356c9a2e6148d6fe25ca195c7949ab8ec2c03e3556126b0d7ed644675e78c4318b08691b7b57de10e5f0d40d05b09238fa0a@52.187.207.27:30303",   // bootnode-azure-australiaeast-001
	"enode://103858bdb88756c71f15e9b5e09b56dc1be52f0a5021d46301dbbfb7e130029cc9d0d6f73f693bc29b665770fff7da4d34f3c6379fe12721b5d7a0bcb5ca1fc1@191.234.162.198:30303", // bootnode-azure-brazilsouth-001
	"enode://715171f50508aba88aecd1250af392a45a330af91d7b90701c436b618c86aaa1589c9184561907bebbb56439b8f8787bc01f49a7c77276c58c1b09822d75e8e8@52.231.165.108:30303",  // bootnode-azure-koreasouth-001
	"enode://5d6d7cd20d6da4bb83a1d28cadb5d409b64edf314c0335df658c1a54e32c7c4a7ab7823d57c39b6a757556e68ff1df17c748b698544a55cb488b52479a92b60f@104.42.217.25:30303",   // bootnode-azure-westus-001
}
~~~

## Discovering peers

The discovery protocol relies on a Kademlia-like DHT that stores information about Ethereum nodes.  Participants in the discovery protocol are expected to maintain a ENR containing up-to-date information. To resolve the current record of any node public key, a DHT lookup is performed and when the node is found, the ENR is requested directly from the peer.


[`p2p/discover/v4_udp.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/p2p/discover/v4_udp.go#L339)

~~~go
// RequestENR sends enrRequest to the given node and waits for a response.
func (t *UDPv4) RequestENR(n *enode.Node) (*enode.Node, error) {
	addr := &net.UDPAddr{IP: n.IP(), Port: n.UDP()}
	t.ensureBond(n.ID(), addr)

	req := &enrRequestV4{
		Expiration: uint64(time.Now().Add(expiration).Unix()),
	}
	packet, hash, err := t.encode(t.priv, req)
	if err != nil {
		return nil, err
	}
	// Add a matcher for the reply to the pending reply queue. Responses are matched if
	// they reference the request we're about to send.
	rm := t.pending(n.ID(), addr.IP, p_enrResponseV4, func(r interface{}) (matched bool, requestDone bool) {
		matched = bytes.Equal(r.(*enrResponseV4).ReplyTok, hash)
		return matched, matched
	})
	// Send the packet and wait for the reply.
	t.write(addr, n.ID(), req.name(), packet)
	if err := <-rm.errc; err != nil {
		return nil, err
	}
	// Verify the response record.
	respN, err := enode.New(enode.ValidSchemes, &rm.reply.(*enrResponseV4).Record)
	if err != nil {
		return nil, err
	}
	if respN.ID() != n.ID() {
		return nil, fmt.Errorf("invalid ID in response record")
	}
	if respN.Seq() < n.Seq() {
		return n, nil // response record is older
	}
	if err := netutil.CheckRelayIP(addr.IP, respN.IP()); err != nil {
		return nil, fmt.Errorf("invalid IP in response record: %v", err)
	}
	return respN, nil
}
~~~

## Propagating blocks

Now that our node is connected to its peers it must download all blocks from genesis block to the latest block. The total difficulty is used to determine which peer has the heaviest chain. The node always verifies the proof-of-work values in the block headers and then can either recreate the state by downloading the transactions and executing them in the EVM or by downloading merkle tree nodes and contract code incrementally until the entire tree is synchronized. Subsequent blocks are propagated to all nodes in the network in two steps; first a new block is announced and nodes perform basic validation before relaying the announcement to a small fraction of connected peers then transactions contained in the block are executed and if valid a hash of the block is propagated to peers it didn't notify earlier. Those peers may need to request the full block from a node they are connected to.

Ethereum blocks are produced on average every 15 seconds. Contrasted with Bitcoin's 10 minute block times, this increases both throughput and shortens finality at the expense of a higher likelihood that two different blocks are proposed at the same hight. Uncles are stale blocks that contribute to the security of the chain but are not considered the canonical truth for that particular chain height. Instead of orphaning stale blocks, Ethereum pays for uncles adding to the security of the chain. The canonical chain is therefore not the necessarily the longest chain but the heaviest when all work, including work performed by uncles, is counted.

The block header contains a number of interesting hashes which provide cryptographic proofs of data authenticity which has possibly been acquired from different peers. The parent hash is the hash of the block prior to the current block which is the keccak256 hash of the RLP encoded block header. The uncle hash and transactions hash are calculated in the same way on the encoded block headers of the uncles and the transactions, respectively. The state hash is a hash of the root node of a Merkle Patricia tree[^7], a mapping between addresses and account states after all transactions are executed, serialized as RLP.

[`core/types/block.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/types/block.go#L168)

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

// Hash returns the keccak256 hash of b's header.
// The hash is computed on the first call and cached thereafter.
func (b *Block) Hash() common.Hash {
	if hash := b.hash.Load(); hash != nil {
		return hash.(common.Hash)
	}
	v := b.header.Hash()
	b.hash.Store(v)
	return v
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

// Hash returns the block hash of the header, which is simply the keccak256 hash of its
// RLP encoding.
func (h *Header) Hash() common.Hash {
	return rlpHash(h)
}

func rlpHash(x interface{}) (h common.Hash) {
	sha := hasherPool.Get().(crypto.KeccakState)
	defer hasherPool.Put(sha)
	sha.Reset()
	rlp.Encode(sha, x)
	sha.Read(h[:])
	return h
}

~~~

# State transition ⏭

## Validating transactions

If a transaction amount is specified that amount of ether denominated in wei is sent to another account identified by the recipient address. If the recipients account does not exist it is created. The sender account is derived from the signature values which must be valid and simultaneously authorize the account state change. The nonce in the transaction must be exactly one higher than the nonce in the sender account state essentially guarding against race conditions of two competing transactions trying to modify the same account state. The transfer transaction fails if the sender account balance has insufficient ether compared to the transaction amount. 

[`core/types/transaction.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/types/transaction.go#L39)

~~~go
type Transaction struct {
	data txdata
	// caches
	hash atomic.Value
	size atomic.Value
	from atomic.Value
}

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

[`core/types/transaction_signing.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/types/transaction_signing.go#L105)

~~~go
// EIP155Transaction implements Signer using the EIP155 rules.
type EIP155Signer struct {
	chainId, chainIdMul *big.Int
}

// Hash returns the hash to be signed by the sender.
// It does not uniquely identify the transaction.
func (s EIP155Signer) Hash(tx *Transaction) common.Hash {
	return rlpHash([]interface{}{
		tx.data.AccountNonce,
		tx.data.Price,
		tx.data.GasLimit,
		tx.data.Recipient,
		tx.data.Amount,
		tx.data.Payload,
		s.chainId, uint(0), uint(0),
	})
}

func (s EIP155Signer) Sender(tx *Transaction) (common.Address, error) {
	if !tx.Protected() {
		return HomesteadSigner{}.Sender(tx)
	}
	if tx.ChainId().Cmp(s.chainId) != 0 {
		return common.Address{}, ErrInvalidChainId
	}
	V := new(big.Int).Sub(tx.data.V, s.chainIdMul)
	V.Sub(V, big8)
	return recoverPlain(s.Hash(tx), tx.data.R, tx.data.S, V, true)
}

func recoverPlain(sighash common.Hash, R, S, Vb *big.Int, homestead bool) (common.Address, error) {
	if Vb.BitLen() > 8 {
		return common.Address{}, ErrInvalidSig
	}
	V := byte(Vb.Uint64() - 27)
	if !crypto.ValidateSignatureValues(V, R, S, homestead) {
		return common.Address{}, ErrInvalidSig
	}
	// encode the signature in uncompressed format
	r, s := R.Bytes(), S.Bytes()
	sig := make([]byte, crypto.SignatureLength)
	copy(sig[32-len(r):32], r)
	copy(sig[64-len(s):64], s)
	sig[64] = V
	// recover the public key from the signature
	pub, err := crypto.Ecrecover(sighash[:], sig)
	if err != nil {
		return common.Address{}, err
	}
	if len(pub) == 0 || pub[0] != 4 {
		return common.Address{}, errors.New("invalid public key")
	}
	var addr common.Address
	copy(addr[:], crypto.Keccak256(pub[1:])[12:])
	return addr, nil
}

~~~

[`crypto/crypto.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/crypto/crypto.go#L248)

~~~go
// ValidateSignatureValues verifies whether the signature values are valid with
// the given chain rules. The v value is assumed to be either 0 or 1.
func ValidateSignatureValues(v byte, r, s *big.Int, homestead bool) bool {
	if r.Cmp(common.Big1) < 0 || s.Cmp(common.Big1) < 0 {
		return false
	}
	// reject upper range of s values (ECDSA malleability)
	// see discussion in secp256k1/libsecp256k1/include/secp256k1.h
	if homestead && s.Cmp(secp256k1halfN) > 0 {
		return false
	}
	// Frontier: allow s to be in full N range
	return r.Cmp(secp256k1N) < 0 && s.Cmp(secp256k1N) < 0 && (v == 0 || v == 1)
}
~~~

## Executing code in the EVM

The code in Ethereum contracts is written in a low-level, stack-based bytecode language, referred to as EVM[^4] code. The code consists of a series of bytes, where each byte represents an [operation](https://ethervm.io/#opcodes). The operations have access to three types of storage:

- The stack, a last-in-first-out container to which values can be pushed and popped.
- Memory, an infinitely expandable byte array.
- The contract's long-term storage, a key/value store. Unlike stack and memory, which reset after computation ends, storage persists for the long term.

If a payload is present but the transaction recipient is left unspecified a new contract is created at an address equal to the hash of the payload and the code is stored in the contract account state. The payload of a transaction creating a contract is itself bytecode that runs the contract constructor, sets up the initial contract state and returns the final contract bytecode, i.e. constructors are not present in the contract once deployed. To invoke a contract the contract account address is specified as the recipient and the transaction payload formatted according to the contract ABI[^8] is used as input when invoking the contract code. Ether balances in contract accounts are governed by the contract code.

Contracts can interact with other contracts by sending messages to the other contract account address.

[`core/vm/opcodes.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/vm/opcodes.go#L56)

~~~go
// 0x10 range - comparison ops.
const (
	LT OpCode = iota + 0x10
	GT
	SLT
	SGT
	EQ
	ISZERO
	AND
	OR
	XOR
	NOT
	BYTE
	SHL
	SHR
	SAR

	SHA3 OpCode = 0x20
)
~~~

The EVM is a Turing-complete runtime environment and as such it can execute all kinds of computations, including infinite loops. As a DDoS prevention executing an operation carries a cost measured in gas. Different operations have different gas costs accessing the contracts long term storage may be much more expensive than adding two integers together[^6]. Gas is priced in ether, denominated in wei, so the the total cost of a computation is \(gas * price_{gas}\). Revisiting the `txdata struct` we can now discuss two mandatory fields, the gas price and the gas limit. The gas price is the amount that the originator of the transaction is willing to pay for the gas required to execute the operations in the payload. The gas limit sets an upper bound on the amount of gas the originator of the transaction is willing to pay for. 

~~~go
	Price        *big.Int        `json:"gasPrice" gencodec:"required"`
	GasLimit     uint64          `json:"gas"      gencodec:"required"`
	Payload      []byte          `json:"input"    gencodec:"required"`
~~~


[`core/vm/gas_table.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/vm/gas_table.go#L250)

~~~go
func gasSha3(evm *EVM, contract *Contract, stack *Stack, mem *Memory, memorySize uint64) (uint64, error) {
	gas, err := memoryGasCost(mem, memorySize)
	if err != nil {
		return 0, err
	}
	wordGas, overflow := stack.Back(1).Uint64WithOverflow()
	if overflow {
		return 0, ErrGasUintOverflow
	}
	if wordGas, overflow = math.SafeMul(toWordSize(wordGas), params.Sha3WordGas); overflow {
		return 0, ErrGasUintOverflow
	}
	if gas, overflow = math.SafeAdd(gas, wordGas); overflow {
		return 0, ErrGasUintOverflow
	}
	return gas, nil
}
~~~

When nodes execute the operations in sequence they must constantly compare accumulated gas cost with the gas limit. If the limit is hit, the balance of the originator account is insufficient for further computation, or the code executes an invalid operation, the node rolls the state back to undo all changes the operations in the transaction have done. Even though the computation failed all the nodes already performed computation and therefore gas payments are not rolled back.

[`core/vm/interpreter.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/vm/interpreter.go#L244)

~~~go

		// Static portion of gas
		cost = operation.constantGas // For tracing
		if !contract.UseGas(operation.constantGas) {
			return nil, ErrOutOfGas
		}
~~~
~~~go
		// Dynamic portion of gas
		// consume the gas and return an error if not enough gas is available.
		// cost is explicitly set so that the capture state defer method can get the proper cost
		if operation.dynamicGas != nil {
			var dynamicCost uint64
			dynamicCost, err = operation.dynamicGas(in.evm, contract, stack, mem, memorySize)
			cost += dynamicCost // total cost, for debug tracing
			if err != nil || !contract.UseGas(dynamicCost) {
				return nil, ErrOutOfGas
			}
		}
~~~

[`core/vm/contract.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/vm/contract.go#L179)

~~~go
// UseGas attempts the use gas and subtracts it and returns true on success
func (c *Contract) UseGas(gas uint64) (ok bool) {
	if c.Gas < gas {
		return false
	}
	c.Gas -= gas
	return true
}
~~~

# Transacting ⚙️

## Broadcasting transactions

Ethereum uses the [ECDSA signature](https://cryptobook.nakov.com/digital-signatures/ecdsa-sign-verify-messages) scheme for the secp256k1 elliptic curve. The `crypto` module provides two implementations for `crypto.Sign`, one relying on an [elliptic curve cryptography library](elliptic curve cryptography library ) written in pure Go and the other relying on [libsecp256k1](https://github.com/bitcoin-core/secp256k1) written C and called using cgo. ECDSA signatures are returned as a pair of integers \( (r, s) \), each in the range \( [1, 2^{256}-1] \), where \( r \) is the \( x \)-coordinate of a random point \( R = k * G \) and \( s = k^{-1} * (h + r * privKey) \) is a proof that the signer knows message \( h = hash(msg) \) and private key \( privKey \). With a signed transaction a node relays it to the peer-to-peer network as described above.

It is important to know that the ECDSA signature scheme allows the public key to be recovered from the signed message together with the signature. The recovery process is based on some mathematical computations and returns 0, 1 or 2 possible points that are valid public keys, corresponding to the signature. To avoid this ambiguity, some ECDSA implementations add one additional bit v to the signature during the signing process and it takes the form \( (r, s, v) \). From this extended ECDSA signature and the signed message, the signer's public key can be restored with confidence. Ethereum uses the extended signatures for the signed transactions on the chain to save storage and bandwidth.

[`core/types/transaction.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/types/transaction.go#L248)

~~~go
// WithSignature returns a new transaction with the given signature.
// This signature needs to be in the [R || S || V] format where V is 0 or 1.
func (tx *Transaction) WithSignature(signer Signer, sig []byte) (*Transaction, error) {
	r, s, v, err := signer.SignatureValues(tx, sig)
	if err != nil {
		return nil, err
	}
	cpy := &Transaction{
		data: tx.data,
		time: tx.time,
	}
	cpy.data.R, cpy.data.S, cpy.data.V = r, s, v
	return cpy, nil
}
~~~

[`core/types/transaction_signing.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/types/transaction_signing.go#L55)

~~~go
// SignTx signs the transaction using the given signer and private key
func SignTx(tx *Transaction, s Signer, prv *ecdsa.PrivateKey) (*Transaction, error) {
	h := s.Hash(tx)
	sig, err := crypto.Sign(h[:], prv)
	if err != nil {
		return nil, err
	}
	return tx.WithSignature(s, sig)
}

// SignatureValues returns signature values. This signature
// needs to be in the [R || S || V] format where V is 0 or 1.
func (s EIP155Signer) SignatureValues(tx *Transaction, sig []byte) (R, S, V *big.Int, err error) {
	R, S, V, err = HomesteadSigner{}.SignatureValues(tx, sig)
	if err != nil {
		return nil, nil, nil, err
	}
	if s.chainId.Sign() != 0 {
		V = big.NewInt(int64(sig[64] + 35))
		V.Add(V, s.chainIdMul)
	}
	return R, S, V, nil
}

// HomesteadTransaction implements TransactionInterface using the
// homestead rules.
type HomesteadSigner struct{ FrontierSigner }

// SignatureValues returns signature values. This signature
// needs to be in the [R || S || V] format where V is 0 or 1.
func (hs HomesteadSigner) SignatureValues(tx *Transaction, sig []byte) (r, s, v *big.Int, err error) {
	return hs.FrontierSigner.SignatureValues(tx, sig)
}

type FrontierSigner struct{}

// SignatureValues returns signature values. This signature
// needs to be in the [R || S || V] format where V is 0 or 1.
func (fs FrontierSigner) SignatureValues(tx *Transaction, sig []byte) (r, s, v *big.Int, err error) {
	if len(sig) != crypto.SignatureLength {
		panic(fmt.Sprintf("wrong size for signature: got %d, want %d", len(sig), crypto.SignatureLength))
	}
	r = new(big.Int).SetBytes(sig[:32])
	s = new(big.Int).SetBytes(sig[32:64])
	v = new(big.Int).SetBytes([]byte{sig[64] + 27})
	return r, s, v, nil
}
~~~

## Mining transactions into a block ⛏

If nodes in the peer-to-peer network, located in different parts of the world, would simply update the state based on the transactions in the order they discover them, different nodes would derive different states because there is no guaranteed order in which transactions arrive to nodes. To solve this problem, there needs to be a consensus protocol for the nodes to order them in a canonical order. The consensus protocol for the Ethereum mainnet includes a proof-of-work algorithm called Ethash. The protocol relies on computing the difficulty for blocks, \( H_d \), which is computed from the previous block's difficulty, the current timestamp and includes a time bomb-factor 🧨. The purpose of the difficulty is twofold; adjust the time it takes to mine a block to the 15 second target and to determine the canonical blockchain in case of forks. The difficulty is included in blocks so that once they are mined in to the blockchain history it is easy to compute both the previous block's difficulty and the total difficulty as the sum of all previous blocks' difficulty. 

[`core/types/block.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/core/types/block.go#L80)

~~~go
	Difficulty  *big.Int       `json:"difficulty"       gencodec:"required"`
~~~

Ethash repeatedly computes the \( PoW \) function, incrementing the nonce each time, until the nonce, \( H_n \), satisfy the relations:

\[ n \leq \frac{2_{256}}{H_d} \; ∧ \; m = H_m \]

with \( (n, m) = PoW(H_{\hat{n}}, H_n, d) \) and \( H_m \) the header's mix-hash.
Where \( H_{\hat{n}} \) is the new block’s header \( H \), but without the nonce and mix-hash components, \( d \) being the current DAG, a large data set needed to compute the mix-hash, and \( PoW \) is the proof-of-work function: this evaluates to an array with the first item being the mix-hash, to prove that a correct DAG has been used, and the second item being a pseudo-random number cryptographically dependent on \( H \) and \( d \). Given an approximately uniform distribution in the range \( [0, 2^{64}) \), the expected time to find a solution is proportional to the difficulty, \( H_d \).

The difficulty time bomb refers to a component of the block difficulty, \( H_d \), which grows exponentially over time making it impossible to mine. That future period is commonly referred to as the ice-age. The purpose of the time bomb is to signal the intent to change Ethereum from a proof-of-work to a proof-of-stake based consensus algorithm. This approach encourages the network to transition to [Ethereum 2.0](https://ethereum.org/en/eth2/) and staking faster or face the consequences of slower block times. The time bomb has been delayed three times with hard forks; Byzantium upgrade in October 2017, Constantinople in February 2019, and Muir Glacier in January 2020 which delayed the difficulty bomb for another 4,000,000 blocks giving the community until roughly July 2021 to launch Ethereum 2.0’s [finality gadget](https://medium.com/@ralexstokes/the-finality-gadget-2bf608529e50) or delay again before the ice-age begins to slow block times to over 20 seconds.

[`consensus/ethash/consensus.go`](https://github.com/ethereum/go-ethereum/blob/v1.9.20/consensus/ethash/consensus.go#L48)

~~~go
	// calcDifficultyEip2384 is the difficulty adjustment algorithm as specified by EIP 2384.
	// It offsets the bomb 4M blocks from Constantinople, so in total 9M blocks.
	// Specification EIP-2384: https://eips.ethereum.org/EIPS/eip-2384
	calcDifficultyEip2384 = makeDifficultyCalculator(big.NewInt(9000000))

// CalcDifficulty is the difficulty adjustment algorithm. It returns
// the difficulty that a new block should have when created at time
// given the parent block's time and difficulty.
func CalcDifficulty(config *params.ChainConfig, time uint64, parent *types.Header) *big.Int {
	next := new(big.Int).Add(parent.Number, big1)
	switch {
	case config.IsMuirGlacier(next):
		return calcDifficultyEip2384(time, parent)
	case config.IsConstantinople(next):
		return calcDifficultyConstantinople(time, parent)
	case config.IsByzantium(next):
		return calcDifficultyByzantium(time, parent)
	case config.IsHomestead(next):
		return calcDifficultyHomestead(time, parent)
	default:
		return calcDifficultyFrontier(time, parent)
	}
}

// makeDifficultyCalculator creates a difficultyCalculator with the given bomb-delay.
// the difficulty is calculated with Byzantium rules, which differs from Homestead in
// how uncles affect the calculation
func makeDifficultyCalculator(bombDelay *big.Int) func(time uint64, parent *types.Header) *big.Int {
	// Note, the calculations below looks at the parent number, which is 1 below
	// the block number. Thus we remove one from the delay given
	bombDelayFromParent := new(big.Int).Sub(bombDelay, big1)
	return func(time uint64, parent *types.Header) *big.Int {
		// https://github.com/ethereum/EIPs/issues/100.
		// algorithm:
		// diff = (parent_diff +
		//         (parent_diff / 2048 * max((2 if len(parent.uncles) else 1) - ((timestamp - parent.timestamp) // 9), -99))
		//        ) + 2^(periodCount - 2)

		bigTime := new(big.Int).SetUint64(time)
		bigParentTime := new(big.Int).SetUint64(parent.Time)

		// holds intermediate values to make the algo easier to read & audit
		x := new(big.Int)
		y := new(big.Int)

		// (2 if len(parent_uncles) else 1) - (block_timestamp - parent_timestamp) // 9
		x.Sub(bigTime, bigParentTime)
		x.Div(x, big9)
		if parent.UncleHash == types.EmptyUncleHash {
			x.Sub(big1, x)
		} else {
			x.Sub(big2, x)
		}
		// max((2 if len(parent_uncles) else 1) - (block_timestamp - parent_timestamp) // 9, -99)
		if x.Cmp(bigMinus99) < 0 {
			x.Set(bigMinus99)
		}
		// parent_diff + (parent_diff / 2048 * max((2 if len(parent.uncles) else 1) - ((timestamp - parent.timestamp) // 9), -99))
		y.Div(parent.Difficulty, params.DifficultyBoundDivisor)
		x.Mul(y, x)
		x.Add(parent.Difficulty, x)

		// minimum difficulty can ever be (before exponential factor)
		if x.Cmp(params.MinimumDifficulty) < 0 {
			x.Set(params.MinimumDifficulty)
		}
		// calculate a fake block number for the ice-age delay
		// Specification: https://eips.ethereum.org/EIPS/eip-1234
		fakeBlockNumber := new(big.Int)
		if parent.Number.Cmp(bombDelayFromParent) >= 0 {
			fakeBlockNumber = fakeBlockNumber.Sub(parent.Number, bombDelayFromParent)
		}
		// for the exponential factor
		periodCount := fakeBlockNumber
		periodCount.Div(periodCount, expDiffPeriod)

		// the exponential factor, commonly referred to as "the bomb"
		// diff = diff + 2^(periodCount - 2)
		if periodCount.Cmp(big1) > 0 {
			y.Sub(periodCount, big2)
			y.Exp(big2, y, nil)
			x.Add(x, y)
		}
		return x
	}
}
~~~

This concludes my journey into Ethereum and its Go implementation. There are a number of other interesting aspects which unfortunately I couldn't cover but I encourage you to take the leap and dive right into the beast 🧟. 

[Gisli]{.handwritten}

[^1]: Today there exist Ethereum implementations in C++, Go, Python, Java, Ruby, JavaScript, Haskell and .NET.
[^2]: A list of [EVM networks](https://chainid.network/).
[^3]: By default the maximum number of connected peers is 50.
[^4]: [Ethereum Virtual Machine](https://eth.wiki/concepts/evm/evm) (EVM) is a Turing complete virtual machine for Ethereum smart contracts.
[^5]: The lowest unit is a wei. Other denominations include szabo (\( 10^{12} \) wei), finney (\( 10^{15} \) wei), and ether (\( 10^{18} \) wei).
[^6]: [Gas Costs from Yellow Paper -- EIP-150 Revision](https://docs.google.com/spreadsheets/d/1n6mRqkBz3iWcOlRem_mO09GtSKEKrAsfO7Frgx18pNU/edit#gid=0)
[^7]: [Merkle Patricia ](https://eth.wiki/en/fundamentals/patricia-tree) tries provide a fully deterministic, cryptographically authenticated data structure that can be used to store all (key, value) bindings. 
[^8]: The Contract [Application Binary Interface](https://solidity.readthedocs.io/en/develop/abi-spec.html) (ABI) is the standard way to interact with contracts in the Ethereum ecosystem, both from outside the blockchain and for contract-to-contract interaction.
