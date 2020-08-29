---
title: Understanding Ethereum by studying the source code
tags: ethereum
withtoc: true
---

# From papers to code

The [Ethereum Whitepaper](https://ethereum.org/en/whitepaper/) was published in 2013 and a year later some of the implementation details were expanded on in the [Ethereum Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf). Originally there were three implementations of the Ethereum protocol written in Go, C++, and Python[^1]. To understand the core concepts I decided to read both of the papers (again), clone the official [Go implementation](https://github.com/ethereum/go-ethereum) repo `commit 979fc9689 (tag: v1.9.20)`, and study the relationship between the ideas and the actual implementation. The [Ethereum Wiki](https://eth.wiki/) turned out to be a handy resource as well.

In blockchains users broadcast transactions using a peer-to-peer protocol to the entire network of nodes. The transactions remain in the node's transaction pool -- which acts as a memory queue -- until miners dequeue transactions, marshal them into a block and perform the consensus protocol which orders the blocks, proposed by different miners, in a canonical sequence. Before appending a block to the chain the nodes verify that its validity and that the chain forms an uninterupted sequence of blocks linked together by cryptographic hashes of the previous block, all the way from the genesis block.

# The state model

In contrast to Bitcoin's UTXO state model, in Ethereum the state consists of accounts, identified by an address. Accounts exist in two forms, externally owned accounts and contract accounts. All accounts store an ether balance, denominated in wei[^5] and a nonce, guarding against replay attacks. Externally owned accounts are controlled by private keys but contract accounts are controlled by their contract code stored in the account and optionally able to read and write to an internal storage.

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

# The peer-to-peer network

The [specification](https://github.com/ethereum/devp2p) for the Ethereum peer-to-peer networking protocols includes specs for ENR metadata format, a Node discovery protocol, a wire format, and the RLPx transport protocol. Ethereum nodes in the network are identified by a public key on the secp256k1 elliptic curve. The node address is the keccak256 hash of the uncompressed public key. Each node is expected to maintain a static private key which is saved and restored between sessions. 

The Ethereum Node Recods (ENR) is an open format for p2p connectivity information. A node record usually contains the network endpoints of a node, i.e. the node's IP addresses and ports. It also holds information about the node's purpose on the network so others can decide whether to connect to the node. The ENR includes the public key of the node publishing the record and a signature proving the authenticity of the record.

`p2p/enr/enr.go`

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

All nodes in a network must agree on the genesis block which anchors the chain of trust. In addition to standard block fields, the initial ether allocation from the pre-sale is configured in the genesis. The data is hardcoded in [RLP](https://eth.wiki/en/fundamentals/design-rationale#rlp)-encoding which is decoded prior to account allocation.

`core/genesis.go`

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

Nodes discover peers in the network with the assistance of bootnodes which can either be provided as a command line flag on startup or the node can rely on a default set of hardcoded bootnodes run by the Ethereum Foundation. As peers are discovered, the node tries to connect to them and initiates the handshake protocol. A list is maintained of succesfully connected peers[^3].

`params/bootnodes.go`

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


`p2p/discover/v4_udp.go`

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

## Propogating blocks

Now that our node is connected to its peers it must download all blocks from genesis block to the latest block. The total difficulty is used to determine which peer has the heaviest chain. The node always verifies the proof-of-work values in the block headers and then can either recreate the state by downloading the transactions and executing them in the EVM[^4] or by downloading merkle tree nodes and contract code incrementally until the entire tree is synchronized. Subsequent blocks are propagated to all nodes in the network in two steps; first a new block is announced and nodes perform basic validation before relaying the announcement to a small fraction of connected peers then transactions contained in the block are executed and if valid a hash of the block is propagated to peers it didn't notify earlyer. Those peers may need to request the full block from a node they are connected to.

Ethereum blocks are produced on average every 15 seconds. This increases both throughput and shortens finality at the expense of a higher likelihood that two different blocks are proposed at the same hight in the blockchain. Uncles are stale blocks that contribute to the security of the chain but are not considered the canonical truth for that particular chain height. Instead of orphaning stale blocks, Ethereum pays for uncles adding to the security of the chain. The canonical chain is therefore not the neccesarily the longst chain but the heaviest when all work, including work performed by uncles, is counted.

The block header contains a number of interesting hashes which provide cryptographic proofs of data authenticity which has possibly been acquired from different peers. The parent hash is the hash of the block prior to the current block which is the keccak256 hash of the RLP encoded block header. The uncle hash and transactions hash are calculated in the same way on the encoded block headers of the uncles and the transactions, respectivily. The state hash is a hash of the root node of a Merkle Patricia tree, a mapping between addresses and account states after all transactions are executed, serialized as RLP.

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

# State transition

## Validating transactions

If a transaction amount is specified a specific amount of ether denominated in wei is sent to another account identified by the recipient address. If the recipients account does not exist it is created. The sender account is derived from the signature values which simultaneously authorize the state change. A valid signature is required. The nonce in the transaction must be exactly one higher than the nonce in the sender account state essentially guarding against race conditions of two competing transactions trying to modify the same account state. The transfer transaction fails if the sender account balance has insufficient ether compared to the transaction amount. 

`core/types/transaction.go`

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

`core/types/transaction_signing.go`

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

`crypto/crypto.go`

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

The code in Ethereum contracts is written in a low-level, stack-based bytecode language, referred to as EVM code. The code consists of a series of bytes, where each byte represents an [operation](https://ethervm.io/#opcodes). The operations have access to three types of storage:

- The stack, a last-in-first-out container to which values can be pushed and popped
- Memory, an infinitely expandable byte array
- The contract's long-term storage, a key/value store. Unlike stack and memory, which reset after computation ends, storage persists for the long term.

If a payload is present but the transaction recipient is left unspecified a new contract is created at an address equal to the hash of the payload and the code is stored in the contract account state. The payload of a transaction creating a contract is itself bytecode that runs the contract constructor, sets up the initial contract state and returns the final contract bytecode, i.e. constructors are not present in the contract once deployed. To invoke a contract the contract account address is specified as the recipient and the transaction payload formatted according to the contract ABI is used as input when invoking the contract code. Ether balances in contract accounts are governed by the contract code.

Contracts can interact with other contracts by sending messages to the other contract account address.

`core/vm/opcodes.go`

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

The EVM is a Turing-complete[^6] runtime environment and as such it can execute all kinds of computations, including infinite loops. As a DDoS prevention executing an operation carries a cost measured in gas. Different operations have different gas costs accessing the contracts long term storage may be much more expensive than adding two integers together[^7]. Gas is priced in ether, denominated in wei, so the the total cost of a computation is \(gas * price_{gas}\). Revisiting the `txdata struct` we can now discuss two mandatory fields, the gas price and the gas limit. The gas price is the amount that the originator of the transaction is willing to pay for the gas required to execute the operations in the payload. The gas limit sets an upper bound on the amount of gas the originator of the transaction is willing to pay for. 

~~~go
	Price        *big.Int        `json:"gasPrice" gencodec:"required"`
	GasLimit     uint64          `json:"gas"      gencodec:"required"`
	Payload      []byte          `json:"input"    gencodec:"required"`
~~~


`core/vm/gas_table.go`

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

`core/vm/interpreter.go`

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

`core/vm/contract.go`

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

# Transacting

## Broadcasting transactions

Ethereum uses the ECDSA signature scheme for the secp256k1 elliptic curve. The `crypto` module provides two implementations for `crypto.Sign`, one relying on an [elliptic curve cryptography library](elliptic curve cryptography library ) written in pure Go and the other relying on [libsecp256k1](https://github.com/bitcoin-core/secp256k1) written C and called using cgo. ECDSA signatures are returned as a pair of integers \( (r, s) \), each in the range \( [1, 2^{256}-1] \), where \( r \) is the \( x \)-coordinate of a random point \( R = k * G \) and \( s = k^{-1} * (h + r * privKey) \) is a proof that the signer knows message \( h = hash(msg) \) and private key \( privKey \).


`core/types/transaction.go`

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

`core/types/transaction_signing.go`

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

## Mining transactions into a block



`consensus/consensus.go`

~~~go
// Engine is an algorithm agnostic consensus engine.
type Engine interface {
	// Author retrieves the Ethereum address of the account that minted the given
	// block, which may be different from the header's coinbase if a consensus
	// engine is based on signatures.
	Author(header *types.Header) (common.Address, error)

	// VerifyHeader checks whether a header conforms to the consensus rules of a
	// given engine. Verifying the seal may be done optionally here, or explicitly
	// via the VerifySeal method.
	VerifyHeader(chain ChainHeaderReader, header *types.Header, seal bool) error

	// VerifyHeaders is similar to VerifyHeader, but verifies a batch of headers
	// concurrently. The method returns a quit channel to abort the operations and
	// a results channel to retrieve the async verifications (the order is that of
	// the input slice).
	VerifyHeaders(chain ChainHeaderReader, headers []*types.Header, seals []bool) (chan<- struct{}, <-chan error)

	// VerifyUncles verifies that the given block's uncles conform to the consensus
	// rules of a given engine.
	VerifyUncles(chain ChainReader, block *types.Block) error

	// VerifySeal checks whether the crypto seal on a header is valid according to
	// the consensus rules of the given engine.
	VerifySeal(chain ChainHeaderReader, header *types.Header) error

	// Prepare initializes the consensus fields of a block header according to the
	// rules of a particular engine. The changes are executed inline.
	Prepare(chain ChainHeaderReader, header *types.Header) error

	// Finalize runs any post-transaction state modifications (e.g. block rewards)
	// but does not assemble the block.
	//
	// Note: The block header and state database might be updated to reflect any
	// consensus rules that happen at finalization (e.g. block rewards).
	Finalize(chain ChainHeaderReader, header *types.Header, state *state.StateDB, txs []*types.Transaction,
		uncles []*types.Header)

	// FinalizeAndAssemble runs any post-transaction state modifications (e.g. block
	// rewards) and assembles the final block.
	//
	// Note: The block header and state database might be updated to reflect any
	// consensus rules that happen at finalization (e.g. block rewards).
	FinalizeAndAssemble(chain ChainHeaderReader, header *types.Header, state *state.StateDB, txs []*types.Transaction,
		uncles []*types.Header, receipts []*types.Receipt) (*types.Block, error)

	// Seal generates a new sealing request for the given input block and pushes
	// the result into the given channel.
	//
	// Note, the method returns immediately and will send the result async. More
	// than one result may also be returned depending on the consensus algorithm.
	Seal(chain ChainHeaderReader, block *types.Block, results chan<- *types.Block, stop <-chan struct{}) error

	// SealHash returns the hash of a block prior to it being sealed.
	SealHash(header *types.Header) common.Hash

	// CalcDifficulty is the difficulty adjustment algorithm. It returns the difficulty
	// that a new block should have.
	CalcDifficulty(chain ChainHeaderReader, time uint64, parent *types.Header) *big.Int

	// APIs returns the RPC APIs this consensus engine provides.
	APIs(chain ChainHeaderReader) []rpc.API

	// Close terminates any background threads maintained by the consensus engine.
	Close() error
}
~~~


- consensus mechanism
	- Ethereum relies on a Proof of Work-based consensus algorithm called _ethash_.
- patricia merkle trie
- smart contracts
- ethereum 2.0

[^1]: rust, javascript, ...
[^2]: Ethereum Testnet Rinkeby 4, Ethereum Testnet Ropsten 3, Ethereum Testnet Kovan 42, Ethereum Classic Mainnet 61. [A list of EVM networks](https://chainid.network/).
[^3]: By default the maximum number of connected peers is 50.
[^4]: Ethereum Virtual Machine (EVM) ...
[^5]: Ether has denominations ...  10^0 Wei 10^12 Szabo 10^15 Finney 10^18 Ether
[^6]: Turing-complete ...
[^7]: [Gas Costs from Yellow Paper -- EIP-150 Revision](https://docs.google.com/spreadsheets/d/1n6mRqkBz3iWcOlRem_mO09GtSKEKrAsfO7Frgx18pNU/edit#gid=0)
