---
title: Understanding Ethereum by studying the source code
tags: ethereum
withtoc: true
---

# From papers to code

The [Ethereum Whitepaper](https://ethereum.org/en/whitepaper/) was published in 2013 and a year later some of the implementation details were expanded on in the [Ethereum Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf). Originally there were three implementations of the Ethereum protocol written in Go, C++, and Python[^1]. To understand the core concepts I decided to read both of the papers (again), clone the official [Go implementation](https://github.com/ethereum/go-ethereum) repo `commit 979fc9689 (tag: v1.9.20)`, and study the relationship between the ideas and the actual implementation. The [Ethereum Wiki](https://eth.wiki/) turned out to be a handy resource as well.

In blockchains users broadcast transactions using a peer-to-peer protocol to the entire network of nodes. The transactions remain in the node's transaction pool -- which acts as a memory queue -- until miners dequeue transactions, marshal them into a block and perform the consensus protocol which orders the blocks, proposed by different miners, in a canonical sequence. Before appending a block to the chain the nodes verify that its validity and that the chain forms an uninterupted sequence of blocks linked together by cryptographic hashes of the previous block, all the way from the genesis block.

# Networks and their genesis

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

# Connecting to the peer-to-peer network

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

`p2p/server.go`

~~~go
// run is the main loop of the server.
func (srv *Server) run() {
	srv.log.Info("Started P2P networking", "self", srv.localnode.Node().URLv4())
	defer srv.loopWG.Done()
	defer srv.nodedb.Close()
	defer srv.discmix.Close()
	defer srv.dialsched.stop()

	var (
		peers        = make(map[enode.ID]*Peer)
		inboundCount = 0
		trusted      = make(map[enode.ID]bool, len(srv.TrustedNodes))
	)
~~~

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

The discovery protocol relies on a Kademlia-like DHT that stores information about Ethereum nodes.  Participants in the Discovery Protocol are expected to maintain a ENR containing up-to-date information. To resolve the current record of any node public key, a DHT lookup is performed and when the node is found, the ENR is requested directly from the peer.


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

# Propogating blocks

Now that our node is connected to its peers it must download all blocks from genesis block to the latest block. The total difficulty is used to determine which peer has the heaviest chain. The node always verifies the proof-of-work values in the block headers and then can either recreate the state by downloading the transactions and executing them in the EVM[^4] or by downloading merkle tree nodes and contract code incrementally until the entire tree is synchronized. Subsequent blocks are propagated to all nodes in the network in two steps; first a new block is announced and nodes perform basic validation before relaying the announcement to a small fraction of connected peers then transactions contained in the block are executed and if valid a hash of the block is propagated to peers it didn't notify earlyer. Those peers may need to request the full block from a node they are connected to.

Ethereum blocks are produced on average every 15 seconds. This increases both throughput and shortens finality at the expense of a higher likelihood that two different blocks are proposed at the same hight in the blockchain. Uncles are stale blocks that contribute to the security of the chain but are not considered the canonical truth for that particular chain height. Instead of orphaning stale blocks, Ethereum pays for uncles adding to the security of the chain. The canonical chain is therefore not the neccesarily the longst chain but the heaviest when all work, including work performed by uncles, is counted.

The block header contains a number of interesting hashes which provide cryptographic proofs of data authenticity which has possibly been acquired from different peers. The parent hash is the hash of the block prior to the current block which is the keccak256 hash of the RLP encoded block header. The uncle hash and transactions hash are calculated in the same way on the encoded block headers of the uncles and the transactions, respectivily.

	Root        common.Hash    `json:"stateRoot"        gencodec:"required"`

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

Ethereum deviates from Bitcoin's state model. UTXO vs accounts

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
[^2]: Ethereum Testnet Rinkeby 4, Ethereum Testnet Ropsten 3, Ethereum Testnet Kovan 42, Ethereum Classic Mainnet 61. [A list of EVM networks](https://chainid.network/).
[^3]: By default the maximum number of connected peers is 50.
[^4]: Ethereum Virtual Machine (EVM) ...
