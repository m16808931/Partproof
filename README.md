# 🔧 Partproof - Spare Part Authenticity Registry

> 🛡️ **Counterfeit prevention for auto parts on the blockchain**

Partproof is a Clarity smart contract that creates an immutable registry for automotive spare parts, enabling manufacturers, dealers, and consumers to verify part authenticity and track ownership history.

## 🚀 Features

- ✅ **Manufacturer Registration** - Verified manufacturer onboarding
- 🏷️ **Part Registration** - Unique part identification with cryptographic hashes
- 🔍 **Authenticity Verification** - Cryptographic proof of genuine parts
- 📋 **Ownership Tracking** - Complete chain of custody
- 🔒 **Anti-Counterfeiting** - Immutable proof of authenticity
- 📊 **Part History** - Full lifecycle tracking

## 🛠️ Installation

```bash
clarinet new partproof-project
cd partproof-project
```

Copy the contract code into `contracts/Partproof.clar`

## 📖 Usage

### For Contract Owner

**Register a Manufacturer:**
```clarity
(contract-call? .Partproof register-manufacturer "Bosch Automotive")
```

**Update Manufacturer Status:**
```clarity
(contract-call? .Partproof update-manufacturer-status 'SP1234... true)
```

### For Manufacturers

**Register a Part:**
```clarity
(contract-call? .Partproof register-part 
    "BP-12345" 
    "Brake Pad Set Front Axle" 
    "BATCH-2024-001" 
    u1704067200 
    0x1234567890abcdef...)
```

### For Verifiers/Consumers

**Verify Part Authenticity:**
```clarity
(contract-call? .Partproof verify-part 
    u1 
    0x1234567890abcdef... 
    u95 
    "Authentic part verified")
```

**Check if Part is Authentic:**
```clarity
(contract-call? .Partproof is-part-authentic u1 0x1234567890abcdef...)
```

### For Part Owners

**Transfer Part Ownership:**
```clarity
(contract-call? .Partproof transfer-part u1 'SP5678...)
```

## 🔍 Read-Only Functions

- `get-part-info` - Get complete part information
- `get-part-ownership` - Get ownership details
- `get-manufacturer-info` - Get manufacturer details
- `get-part-verification` - Get verification status
- `get-part-history` - Get complete part history
- `is-manufacturer-verified` - Check manufacturer status

## 🏗️ Contract Architecture

### Data Structures

- **Parts Registry** - Core part information with cryptographic hashes
- **Manufacturer Registry** - Verified manufacturer database
- **Ownership Chain** - Complete ownership history
- **Verification Records** - Authenticity verification logs

### Security Features

- 🔐 Owner-only manufacturer registration
- 🔑 Cryptographic hash verification
- 🚫 Duplicate part prevention
- ✋ Unauthorized access protection

## 🧪 Testing

```bash
clarinet test
```

## 🚀 Deployment

```bash
clarinet deploy --testnet
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

MIT License - see LICENSE file for details


