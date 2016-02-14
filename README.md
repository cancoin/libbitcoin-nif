libbitcoin
=====

An (very basic) Erlang NIF for Libbitcoin

Requirements
------------

* Erlang 17.3+ (for dirty thread scheduler)

Build
-----

On Debian/Ubuntu install libbitcoin's build dependancies with apt

    $ sudo apt-get install build-essential autoconf automake libtool pkg-config libboost-all-dev


On OSX install libbitcoin's build dependancies with homebrew

    $ brew install autoconf automake libtool pkgconfig wget boost


Running `make` will download and install libbitcoin and it's dependancies, build
them as shared libraries, then copy them to the `priv` directory.

    $ make


You must set LD_LIBRARY_PATH to include to this application's priv directory

    $ LD_LIBRARY_PATH=`pwd`/priv make test


Roadmap
-------

Right now this library only has one function which decodes transactions for bitcoin mainnet. In the future it would be nice if this library were to include more functions following libbitcoin-explorer's command set, or at least the commands that are well suited for NIF's. For the slower executing functions it will be important to use Erlang's dirty thread scheduler.

```
[ ] address_decode
[ ] address_embed
[ ] address_encode
[ ] address_validate
[ ] ec_add
[ ] ec_add_secrets
[ ] ec_multiply
[ ] ec_multiply_secrets
[ ] ec_new
[ ] ec_to_address
[ ] ec_to_public
[ ] ec_to_wif
[ ] ec_unlock
[ ] hd_new
[ ] hd_private
[ ] hd_public
[ ] hd_to_address
[ ] hd_to_ec
[ ] hd_to_public
[ ] hd_to_wif
[ ] input_set
[ ] input_sign
[ ] input_validate
[✓] input_signature_hash
[ ] message_sign
[ ] message_validate
[ ] mnemonic_new
[ ] mnemonic_to_seed
[✓] script_decode
[✓] script_encode
[✓] script_to_address
[ ] seed
[ ] stealth_decode
[ ] stealth_encode
[ ] stealth_public
[ ] stealth_secret
[ ] stealth_shared
[✓] tx_decode
[ ] tx_encode
[ ] wif_to_ec
[ ] wif_to_public
```
