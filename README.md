libbitcoin
=====

An (very basic) Erlang NIF for Libbitcoin

Requirements
------------

Erlang 17.3+ (for dirty scheduler)
Libbitcoin and all it's dependencies: https://github.com/libbitcoin/libbitcoin

Build
-----

    $ make
    $ make test
    
If you installed libbitcoin with a non-standard prefix you must specify it's pkg-config path

    $ PKG_CONFIG_PATH=/opt/libbitcoin/pkgconfig make


Roadmap
-------

Right now this library only has one function which decodes transactions for bitcoin mainnet. In the future it would be nice if this library were to include more functions following libbitcoin-explorer's command set, or at least the commands that are well suited for NIF's. For the slower executing functions it will be important to use Erlang's dirty thread scheduler.

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
[ ] message_sign
[ ] message_validate
[ ] mnemonic_new
[ ] mnemonic_to_seed
[ ] script_decode
[ ] script_encode
[ ] script_to_address
[ ] seed
[ ] stealth_decode
[ ] stealth_encode
[ ] stealth_public
[ ] stealth_secret
[ ] stealth_shared
[✓] tx_decode
[ ] tx_decode
[ ] tx_encode
[ ] tx_sign
[ ] wif_to_ec
[ ] wif_to_public
