-module(libbitcoin_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.

hexstr_to_bin(S) ->
    list_to_binary(hexstr_to_list(S)).

hexstr_to_list([X,Y|T]) ->
    [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
    [].

-define(raw_tx_p2sh_out, hexstr_to_bin("010000000189632848f99722915727c5c75da8db2dbf194342a0429828f66ff88fab2af7d6000000008b483045022100abbc8a73fe2054480bda3f3281da2d0c51e2841391abd4c09f4f908a2034c18d02205bc9e4d68eafb918f3e9662338647a4419c0de1a650ab8983f1d216e2a31d8e30141046f55d7adeff6011c7eac294fe540c57830be80e9355c83869c9260a4b8bf4767a66bacbd70b804dc63d5beeb14180292ad7f3b083372b1d02d7a37dd97ff5c9effffffff0140420f000000000017a914f815b036d9bbbce5e9f2a00abd1bf3dc91e955108700000000")).
-define(raw_tx_p2sh_in, hexstr_to_bin("0100000001aca7f3b45654c230e0886a57fb988c3044ef5e8f7f39726d305c61d5e818903c00000000fd5d010048304502200187af928e9d155c4b1ac9c1c9118153239aba76774f775d7c1f9c3e106ff33c0221008822b0f658edec22274d0b6ae9de10ebf2da06b1bbdaaba4e50eb078f39e3d78014730440220795f0f4f5941a77ae032ecb9e33753788d7eb5cb0c78d805575d6b00a1d9bfed02203e1f4ad9332d1416ae01e27038e945bc9db59c732728a383a6f1ed2fb99da7a4014cc952410491bba2510912a5bd37da1fb5b1673010e43d2c6d812c514e91bfa9f2eb129e1c183329db55bd868e209aac2fbc02cb33d98fe74bf23f0c235d6126b1d8334f864104865c40293a680cb9c020e7b1e106d8c1916d3cef99aa431a56d253e69256dac09ef122b1a986818a7cb624532f062c1d1f8722084861c5c3291ccffef4ec687441048d2455d2403e08708fc1f556002f1b6cd83f992d085097f9974ab08a28838f07896fbab08f39495e15fa6fad6edbfb1e754e35fa1c7844c41f322a1863d4621353aeffffffff0140420f00000000001976a914ae56b4db13554d321c402db3961187aed1bbed5b88ac00000000")).
-define(header, hexstr_to_bin("04000000ccb2c6074ab460b37cc756567630e996bcd0ed452ac56e060000000000000000974eecbb5824853cdda389433fa4e16a89e5daef2670be442e94aa083ead3f4d2f71ad56f02809185acd606b")).

-define(tx, hexstr_to_bin("0100000002dc38e9359bd7da3b58386204e186d9408685f427f5e513666db735aa8a6b2169000000006a47304402205d8feeb312478e468d0b514e63e113958d7214fa572acd87079a7f0cc026fc5c02200fa76ea05bf243af6d0f9177f241caf606d01fcfd5e62d6befbca24e569e5c27032102100a1a9ca2c18932d6577c58f225580184d0e08226d41959874ac963e3c1b2feffffffffdc38e9359bd7da3b58386204e186d9408685f427f5e513666db735aa8a6b2169010000006b4830450220087ede38729e6d35e4f515505018e659222031273b7366920f393ee3ab17bc1e022100ca43164b757d1a6d1235f13200d4b5f76dd8fda4ec9fc28546b2df5b1211e8df03210275983913e60093b767e85597ca9397fb2f418e57f998d6afbbc536116085b1cbffffffff0140899500000000001976a914fcc9b36d38cf55d7d5b4ee4dddb6b2c17612f48c88ac00000000")).
-define(script, hexstr_to_bin("76a91433cef61749d11ba2adf091a5e045678177fe3a6d88ac")).

-define(TXIN, #{
  hash => hexstr_to_bin("97e06e49dfdd26c5a904670971ccf4c7fe7d9da53cb379bf9b442fc9427080b3"),
  index => 0
}).

-define(TXOUT_KH, #{
  address => <<"1966U1pjj15tLxPXZ19U48c99EJDkdXeqb">>,
  amount => 30000
}).

-define(TXOUT_SH, #{
  address => <<"3QJmV3qfvL9SuYo34YihAf3sRCW3qSinyC">>,
  amount => 5000
}).

-define(TXOUT_STEALTH, #{
  address => <<"vJmtuUb8ysKiM1HtHQF23FGfjGAKu5sM94UyyjknqhJHNdj5CZzwtpGzeyaATQ2HvuzomNVtiwsTJSWzzCBgCTtUZbRFpzKVq9MAUr">>,
  amount => 10000,
  seed => <<"deadbeefdeadbeef">>
}).

-define(TX, <<"010000000197e06e49dfdd26c5a904670971ccf4c7fe7d9da53cb379bf9b442fc9427080b30000000000ffffffff0430750000000000001976a91458b7a60f11a904feef35a639b6048de8dd4d9f1c88ac881300000000000017a914f815b036d9bbbce5e9f2a00abd1bf3dc91e95510870000000000000000456a43634c21ae4c49698643d947eee9d61e4942c94db8ad2a685bc62fd9cdc4071aed2338705c3d1dd43a6e06c7dacace1a929a7ceec44473af1c7b3700df891cdd8ab6a7fe10270000000000001976a91425b69aa1cbd426586f5b1662a36e924acb01b5fd88ac00000000">>).

-define(SIGHASH_TX, hexstr_to_bin("0100000001b3807042c92f449bbf79b33ca59d7dfec7f4cc71096704a9c526dddf496ee0970000000000ffffffff0000000000")).
-define(SIGHASH_INDEX, 0).
-define(SIGHASH_SCRIPT, hexstr_to_bin("76a91488350574280395ad2c3e2ee20e322073d94e5e4088ac")).
-define(SIGHASH_SCRIPT_HEX, list_to_binary("76a91488350574280395ad2c3e2ee20e322073d94e5e4088ac")).
-define(SIGHASH_SCRIPT_TEXT, list_to_binary("dup hash160 [88350574280395ad2c3e2ee20e322073d94e5e40] equalverify checksig")).
-define(SIGHASH_TYPE, 1).
-define(SIGHASH, list_to_binary("f89572635651b2e4f89778350616989183c98d1a721c911324bf9f17a0cf5bf0")).

-define(TWOOFTHREE_SCRIPT, hexstr_to_bin("5221022df8750480ad5b26950b25c7ba79d3e37d75f640f8e5d9bcd5b150a0f85014da2103e3818b65bcc73a7d64064106a859cc1a5a728c4345ff0b641209fba0d90de6e921021f2f6e1e50cb6a953935c3601284925decd3fd21bc445712576873fb8c6ebc1853ae")).
-define(TWOOFTHREE_SCRIPT_HEX, list_to_binary("5221022df8750480ad5b26950b25c7ba79d3e37d75f640f8e5d9bcd5b150a0f85014da2103e3818b65bcc73a7d64064106a859cc1a5a728c4345ff0b641209fba0d90de6e921021f2f6e1e50cb6a953935c3601284925decd3fd21bc445712576873fb8c6ebc1853ae")).
-define(TWOOFTHREE_SCRIPT_TEXT, list_to_binary("2 [022df8750480ad5b26950b25c7ba79d3e37d75f640f8e5d9bcd5b150a0f85014da] [03e3818b65bcc73a7d64064106a859cc1a5a728c4345ff0b641209fba0d90de6e9] [021f2f6e1e50cb6a953935c3601284925decd3fd21bc445712576873fb8c6ebc18] 3 checkmultisig")).
-define(TWOOFTHREE_ADDRESS, <<"3AdQeG1eNTKxT2VKeddnZxdQbU5PeCfuk6">>).

tx_decode_p2sh_out_test() ->
  #{inputs := Inputs, outputs := Outputs,
    coinbase := Coinbase, hash := Hash,
    size := Size, value := Value,
    version := Version} = libbitcoin:tx_decode(?raw_tx_p2sh_out),
  ?assertEqual(Coinbase, false),
  ?assertEqual(Hash, <<"3c9018e8d5615c306d72397f8f5eef44308c98fb576a88e030c25456b4f3a7ac">>),
  ?assertEqual(Size, 222),
  ?assertEqual(Value, 1000000),
  ?assertEqual(Version, 1),
  ?assertEqual(Coinbase, false),
  
  ?assertEqual(Inputs,[#{address => <<"18pV61UrtyK9YW8tDa53UkM8DDbFWKiwvc">>,
                         previous_output => #{
                           hash => <<"d6f72aab8ff86ff6289842a0424319bf2ddba85dc7c52757912297f948286389">>,
                           index => 0},
                         script => <<"483045022100abbc8a73fe2054480bda3f3281da2d0c51e2841391abd4c09f4f908a2034c18d02205bc9e4d68eafb918f3e9662338647a4419c0de1a650ab8983f1d216e2a31d8e30141046f55d7adeff6011c7eac294fe540c57830be80e9355c83869c9260a4b8bf4767a66bacbd70b804dc63d5beeb14180292ad7f3b083372b1d02d7a37dd97ff5c9e">>,
                         script_asm => <<"[3045022100abbc8a73fe2054480bda3f3281da2d0c51e2841391abd4c09f4f908a2034c18d02205bc9e4d68eafb918f3e9662338647a4419c0de1a650ab8983f1d216e2a31d8e301] [046f55d7adeff6011c7eac294fe540c57830be80e9355c83869c9260a4b8bf4767a66bacbd70b804dc63d5beeb14180292ad7f3b083372b1d02d7a37dd97ff5c9e]">>,
                         sequence => 4294967295}]),
  ?assertEqual(Outputs, [#{address => <<"3QJmV3qfvL9SuYo34YihAf3sRCW3qSinyC">>,
                           script => <<"a914f815b036d9bbbce5e9f2a00abd1bf3dc91e9551087">>,
                           script_asm => <<"hash160 [f815b036d9bbbce5e9f2a00abd1bf3dc91e95510] equal">>,
                           size => 32,
                           value => 1000000}]).

tx_decode_p2sh_in_test() ->
  #{inputs := Inputs, outputs := Outputs,
    coinbase := Coinbase, hash := Hash,
    size := Size, value := Value,
    version := Version} = libbitcoin:tx_decode(?raw_tx_p2sh_in),
  ?assertEqual(Coinbase, false),
  ?assertEqual(Hash, <<"837dea37ddc8b1e3ce646f1a656e79bbd8cc7f558ac56a169626d649ebe2a3ba">>),
  ?assertEqual(Size, 436),
  ?assertEqual(Value, 1000000),
  ?assertEqual(Version, 1),
  ?assertEqual(Coinbase, false),
  
  ?assertEqual(Inputs,[#{previous_output => #{
                           hash => <<"3c9018e8d5615c306d72397f8f5eef44308c98fb576a88e030c25456b4f3a7ac">>,
                           index => 0},
                         script => <<"0048304502200187af928e9d155c4b1ac9c1c9118153239aba76774f775d7c1f9c3e106ff33c0221008822b0f658edec22274d0b6ae9de10ebf2da06b1bbdaaba4e50eb078f39e3d78014730440220795f0f4f5941a77ae032ecb9e33753788d7eb5cb0c78d805575d6b00a1d9bfed02203e1f4ad9332d1416ae01e27038e945bc9db59c732728a383a6f1ed2fb99da7a4014cc952410491bba2510912a5bd37da1fb5b1673010e43d2c6d812c514e91bfa9f2eb129e1c183329db55bd868e209aac2fbc02cb33d98fe74bf23f0c235d6126b1d8334f864104865c40293a680cb9c020e7b1e106d8c1916d3cef99aa431a56d253e69256dac09ef122b1a986818a7cb624532f062c1d1f8722084861c5c3291ccffef4ec687441048d2455d2403e08708fc1f556002f1b6cd83f992d085097f9974ab08a28838f07896fbab08f39495e15fa6fad6edbfb1e754e35fa1c7844c41f322a1863d4621353ae">>,
                         script_asm => <<"zero [304502200187af928e9d155c4b1ac9c1c9118153239aba76774f775d7c1f9c3e106ff33c0221008822b0f658edec22274d0b6ae9de10ebf2da06b1bbdaaba4e50eb078f39e3d7801] [30440220795f0f4f5941a77ae032ecb9e33753788d7eb5cb0c78d805575d6b00a1d9bfed02203e1f4ad9332d1416ae01e27038e945bc9db59c732728a383a6f1ed2fb99da7a401] [52410491bba2510912a5bd37da1fb5b1673010e43d2c6d812c514e91bfa9f2eb129e1c183329db55bd868e209aac2fbc02cb33d98fe74bf23f0c235d6126b1d8334f864104865c40293a680cb9c020e7b1e106d8c1916d3cef99aa431a56d253e69256dac09ef122b1a986818a7cb624532f062c1d1f8722084861c5c3291ccffef4ec687441048d2455d2403e08708fc1f556002f1b6cd83f992d085097f9974ab08a28838f07896fbab08f39495e15fa6fad6edbfb1e754e35fa1c7844c41f322a1863d4621353ae]">>,
                         sequence => 4294967295}]),
  ?assertEqual(Outputs, [#{address => <<"1GtpSrGhRGY5kkrNz4RykoqRQoJuG2L6DS">>,
                           script => <<"76a914ae56b4db13554d321c402db3961187aed1bbed5b88ac">>,
                           script_asm => <<"dup hash160 [ae56b4db13554d321c402db3961187aed1bbed5b] equalverify checksig">>,
                           size => 34,
                           value => 1000000}]).


tx_encode_test() ->
  tx_encode_test(#{
    inputs => [?TXIN],
    outputs => [?TXOUT_KH, ?TXOUT_SH, ?TXOUT_STEALTH],
    locktime => 0,
    sequence => 16#FFFFFFFF,
    tx_version => 1,
    script_version => 5
  }),
  tx_encode_test(#{
    inputs => [?TXIN],
    outputs => [?TXOUT_KH, ?TXOUT_SH, ?TXOUT_STEALTH]
  }),
  ok.

tx_encode_test(TxObj) ->
  Tx = libbitcoin:tx_encode(TxObj),
  ?assertEqual(?TX, Tx),
  ok.

header_decode_test() ->
  ?assertEqual(#{bits => 403253488,
                 hash => <<"00000000000000000495ef1dede300947fdb51a4bad6f85a9e113cc6f3d241d9">>,
                 merkle => <<"4d3fad3e08aa942e44be7026efdae5896ae1a43f4389a3dd3c852458bbec4e97">>,
                 nonce => 1801506138,
                 previous_block_hash => <<"0000000000000000066ec52a45edd0bc96e930765656c77cb360b44a07c6b2cc">>,
                 size => 80,
                 timestamp => 1454207279,
                 version => 4}, libbitcoin:header_decode(?header)),
  ok.

script_decode_test() ->
  ?assertEqual(?SIGHASH_SCRIPT_TEXT, libbitcoin:script_decode(?SIGHASH_SCRIPT)),
  ?assertEqual(?TWOOFTHREE_SCRIPT_TEXT, libbitcoin:script_decode(?TWOOFTHREE_SCRIPT)),
  ok.

script_encode_test() ->
  ?assertEqual(?SIGHASH_SCRIPT_HEX, libbitcoin:script_encode(?SIGHASH_SCRIPT_TEXT)),
  ?assertEqual(?TWOOFTHREE_SCRIPT_HEX, libbitcoin:script_encode(?TWOOFTHREE_SCRIPT_TEXT)),
  ok.

script_to_address_test() ->
  ?assertEqual(?TWOOFTHREE_ADDRESS, libbitcoin:script_to_address(?TWOOFTHREE_SCRIPT, 5)),
  ok.

input_signature_hash_test() ->
  Encoded = libbitcoin:input_signature_hash(?SIGHASH_TX, ?SIGHASH_INDEX, ?SIGHASH_SCRIPT, ?SIGHASH_TYPE),
  ?assertEqual(?SIGHASH, Encoded),
  ok.

spend_checksum_test() ->
  ?assertEqual(16#FFFFFFFFFFFFFFFF, libbitcoin:spend_checksum(hexstr_to_bin("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"), 16#FFFFFFFF)),
  ?assertEqual(0, libbitcoin:spend_checksum(hexstr_to_bin("0000000000000000000000000000000000000000000000000000000000000000"), 0)),
  ?assertEqual(16#AAAAAAAAAAAA8001, libbitcoin:spend_checksum(hexstr_to_bin("000000000000000000000000aaaaaaaaaaaaaaaa000000000000000000000000"), 1)),
  ?assertEqual(16#1234567AAAACDEF, libbitcoin:spend_checksum(hexstr_to_bin("ffffffffffffffffffffffff01234567aaaaaaaaffffffffffffffffffffffff"), 16#89ABCDEF)),
  ok.


-endif.
