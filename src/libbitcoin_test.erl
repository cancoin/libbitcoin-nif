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
                         script_asm => <<"[ 3045022100abbc8a73fe2054480bda3f3281da2d0c51e2841391abd4c09f4f908a2034c18d02205bc9e4d68eafb918f3e9662338647a4419c0de1a650ab8983f1d216e2a31d8e301 ] [ 046f55d7adeff6011c7eac294fe540c57830be80e9355c83869c9260a4b8bf4767a66bacbd70b804dc63d5beeb14180292ad7f3b083372b1d02d7a37dd97ff5c9e ]">>,
                         sequence => 4294967295}]),
  ?assertEqual(Outputs, [#{address => <<"3QJmV3qfvL9SuYo34YihAf3sRCW3qSinyC">>,
                           script => <<"a914f815b036d9bbbce5e9f2a00abd1bf3dc91e9551087">>,
                           script_asm => <<"hash160 [ f815b036d9bbbce5e9f2a00abd1bf3dc91e95510 ] equal">>,
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
                         script_asm => <<"zero [ 304502200187af928e9d155c4b1ac9c1c9118153239aba76774f775d7c1f9c3e106ff33c0221008822b0f658edec22274d0b6ae9de10ebf2da06b1bbdaaba4e50eb078f39e3d7801 ] [ 30440220795f0f4f5941a77ae032ecb9e33753788d7eb5cb0c78d805575d6b00a1d9bfed02203e1f4ad9332d1416ae01e27038e945bc9db59c732728a383a6f1ed2fb99da7a401 ] [ 52410491bba2510912a5bd37da1fb5b1673010e43d2c6d812c514e91bfa9f2eb129e1c183329db55bd868e209aac2fbc02cb33d98fe74bf23f0c235d6126b1d8334f864104865c40293a680cb9c020e7b1e106d8c1916d3cef99aa431a56d253e69256dac09ef122b1a986818a7cb624532f062c1d1f8722084861c5c3291ccffef4ec687441048d2455d2403e08708fc1f556002f1b6cd83f992d085097f9974ab08a28838f07896fbab08f39495e15fa6fad6edbfb1e754e35fa1c7844c41f322a1863d4621353ae ]">>,
                         sequence => 4294967295}]),
  ?assertEqual(Outputs, [#{address => <<"1GtpSrGhRGY5kkrNz4RykoqRQoJuG2L6DS">>,
                           script => <<"76a914ae56b4db13554d321c402db3961187aed1bbed5b88ac">>,
                           script_asm => <<"dup hash160 [ ae56b4db13554d321c402db3961187aed1bbed5b ] equalverify checksig">>,
                           size => 32,
                           value => 1000000}]).

-endif.
