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


-define(raw_tx, hexstr_to_bin("010000000189632848f99722915727c5c75da8db2dbf194342a0429828f66ff88fab2af7d6000000008b483045022100abbc8a73fe2054480bda3f3281da2d0c51e2841391abd4c09f4f908a2034c18d02205bc9e4d68eafb918f3e9662338647a4419c0de1a650ab8983f1d216e2a31d8e30141046f55d7adeff6011c7eac294fe540c57830be80e9355c83869c9260a4b8bf4767a66bacbd70b804dc63d5beeb14180292ad7f3b083372b1d02d7a37dd97ff5c9effffffff0140420f000000000017a914f815b036d9bbbce5e9f2a00abd1bf3dc91e955108700000000")).

tx_decode_test() ->
  #{inputs := Inputs, outputs := Outputs,
    coinbase := Coinbase, hash := Hash,
    size := Size, value := Value,
    version := Version} = libbitcoin:tx_decode(?raw_tx),
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
                         script => <<"8b483045022100abbc8a73fe2054480bda3f3281da2d0c51e2841391abd4c09f4f908a2034c18d02205bc9e4d68eafb918f3e9662338647a4419c0de1a650ab8983f1d216e2a31d8e30141046f55d7adeff6011c7eac294fe540c57830be80e9355c83869c9260a4b8bf4767a66bacbd70b804dc63d5beeb14180292ad7f3b083372b1d02d7a37dd97ff5c9e">>,
                         sequence => 4294967295}]),
  ?assertEqual(Outputs, [#{address => <<"3QJmV3qfvL9SuYo34YihAf3sRCW3qSinyC">>,
                           script => <<"17a914f815b036d9bbbce5e9f2a00abd1bf3dc91e9551087">>,
                           script_asm => <<"hash160 [ f815b036d9bbbce5e9f2a00abd1bf3dc91e95510 ] equal">>,
                           size => 32,
                           value => 1000000}]).

-endif.
