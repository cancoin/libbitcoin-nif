-module(libbitcoin).
-export([tx_decode/1, header_decode/1,
         script_decode/1, script_encode/1, script_to_address/2,
         input_signature_hash/4]).

-on_load(init/0).

-define(APPNAME, libbitcoin).
-define(LIBNAME, 'libbitcoin-nif').

tx_decode(_RawTx) ->
    not_loaded(?LINE).

header_decode(_RawHeader) ->
    not_loaded(?LINE).

input_signature_hash(_RawTx, _Index, _Script, _HashType) ->
    not_loaded(?LINE).

script_decode(_Script) ->
    not_loaded(?LINE).

script_encode(_Script) ->
    not_loaded(?LINE).

script_to_address(_Script, _Version) ->
    not_loaded(?LINE).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

