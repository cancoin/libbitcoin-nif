#include <erl_nif.h>
#include <iostream>
#include <sstream>
#include <bitcoin/bitcoin.hpp>
#include <bitcoin/bitcoin/chain/transaction.hpp>
#include <bitcoin/bitcoin/config/btc256.hpp>
#include <bitcoin/bitcoin/formats/base16.hpp>

#include <assert.h>
#include "erl_nif.h"
#include "nifpp.h"

#pragma GCC diagnostic ignored "-Wwrite-strings"

using namespace bc;
using namespace libbitcoin::chain;
using namespace bc::config;
using namespace bc::wallet;

nifpp::TERM make_binary(ErlNifEnv* env, std::string str)
{
    nifpp::binary newbin(str.length());
    std::memcpy(newbin.data, str.c_str(), str.length());
    return nifpp::make(env, newbin);
}

data_chunk make_data_chunk(ErlNifEnv* env, ErlNifBinary *bin)
{

    std::vector<char> data(bin->data, bin->data + bin->size);
    return to_chunk(data);

}

ERL_NIF_TERM
erlang_libbitcoin_tx_decode(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    uint32_t flags = 0x00;
    chain::transaction tx;
    tx.from_data(make_data_chunk(env, &bin));
    std::map<nifpp::str_atom, nifpp::TERM> map_tx;
    long serialized_size = tx.serialized_size();
    ErlNifUInt64 total_output_value = tx.total_output_value();
    map_tx["version"] = nifpp::make(env, tx.version);
    map_tx["locktime"] = nifpp::make(env, tx.locktime);
    map_tx["hash"] = make_binary(env, btc256(tx.hash()).to_string());
    map_tx["coinbase"] = nifpp::make(env, tx.is_coinbase());
    map_tx["size"] = nifpp::make(env, serialized_size);
    map_tx["value"] = nifpp::make(env, total_output_value);
    std::list<nifpp::TERM> inputs;
    for (const auto input: tx.inputs) {
        std::map<nifpp::str_atom, nifpp::TERM> input_map;
        std::map<nifpp::str_atom, nifpp::TERM> prev_input_map;
        prev_input_map["hash"] = make_binary(env, btc256(input.previous_output.hash).to_string());
        prev_input_map["index"] = nifpp::make(env, input.previous_output.index);
        input_map["previous_output"] = nifpp::make(env, prev_input_map);
        const auto script_address = payment_address::extract(input.script, payment_address::mainnet_p2kh, payment_address::mainnet_p2sh);
        if (script_address)
            input_map["address"] = make_binary(env, script_address.encoded());
        input_map["script_asm"] = make_binary(env, chain::script(input.script).to_string(flags));
        input_map["script"] = make_binary(env, encode_base16(chain::script(input.script).to_data(false)));
        input_map["sequence"] = nifpp::make(env, input.sequence);
        inputs.push_back(nifpp::make(env, input_map));
    }
    map_tx["inputs"] = nifpp::make(env, inputs);
    std::list<nifpp::TERM> outputs;
    for (const auto output: tx.outputs) {
        std::map<nifpp::str_atom, nifpp::TERM> output_map;
        const auto script_address = payment_address::extract(output.script, payment_address::mainnet_p2kh, payment_address::mainnet_p2sh);
        ErlNifUInt64 output_value = output.value;
        ErlNifUInt64 output_serialized_size = output.serialized_size();
        if (script_address)
            output_map["address"] = make_binary(env, script_address.encoded());
        output_map["script_asm"] = make_binary(env, output.script.to_string(flags));
        output_map["script"] = make_binary(env, encode_base16(chain::script(output.script).to_data(false)));
        output_map["value"] = nifpp::make(env, output_value);
        output_map["size"] = nifpp::make(env, output_serialized_size);
        outputs.push_back(nifpp::make(env, output_map));
    }
    map_tx["outputs"] = nifpp::make(env, outputs);
    nifpp::TERM term = nifpp::make(env, map_tx);
    return term;
}

ERL_NIF_TERM
erlang_libbitcoin_header_decode(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    chain::header header;
    header.from_data(make_data_chunk(env, &bin), false);
    auto hash = encode_hash(header.hash());
    auto previous_block_hash = encode_hash(header.previous_block_hash);
    std::map<nifpp::str_atom, nifpp::TERM> map_header;
    long serialized_size = header.serialized_size();
    map_header["size"] = nifpp::make(env, serialized_size);
    map_header["version"] = nifpp::make(env, header.version);
    map_header["hash"] = make_binary(env, hash);
    map_header["merkle"] = make_binary(env, encode_hash(header.merkle));
    map_header["timestamp"] = nifpp::make(env, header.timestamp);
    map_header["bits"] = nifpp::make(env, header.bits);
    map_header["nonce"] = nifpp::make(env, header.nonce);
    map_header["previous_block_hash"] = make_binary(env, previous_block_hash);
    nifpp::TERM term = nifpp::make(env, map_header);
    return term;
}

ERL_NIF_TERM
erlang_tx_signature_hash(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
    ErlNifBinary raw_tx;
    unsigned long index;
    ErlNifBinary raw_script;
    unsigned long hash_type;

    if (!enif_inspect_binary(env, argv[0], &raw_tx)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_ulong(env, argv[1], &index)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[2], &raw_script)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_ulong(env, argv[3], &hash_type)) {
        return enif_make_badarg(env);
    }

    chain::transaction tx;
    tx.from_data(make_data_chunk(env, &raw_tx));
    auto script_data = make_data_chunk(env, &raw_script);

    chain::script script_code =
      chain::script::factory_from_data(script_data, false, chain::script::parse_mode::raw_data);

    hash_digest signature_hash =
      script::generate_signature_hash(tx, index, script_code, hash_type);

    return nifpp::make(env, signature_hash);
}

static ErlNifFunc nif_funcs[] = {
    {"tx_decode", 1, erlang_libbitcoin_tx_decode, 0},
    {"header_decode", 1, erlang_libbitcoin_header_decode, 0},
    {"tx_signature_hash", 4, erlang_tx_signature_hash, 0}
};

ERL_NIF_INIT(libbitcoin, nif_funcs, NULL, NULL, NULL, NULL);

