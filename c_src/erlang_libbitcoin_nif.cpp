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

std::string make_string(ErlNifEnv* env, ErlNifBinary *bin)
{
    std::string data(bin->data, bin->data + bin->size);
    return data;
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

/*
 *
    data_chunk tx_data;
    decode_base16(tx_data, "0100000001b3807042c92f449bbf79b33ca59d7dfec7f4cc71096704a9c526dddf496ee0970000000000ffffffff0000000000");
    chain::transaction new_tx;
    new_tx.from_data(tx_data);

    chain::script prevout_script;
    prevout_script.from_string("dup hash160 [ 88350574280395ad2c3e2ee20e322073d94e5e40 ] equalverify checksig");
    const uint32_t input_index = 0;
    const uint8_t sighash_type = chain::signature_hash_algorithm::all;
    const auto sighash = chain::script::generate_signature_hash(new_tx, input_index, prevout_script, sighash_type);
    const auto str = prevout_script.to_data(false);
    const auto result = encode_base16(str);
   return nifpp::make(env, make_binary(env, result));

 */

ERL_NIF_TERM
erlang_script_decode(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin_script;
    chain::script script;

    if (!enif_inspect_binary(env, argv[0], &bin_script)) {
        return enif_make_badarg(env);
    }

    auto script_chunk = make_data_chunk(env, &bin_script);
    script.from_data(script_chunk, false, chain::script::parse_mode::strict);

    return make_binary(env, script.to_string(0xffffffff));
}

ERL_NIF_TERM
erlang_script_encode(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin_script;
    chain::script script;

    if (!enif_inspect_binary(env, argv[0], &bin_script)) {
        return enif_make_badarg(env);
    }

    auto script_str = make_string(env, &bin_script);

    if (!script.from_string(script_str)) {
        return enif_make_badarg(env);
    };

    return make_binary(env, encode_base16(script.to_data(false)));
}

ERL_NIF_TERM
erlang_script_to_address(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin_script;
    unsigned int version;
    chain::script script;

    if (!enif_inspect_binary(env, argv[0], &bin_script)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[1], &version)) {
        return enif_make_badarg(env);
    }

    auto script_chunk = make_data_chunk(env, &bin_script);
    script.from_data(script_chunk, false, chain::script::parse_mode::strict);
    const wallet::payment_address address(script, version);

    return make_binary(env, address.encoded());
}


ERL_NIF_TERM
erlang_input_sign(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
    ErlNifBinary raw_tx;
    unsigned long index;
    ErlNifBinary raw_script;
    unsigned int hash_type;

    if (!enif_inspect_binary(env, argv[0], &raw_tx)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_ulong(env, argv[1], &index)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[2], &raw_script)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[3], &hash_type)) {
        return enif_make_badarg(env);
    }

    chain::transaction tx;
    tx.from_data(make_data_chunk(env, &raw_tx));

    auto script_chunk = make_data_chunk(env, &raw_script);

    chain::script script_code;
    script_code.from_data(script_chunk, false, chain::script::parse_mode::raw_data);

    hash_digest hash =
      script::generate_signature_hash(tx, index, script_code, hash_type);

    return make_binary(env, encode_base16(hash));
}


ERL_NIF_TERM
erlang_input_signature_hash(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
    ErlNifBinary raw_tx;
    unsigned long index;
    ErlNifBinary raw_script;
    unsigned int hash_type;

    if (!enif_inspect_binary(env, argv[0], &raw_tx)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_ulong(env, argv[1], &index)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[2], &raw_script)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[3], &hash_type)) {
        return enif_make_badarg(env);
    }

    chain::transaction tx;
    tx.from_data(make_data_chunk(env, &raw_tx));

    auto script_chunk = make_data_chunk(env, &raw_script);

    chain::script script_code;
    script_code.from_data(script_chunk, false, chain::script::parse_mode::raw_data);

    hash_digest hash =
      script::generate_signature_hash(tx, index, script_code, hash_type);

    return make_binary(env, encode_base16(hash));
}

static ErlNifFunc nif_funcs[] = {
    {"tx_decode", 1, erlang_libbitcoin_tx_decode, 0},
    {"header_decode", 1, erlang_libbitcoin_header_decode, 0},
    {"script_decode", 1, erlang_script_decode, 0},
    {"script_encode", 1, erlang_script_encode, 0},
    {"script_to_address", 2, erlang_script_to_address, 0},
    {"input_signature_hash", 4, erlang_input_signature_hash, 0}
};

ERL_NIF_INIT(libbitcoin, nif_funcs, NULL, NULL, NULL, NULL);

