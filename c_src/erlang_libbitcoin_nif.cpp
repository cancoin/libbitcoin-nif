#include <erl_nif.h>
#include <iostream>
#include <sstream>
#include <bitcoin/bitcoin.hpp>
#include <bitcoin/bitcoin/chain/transaction.hpp>
#include <bitcoin/bitcoin/config/hash256.hpp>
#include <bitcoin/bitcoin/formats/base_16.hpp>

#include <assert.h>
#include "erl_nif.h"
#include "nifpp.h"

#ifdef ERL_NIF_DIRTY_JOB_CPU_BOUND
#define LIBBITCOIN_NIF_FLAGS ERL_NIF_DIRTY_JOB_CPU_BOUND
#else
#define LIBBITCOIN_NIF_FLAGS 0
#endif

using namespace bc;
using namespace bc::chain;
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
    map_tx["hash"] = make_binary(env, hash256(tx.hash()).to_string());
    map_tx["coinbase"] = nifpp::make(env, tx.is_coinbase());
    map_tx["size"] = nifpp::make(env, serialized_size);
    map_tx["value"] = nifpp::make(env, total_output_value);
    std::list<nifpp::TERM> inputs;
    for (const auto input: tx.inputs) {
        std::map<nifpp::str_atom, nifpp::TERM> input_map;
        std::map<nifpp::str_atom, nifpp::TERM> prev_input_map;
        prev_input_map["hash"] = make_binary(env, hash256(input.previous_output.hash).to_string());
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
erlang_libbitcoin_tx_encode(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
    std::map<nifpp::str_atom, nifpp::TERM> tx_map;

    std::vector<nifpp::TERM> input_terms;
    std::vector<nifpp::TERM> output_terms;

    std::vector<chain::input> inputs;
    std::vector<chain::output> outputs;

    unsigned long locktime;
    int tx_version;
    int script_version;

    if (!nifpp::get(env, argv[0], tx_map))
       return enif_make_badarg(env);
    if (!nifpp::get(env, tx_map["inputs"], input_terms))
       return enif_make_badarg(env);
    if (!nifpp::get(env, tx_map["outputs"], output_terms))
       return enif_make_badarg(env);
    if (!nifpp::get(env, tx_map["locktime"], locktime))
        locktime = 0;
    if (!nifpp::get(env, tx_map["tx_version"], tx_version))
        tx_version = 1;
    if (!nifpp::get(env, tx_map["script_version"], script_version))
        script_version = 5;

    for (auto input_term: input_terms) {
      std::map<nifpp::str_atom, nifpp::TERM> input_map;

      chain::input input;
      chain::point point;
      ErlNifBinary hash;
      unsigned long index;

      if (!nifpp::get(env, input_term, input_map))
         return enif_make_badarg(env);
      if (!enif_inspect_binary(env, input_map["hash"], &hash))
          return enif_make_badarg(env);
      if (!nifpp::get(env, input_map["index"], index))
          return enif_make_badarg(env);

      input.sequence = max_input_sequence;
      std::copy(hash.data, hash.data + hash.size, point.hash.begin());
      point.index = boost::lexical_cast<uint32_t>(index);
      input.previous_output = point;
      inputs.push_back(input);
    }

    for (auto output_term: output_terms) {
      std::map<nifpp::str_atom, nifpp::TERM> output_map;

      if (!nifpp::get(env, output_term, output_map))
         return enif_make_badarg(env);

      chain::output output;
      ErlNifUInt64 amount;
      ErlNifBinary address_bin;

      if (!nifpp::get(env, output_map["amount"], amount))
          return enif_make_badarg(env);
      if (!enif_inspect_binary(env, output_map["address"], &address_bin))
          return enif_make_badarg(env);

      std::string address_str = make_string(env, &address_bin);
      wallet::payment_address payment(address_str);
      data_chunk decoded;

      if (payment) {
        chain::operation::stack payment_ops;
        auto hash = payment.hash();
        if (payment.version() != script_version)
            payment_ops = chain::operation::to_pay_key_hash_pattern(hash);
        else if (payment.version() == script_version)
            payment_ops = chain::operation::to_pay_script_hash_pattern(hash);
        else
          return enif_make_badarg(env);

        const auto payment_script = chain::script{ payment_ops };
        outputs.push_back({ amount, payment_script });

      } else if (decode_base16(decoded, address_str)) {
        chain::script script;
        script.from_data(decoded, false, chain::script::parse_mode::raw_data);
        outputs.push_back({ amount, script });
      } else {
        wallet::stealth_address stealth(address_str);

        if (!stealth)
          return enif_make_badarg(env);

        if (stealth.signatures() != 1)
          return enif_make_badarg(env);

        ErlNifBinary seed_bin;
        if (!enif_inspect_binary(env, output_map["seed"], &seed_bin))
            return enif_make_badarg(env);

        std::string seed_str = make_string(env, &seed_bin);
        data_chunk seed;
        if (!decode_base16(seed, seed_str)) // || seed.size() < 16)
            return enif_make_badarg(env);

        data_chunk data;
        ec_secret ephemeral_secret;
        if (!create_stealth_data(data, ephemeral_secret, stealth.filter(), seed))
            return nifpp::make(env, 406);

        ec_compressed stealth_key;
        if (!uncover_stealth(stealth_key, stealth.scan_key(), ephemeral_secret, stealth.spend_keys().front()))
            return nifpp::make(env, 406);

        static constexpr uint64_t no_amount = 0;
        const auto null_data = chain::operation::to_null_data_pattern(data);
        const auto null_data_script = chain::script{ null_data };
        outputs.push_back({ no_amount, null_data_script });

        chain::operation::stack payment_ops;
        auto hash = bitcoin_short_hash(stealth_key);
        auto version = stealth.version();
        if (version != script_version)
            payment_ops = chain::operation::to_pay_key_hash_pattern(hash);
        else if (version == script_version)
            payment_ops = chain::operation::to_pay_script_hash_pattern(hash);
        else
            return false;

        const auto payment_script = chain::script{ payment_ops };
        outputs.push_back({ amount, payment_script });
      }
    }

    chain::transaction tx;

    tx.version = tx_version;
    tx.locktime = locktime;

    for (const chain::input& input: inputs)
        tx.inputs.push_back(input);
    for (const chain::output& output: outputs)
        tx.outputs.push_back(output);

    if (tx.is_locktime_conflict()) {
        return enif_make_badarg(env);
    }

    auto encoded = encode_base16(chain::transaction(tx).to_data());
    return make_binary(env, encoded);
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
    {"tx_decode", 1, erlang_libbitcoin_tx_decode, LIBBITCOIN_NIF_FLAGS},
    {"do_tx_encode", 1, erlang_libbitcoin_tx_encode, LIBBITCOIN_NIF_FLAGS},
    {"header_decode", 1, erlang_libbitcoin_header_decode, LIBBITCOIN_NIF_FLAGS},
    {"script_decode", 1, erlang_script_decode, LIBBITCOIN_NIF_FLAGS},
    {"script_encode", 1, erlang_script_encode, LIBBITCOIN_NIF_FLAGS},
    {"script_to_address", 2, erlang_script_to_address, LIBBITCOIN_NIF_FLAGS},
    {"input_signature_hash", 4, erlang_input_signature_hash, LIBBITCOIN_NIF_FLAGS}
};

ERL_NIF_INIT(libbitcoin, nif_funcs, NULL, NULL, NULL, NULL);

