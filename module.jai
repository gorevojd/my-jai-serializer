/**
 * @brief Serialization markers and custom codecs.
 * @details
 * - Build through the MyReflection build hook: it validates markers and registers callbacks at compile time.
 * - Struct fields are serialized in declaration order by default. @NonSerializeField excludes a field.
 * - @SerializeFieldsByDefault=false on a struct opts into explicit selection: only fields marked @SerializeField are serialized.
 *   @SerializeFieldsByDefault=true restores the default behavior. @NonSerializeField takes precedence over @SerializeField.
 * - @RestrictSerialization on a struct forbids reading/writing it through MySerialize: visiting its value triggers an assertion.
 * - @CustomSerialize applies to a struct, not a field, and replaces automatic field traversal with the struct's own codec.
 *   @RestrictSerialization also takes precedence over custom codecs. Field-selection markers do not control callback code.
 *
 * A struct T marked @CustomSerialize must declare three methods with these exact signatures:
 *   get_serialization_size :: (value: T) -> s64;
 *   serialize :: (destination: BinaryView, value: T) -> s64;
 *   deserialize :: (source: BinaryView, value: *T) -> s64;
 * - get_serialization_size returns the payload size only. It may run multiple times; keep the value and consulted external state stable.
 * - serialize writes directly into the supplied buffer and returns bytes written; it does not need to allocate a new Binary.
 * - deserialize restores value and returns bytes consumed. Check bounds and copy retained data:
 *   strings, views and other parts of the result must never reference source. Use make_name() to restore a Name.
 * - Return -1 on failure. MySerialize asserts on failure or when the returned byte count differs from the payload size.
 *
 * MySerialize prefixes the custom payload with its size (u64); callbacks receive a view of the payload only, excluding this prefix.
 * For nested values, call the module functions: get_serialization_size(field), serialize_into(view, field), deserialize_into(view, *field).
 * Sum their sizes and advance the view by the returned byte count, checking for failure first.
 * Nested sizes already include their framing; do not add it manually. Passing the same T value back into these functions causes recursion.
 * For example, an Asset_Handle codec can serialize a path through these functions and resolve the restored path to a handle when reading.
 *
 * Call serialize(value) / deserialize(binary, T) externally, or serialize_into / deserialize_into for existing buffers.
 * Calling T.serialize / T.deserialize directly handles only the payload, without MySerialize's automatic prefix and checks.
 * The caller remains responsible for releasing custom resources on failure; see the TODO below.
 */

// Format: selected fields in declaration order, native scalars, u64 byte lengths and collection element counts.
// Hash tables store key/value pairs in iteration order; hashes, capacity and tombstones are rebuilt on read.
// Composite values have a u64 payload-byte-length prefix, excluding the prefix itself.
// This format is incompatible with the older unframed struct/array format.
// No padding, field names, schema version or endian conversion is stored.
// Exclude external resource handles with @NonSerializeField: integer handles cannot be distinguished from ordinary numbers.
//
// TODO: Define a cleanup contract for custom deserialized values before making table reads transactional.
// A custom codec can create owned resources, including before returning an error; reflection cannot infer how to release them.
// For now, failed table reads retain staging entries in the destination so callers can clean up partial keys/values.
// Such a table supports cleanup iteration and deinit only: its hash index may be unfinished, so do not look up or insert keys.
// The caller must handle partially decoded custom values and release their resources; automatic rollback is not provided.

#import "MyBasic";

#load "serialize_basic.jai";
#load "serialize_block.jai";
#load "serialize_union.jai";
#load "serialize_array.jai";
#load "serialize_hash_table.jai";

/**
 * @brief Computes the encoded size without allocating output or invoking custom serialize methods.
 * @param[in] value Stable source value, including any external state consulted by its custom codec.
 * @return Byte count, or -1 after an assertion on failure. Zero is a valid size.
 */
get_serialization_size :: (value: $T) -> s64 {
    state: Serialize_State;
    state.mode = .Measure;
    if !visit_value(*state, type_info(T), cast(*u8)*value, 0) {
        return -1;
    }
    return state.offset;
}

/**
 * @brief Allocates one output buffer and serializes selected reflected fields into it.
 * @param[in] value Source value, unchanged between size calculation and writing.
 * @return Owned buffer and success flag. Release the buffer with clear_binary.
 * @note Custom size and write methods must avoid temporary allocations to preserve the one-allocation contract.
 */
serialize :: (value: $T) -> Binary, bool {
    size := get_serialization_size(value);
    if size < 0 {
        return .{}, false;
    }
    output := make_binary(size);
    if write_value(make_binary_view(output), type_info(T), cast(*u8)*value, size) < 0 {
        clear_binary(*output);
        return .{}, false;
    }
    return output, true;
}

/**
 * @brief Writes one value into caller-provided storage without allocating output memory.
 * @param[in] destination Writable storage, disjoint from the source value and its referenced bytes.
 * @param[in] value Source value, unchanged during size calculation and writing.
 * @return Bytes written, or -1 after an assertion on failure. Unused trailing storage is untouched.
 */
serialize_into :: (destination: BinaryView, value: $T) -> s64 {
    size := get_serialization_size(value);
    if size < 0 {
        return -1;
    }
    return write_value(destination, type_info(T), cast(*u8)*value, size);
}

/**
 * @brief Reads an entire binary into a value initialized with its declared defaults.
 * @param[in] binary Input storage; decoded results must never retain its bytes.
 * @param[in] T Result type.
 * @return Value and success flag. Custom-codec failure may leave a partially decoded value requiring cleanup.
 * @details
 * - Strings and BinaryViews own copies: free their data in the decoding allocator context.
 * - Binary fields own copies released with clear_binary. Names are interned and must not be freed.
 * - Arrays own independent element storage. Release nested resources before freeing the array itself.
 * - Tables own their entries and decoded keys/values. Release nested resources before deinit.
 * - Array/table decoding shares a limit of 1,048,576 elements and 256 MiB of collection storage per call.
 * - Table keys must preserve equality/hash semantics across serialization. Duplicate decoded keys are rejected.
 * - On failure, tables may contain staging entries: only iterate for resource cleanup and deinit; do not perform lookups or inserts.
 */
deserialize :: (binary: Binary, $T: Type) -> T, bool {
    value: T;
    count := deserialize_into(binary, *value);
    return value, count >= 0;
}

/**
 * @brief Reads one value from the beginning of a view after checking its structural bounds.
 * @param[in] source Input view, borrowed only for this call and never retained by decoded results.
 * @param[in,out] value Destination; unselected fields remain unchanged.
 * @return Bytes consumed, or -1 after an assertion on failure. Trailing bytes may belong to the next value.
 * @pre Release resources of an existing tagged-union variant before replacing it.
 * @pre Dynamic array destinations must have zero count and null data (no reserved storage).
 * @pre Hash-table destinations must be empty with no reserved storage; their allocator may be configured.
 * @note On a custom-codec failure, allocated arrays remain attached to the partial result for caller cleanup.
 * @pre Input must not overlap the destination or its owned allocations. Binary fields must be valid or empty.
 * @note Release previously owned strings/views before replacement; their prior storage may be borrowed or literal.
 * @note Custom codecs must check bounds before access and copy retained data. Their failure may leave earlier fields updated.
 */
deserialize_into :: (source: BinaryView, value: *$T) -> s64 {
    return read_value(source, type_info(T), cast(*u8)value, false);
}

// An owning Binary represents one complete message; reject trailing bytes before updating the destination.
deserialize_into :: (binary: Binary, value: *$T) -> s64 {
    return read_value(make_binary_view(binary), type_info(T), cast(*u8)value, true);
}

#scope_module
#import "Basic";
Reflection :: #import "MyReflection";

Serialize_Mode :: enum {
    Measure;
    Write;
    Validate;
    Read;
}

Serialization_Path_Kind :: enum { Field; Element; }
Serialization_Path :: struct {
    kind: Serialization_Path_Kind;
    name: string;
    index: s64;
}

Serialize_State :: struct {
    mode: Serialize_Mode;
    bytes: BinaryView;
    offset: s64;
    array_elements: s64;
    array_storage: s64;
    path_count: int;
    path: [MAX_SERIALIZE_DEPTH + 1]Serialization_Path;
}

MAX_SERIALIZE_DEPTH :: 128;
MAX_SERIALIZE_SIZE :: 0x7fffffffffffffff;
MAX_ARRAY_ELEMENTS :: 1048576;
MAX_ARRAY_STORAGE :: 256 * 1024 * 1024;

serialize_error :: (message: string, state: *Serialize_State = null) -> bool {
    if state != null {
        builder: String_Builder;
        defer free_buffers(*builder);
        append(*builder, "$");
        for index: 0..state.path_count - 1 {
            entry := state.path[index];
            if entry.kind == .Field {
                print_to_builder(*builder, ".%", entry.name);
            }
            else if entry.kind == .Element {
                print_to_builder(*builder, "[%]", entry.index);
            }
        }
        path := builder_to_string(*builder);
        defer free(path);
        assert(false, "MySerialize at % byte % (block end %): %", path, state.offset, state.bytes.count, message);
        return false;
    }
    assert(false, "MySerialize: %", message);
    return false;
}

transfer_bytes :: (state: *Serialize_State, data: *u8, count: s64) -> bool {
    if count < 0 || count > MAX_SERIALIZE_SIZE - state.offset {
        return serialize_error("Serialized size overflow.", state);
    }
    if state.mode != .Measure && count > state.bytes.count - state.offset {
        return serialize_error("Serialized buffer is too short.", state);
    }
    if count > 0 {
        if state.mode == .Write {
            memcpy(state.bytes.data + state.offset, data, count);
        }
        else if state.mode == .Read {
            memcpy(data, state.bytes.data + state.offset, count);
        }
    }
    state.offset += count;
    return true;
}

valid_buffer :: (bytes: BinaryView) -> bool {
    if bytes.count < 0 || (bytes.count > 0 && bytes.data == null) {
        return serialize_error("Invalid binary storage.");
    }
    return true;
}

write_value :: (destination: BinaryView, info: *Type_Info, data: *u8, expected: s64) -> s64 {
    if !valid_buffer(destination) {
        return -1;
    }
    if expected > destination.count {
        serialize_error("Destination is smaller than the calculated size.");
        return -1;
    }
    state: Serialize_State;
    state.mode = .Write;
    state.bytes = make_binary_view(destination.data, expected);
    if !visit_value(*state, info, data, 0) {
        return -1;
    }
    if state.offset != expected {
        serialize_error("Written size differs from the calculated size.");
        return -1;
    }
    return state.offset;
}

read_value :: (source: BinaryView, info: *Type_Info, data: *u8, require_complete: bool) -> s64 {
    if data == null {
        serialize_error("Null deserialization destination.");
        return -1;
    }
    if !valid_buffer(source) {
        return -1;
    }
    state: Serialize_State;
    state.mode = .Validate;
    state.bytes = source;
    if !visit_value(*state, info, data, 0) {
        return -1;
    }
    expected := state.offset;
    if require_complete && expected != source.count {
        serialize_error("Trailing bytes in serialized input.");
        return -1;
    }
    state.mode = .Read;
    state.offset = 0;
    state.array_elements = 0;
    state.array_storage = 0;
    if !visit_value(*state, info, data, 0) {
        return -1;
    }
    if state.offset != expected {
        serialize_error("Read size differs from the validated size.");
        return -1;
    }
    return state.offset;
}
