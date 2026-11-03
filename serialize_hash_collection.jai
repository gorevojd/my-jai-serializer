#scope_module

visit_hash_collection :: (state: *Serialize_State, collection: Reflection.Hash_Collection_Adapter, data: *u8, depth: int) -> bool {
    if (collection.count == null ||
        collection.access == null ||
        collection.try_reserve_empty == null ||
        collection.empty == null ||
        collection.next == null ||
        collection.contains == null ||
        collection.insert == null ||
        collection.reset == null)
    {
        return serialize_error("Missing reflected hash-collection adapter.", state);
    }
    has_values := collection.kind == .Table;
    reading := state.mode == .Validate || state.mode == .Read;
    length: u64;
    if !reading {
        count := collection.count(data);
        if count < 0 {
            return serialize_error("Invalid hash-collection count.", state);
        }
        length = cast(u64)count;
    }
    else if data != null && !collection.empty(data) {
        return serialize_error("Hash-collection destination must be empty with no reserved storage.", state);
    }
    if !transfer_bytes(state, cast(*u8)*length, size_of(u64)) {
        return false;
    }
    if state.mode == .Validate {
        memcpy(*length, state.bytes.data + state.offset - size_of(u64), size_of(u64));
    }
    // The encoded count is u64; the runtime collection count is s64.
    if length > cast(u64)S64_MAX {
        return serialize_error("Hash-collection count exceeds s64 capacity.", state);
    }
    count := cast(s64)length;
    index_bytes: s64;
    if reading && count > 0 {
        capacity, valid := Reflection.hash_collection_capacity(count, collection.entry_size, collection.load_factor, collection.minimum_slots);
        if !valid {
            return serialize_error("Hash-collection storage size overflow or invalid configuration.", state);
        }
        index_bytes = capacity * collection.entry_size;
    }
    if state.mode == .Read {
        staging_bytes := count * collection.entry_size;
        if !can_allocate_deserialized(state.read_context, staging_bytes) {
            return false;
        }
        if !prepare_hash_entries(collection, data, count) {
            return serialize_allocation_error("MySerialize: could not allocate hash-collection entries.");
        }
        state.read_context.allocated_bytes += staging_bytes;
    }
    cursor: s64;
    for index: 0..count - 1 {
        key, value: *u8;
        if state.mode != .Validate {
            key, value = collection.next(data, *cursor);
            if key == null || (has_values && value == null) {
                return serialize_error("Hash-collection count does not match its occupied entries.", state);
            }
        }
        if !visit_hash_entry(state, collection, key, value, index, depth) {
            return false;
        }
    }
    if !reading {
        key, value := collection.next(data, *cursor);
        if key != null || value != null {
            return serialize_error("Hash-collection count does not match its occupied entries.", state);
        }
    }
    if state.mode == .Read {
        if !can_allocate_deserialized(state.read_context, index_bytes) {
            return false;
        }
        complete, duplicate := finish_hash_entries(collection, data, state.read_context, index_bytes);
        if !complete && duplicate < 0 {
            return serialize_allocation_error("MySerialize: could not allocate the hash-collection index.");
        }
        if duplicate >= 0 {
            if state.path_count < state.path.count {
                path := *state.path[state.path_count];
                path.kind = .Element;
                path.index = duplicate;
                state.path_count += 1;
                defer state.path_count -= 1;
                return serialize_error("Duplicate hash-collection key after deserialization.", state);
            }
            return serialize_error("Duplicate hash-collection key after deserialization.", state);
        }
    }
    return true;
}

prepare_hash_entries :: (collection: Reflection.Hash_Collection_Adapter, data: *u8, count: s64) -> bool {
    if count == 0 {
        return true;
    }

    storage := collection.access(data);
    storage.allocator.* = context.allocator;

    bytes := count * collection.entry_size;
    entries := cast(*u8)alloc(bytes,, storage.allocator.*);
    if entries == null {
        return false;
    }

    memset(entries, 0, bytes);
    storage.entries.data = entries;
    storage.entries.count = count;
    storage.count.* = count;
    storage.allocated.* = count;
    storage.slots_filled.* = count;

    // Preserve access to partial keys/values for caller cleanup if decoding fails.
    for index: 0..count - 1 {
        entry := entries + index * collection.entry_size;
        initialize_array_element(cast(*Type_Info)collection.entry_type, entry);
        (cast(*u32)(entry + collection.hash_offset)).* = collection.first_valid_hash;
    }
    return true;
}

finish_hash_entries :: (collection: Reflection.Hash_Collection_Adapter, data: *u8, settings: *Deserialize_Context, index_bytes: s64) -> bool, s64 {
    storage := collection.access(data);
    count := storage.count.*;
    if count == 0 {
        return true, -1;
    }

    staging := storage.entries.*;
    // Detach the staging allocation before reset so it remains available on failure.
    storage.entries.* = .{};
    collection.reset(data);
    complete := false;
    defer {
        if !complete {
            collection.reset(data);
            storage.entries.* = staging;
            storage.count.* = count;
            storage.allocated.* = count;
            storage.slots_filled.* = count;
        }
    }

    if !collection.try_reserve_empty(data, count) {
        return false, -1;
    }
    settings.allocated_bytes += index_bytes;

    for index: 0..count - 1 {
        entry := staging.data + index * collection.entry_size;
        key := entry + collection.key_offset;
        value := ifx collection.kind == .Table then entry + collection.value_offset else null;
        if collection.contains(data, key) {
            return false, index;
        }
        collection.insert(data, key, value);
    }

    free(staging.data,, storage.allocator.*);
    complete = true;
    return true, -1;
}

visit_hash_entry :: (state: *Serialize_State,
                     collection: Reflection.Hash_Collection_Adapter,
                     key: *u8,
                     value: *u8,
                     index: s64,
                     depth: int) -> bool
{
    if state.path_count >= state.path.count {
        return serialize_error("Serialization path depth exceeded.", state);
    }

    path := *state.path[state.path_count];
    path.kind = .Element;
    path.index = index;
    state.path_count += 1;
    defer state.path_count -= 1;

    if collection.kind == .Set {
        return visit_value(state, cast(*Type_Info)collection.key_type, key, depth + 1);
    }

    if !visit_field(state, "key", cast(*Type_Info)collection.key_type, key, depth + 1) {
        return false;
    }

    return visit_field(state, "value", cast(*Type_Info)collection.value_type, value, depth + 1);
}
