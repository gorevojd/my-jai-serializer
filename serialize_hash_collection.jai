#scope_module

visit_hash_collection :: (state: *Serialize_State, table: Reflection.Hash_Collection_Adapter, data: *u8, depth: int) -> bool {
    if (table.count == null ||
        table.access == null ||
        table.try_reserve_empty == null ||
        table.empty == null ||
        table.next == null ||
        table.contains == null ||
        table.insert == null ||
        table.reset == null)
    {
        return serialize_error("Missing reflected hash-collection adapter.", state);
    }
    has_values := table.kind == .Table;
    reading := state.mode == .Validate || state.mode == .Read;
    length: u64;
    if !reading {
        count := table.count(data);
        if count < 0 {
            return serialize_error("Invalid hash-collection count.", state);
        }
        length = cast(u64)count;
    }
    else if data != null && !table.empty(data) {
        return serialize_error("Hash-collection destination must be empty with no reserved storage.", state);
    }
    if !transfer_bytes(state, cast(*u8)*length, size_of(u64)) {
        return false;
    }
    if state.mode == .Validate {
        memcpy(*length, state.bytes.data + state.offset - size_of(u64), size_of(u64));
    }
    // The encoded count is u64; the runtime table count is s64.
    if length > cast(u64)S64_MAX {
        return serialize_error("Hash-collection count exceeds s64 capacity.", state);
    }
    count := cast(s64)length;
    index_bytes: s64;
    if reading && count > 0 {
        capacity, valid := Reflection.hash_collection_capacity(count, table.entry_size, table.load_factor, table.minimum_slots);
        if !valid {
            return serialize_error("Hash-collection storage size overflow or invalid configuration.", state);
        }
        index_bytes = capacity * table.entry_size;
    }
    if state.mode == .Read {
        staging_bytes := count * table.entry_size;
        if !can_allocate_deserialized(state.read_context, staging_bytes) {
            return false;
        }
        if !prepare_hash_entries(table, data, count) {
            return serialize_allocation_error("MySerialize: could not allocate hash-collection entries.");
        }
        state.read_context.allocated_bytes += staging_bytes;
    }
    cursor: s64;
    for index: 0..count - 1 {
        key, value: *u8;
        if state.mode != .Validate {
            key, value = table.next(data, *cursor);
            if key == null || (has_values && value == null) {
                return serialize_error("Hash-collection count does not match its occupied entries.", state);
            }
        }
        if !visit_hash_entry(state, table, key, value, index, depth) {
            return false;
        }
    }
    if !reading {
        key, value := table.next(data, *cursor);
        if key != null || value != null {
            return serialize_error("Hash-collection count does not match its occupied entries.", state);
        }
    }
    if state.mode == .Read {
        if !can_allocate_deserialized(state.read_context, index_bytes) {
            return false;
        }
        complete, duplicate := finish_hash_entries(table, data, state.read_context, index_bytes);
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

prepare_hash_entries :: (table: Reflection.Hash_Collection_Adapter, data: *u8, count: s64) -> bool {
    if count == 0 {
        return true;
    }

    storage := table.access(data);
    storage.allocator.* = context.allocator;

    bytes := count * table.entry_size;
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
        entry := entries + index * table.entry_size;
        initialize_array_element(cast(*Type_Info)table.entry_type, entry);
        (cast(*u32)(entry + table.hash_offset)).* = table.first_valid_hash;
    }
    return true;
}

finish_hash_entries :: (table: Reflection.Hash_Collection_Adapter, data: *u8, settings: *Deserialize_Context, index_bytes: s64) -> bool, s64 {
    storage := table.access(data);
    count := storage.count.*;
    if count == 0 {
        return true, -1;
    }

    staging := storage.entries.*;
    // Detach the staging allocation before reset so it remains available on failure.
    storage.entries.* = .{};
    table.reset(data);
    complete := false;
    defer {
        if !complete {
            table.reset(data);
            storage.entries.* = staging;
            storage.count.* = count;
            storage.allocated.* = count;
            storage.slots_filled.* = count;
        }
    }

    if !table.try_reserve_empty(data, count) {
        return false, -1;
    }
    settings.allocated_bytes += index_bytes;

    for index: 0..count - 1 {
        entry := staging.data + index * table.entry_size;
        key := entry + table.key_offset;
        value := ifx table.kind == .Table then entry + table.value_offset else null;
        if table.contains(data, key) {
            return false, index;
        }
        table.insert(data, key, value);
    }

    free(staging.data,, storage.allocator.*);
    complete = true;
    return true, -1;
}

visit_hash_entry :: (state: *Serialize_State,
                      table: Reflection.Hash_Collection_Adapter,
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

    if table.kind == .Set {
        return visit_value(state, cast(*Type_Info)table.key_type, key, depth + 1);
    }

    if !visit_field(state, "key", cast(*Type_Info)table.key_type, key, depth + 1) {
        return false;
    }

    return visit_field(state, "value", cast(*Type_Info)table.value_type, value, depth + 1);
}
