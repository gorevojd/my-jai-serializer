#scope_module

visit_value :: (state: *Serialize_State, info: *Type_Info, data: *u8, depth: int) -> bool {
    framed := info.type == .ARRAY || info.type == .STRUCT;
    if info == type_info(Name) || info == type_info(Binary) || info == type_info(BinaryView) {
        framed = false;
    }
    else if info.type == .STRUCT {
        metadata := Reflection.find_struct(info);
        if metadata != null && metadata.custom_serialization {
            framed = false;
        }
    }
    if !framed {
        return visit_value_body(state, info, data, depth);
    }
    prefix := state.offset;
    length: u64;
    if !transfer_bytes(state, cast(*u8)*length, size_of(u64)) {
        return false;
    }
    begin := state.offset;
    reading := state.mode == .Validate || state.mode == .Read;
    previous_bound := state.bytes.count;
    defer state.bytes.count = previous_bound;
    if reading {
        if state.mode == .Validate {
            memcpy(*length, state.bytes.data + prefix, size_of(u64));
        }
        if length > cast(u64)(previous_bound - begin) {
            return serialize_error("Declared block size exceeds its parent block.", state);
        }
        state.bytes.count = begin + cast(s64)length;
    }
    if !visit_value_body(state, info, data, depth) {
        return false;
    }
    if reading && state.offset != state.bytes.count {
        return serialize_error("Block was not consumed exactly.", state);
    }
    if state.mode == .Write {
        length = cast(u64)(state.offset - begin);
        memcpy(state.bytes.data + prefix, *length, size_of(u64));
    }
    return true;
}

visit_field :: (state: *Serialize_State, name: string, info: *Type_Info, data: *u8, depth: int) -> bool {
    if state.path_count >= state.path.count {
        return serialize_error("Serialization path depth exceeded.", state);
    }
    path := *state.path[state.path_count];
    path.kind = .Field;
    path.name = name;
    state.path_count += 1;
    defer state.path_count -= 1;
    return visit_value(state, info, data, depth);
}
