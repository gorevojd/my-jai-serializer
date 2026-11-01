#scope_module

visit_type :: (state: *Serialize_State, data: *u8) -> bool {
    reading := state.mode == .Validate || state.mode == .Read;
    name: string;
    length: u64;
    if !reading {
        name = Reflection.registered_type_name((cast(*Type)data).*);
        if name.count == 0 {
            return serialize_error("Type value must refer to a struct declared with @Register.", state);
        }
        length = cast(u64)name.count;
    }
    if !transfer_bytes(state, cast(*u8)*length, size_of(u64)) {
        return false;
    }
    if state.mode == .Validate {
        memcpy(*length, state.bytes.data + state.offset - size_of(u64), size_of(u64));
    }
    if reading {
        if length > cast(u64)(state.bytes.count - state.offset) {
            return serialize_error("Registered type name exceeds its block.", state);
        }
        name.count = cast(s64)length;
        name.data = state.bytes.data + state.offset;
        registered := Reflection.find_registered_struct(name);
        if registered == null {
            return serialize_error("Unknown registered type name.", state);
        }
        if state.mode == .Read {
            (cast(*Type)data).* = registered.struct_type;
        }
        state.offset += name.count;
        return true;
    }
    return transfer_bytes(state, name.data, name.count);
}
