#scope_module

visit_enum :: (state: *Serialize_State, info: *Type_Info_Enum, data: *u8) -> bool {
    width := info.runtime_size;
    if width != 1 && width != 2 && width != 4 && width != 8 {
        return serialize_error("Unsupported enum storage width.", state);
    }
    if state.mode != .Read {
        // Validate checks bounds; checking values during Read lets us apply the zero fallback to an actual destination.
        return transfer_bytes(state, data, width);
    }
    bits: u64;
    if !transfer_bytes(state, cast(*u8)*bits, width) {
        return false;
    }
    mask: u64 = 0xffffffffffffffff;
    if width < 8 {
        mask = (cast(u64)1 << (width * 8)) - 1;
    }
    valid: bool;
    if info.enum_type_flags & .FLAGS {
        allowed: u64;
        for value: info.values {
            value_bits: u64;
            memcpy(*value_bits, *value, size_of(u64));
            allowed |= value_bits & mask;
        }
        valid = (bits & ~allowed) == 0;
    }
    else {
        for value: info.values {
            value_bits: u64;
            memcpy(*value_bits, *value, size_of(u64));
            if (value_bits & mask) == bits {
                valid = true;
                break;
            }
        }
    }
    if !valid {
        memset(data, 0, width);
        return serialize_error(tprint("Invalid value % for enum '%'; destination set to 0.", bits, info.name), state);
    }
    memcpy(data, *bits, width);
    return true;
}
