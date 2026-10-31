#scope_module

visit_blob :: (state: *Serialize_State, info: *Type_Info, data: *u8) -> bool {
    reading := state.mode == .Validate || state.mode == .Read;
    bytes: BinaryView;
    length: u64;
    if !reading {
        if info == type_info(string) {
            text := (cast(*string)data).*;
            bytes = make_binary_view(text.data, text.count);
        }
        else if info == type_info(Name) {
            text := (cast(*Name)data).text;
            bytes = make_binary_view(text.data, text.count);
        }
        else if info == type_info(Binary) {
            bytes = make_binary_view((cast(*Binary)data).*);
        }
        else if info == type_info(BinaryView) {
            bytes = (cast(*BinaryView)data).*;
        }
        else {
            return serialize_error("Unsupported blob type.", state);
        }

        if bytes.count < 0 || (bytes.count > 0 && bytes.data == null) {
            return serialize_error("Invalid string or binary storage.", state);
        }

        length = cast(u64)bytes.count;
    }

    if !transfer_bytes(state, cast(*u8)*length, size_of(u64)) {
        return false;
    }

    if state.mode == .Validate {
        memcpy(*length, state.bytes.data + state.offset - size_of(u64), size_of(u64));
    }

    if length > MAX_SERIALIZE_SIZE {
        return serialize_error("Serialized byte length exceeds s64 capacity.", state);
    }

    count := cast(s64)length;
    if reading {
        if count > state.bytes.count - state.offset {
            return serialize_error("Serialized byte payload is truncated.", state);
        }

        if count > 0 {
            bytes = make_binary_view(state.bytes.data + state.offset, count);
        }

        if state.mode == .Read {
            if info == type_info(string) || info == type_info(Name) {
                text: string;
                text.data = bytes.data;
                text.count = bytes.count;
                if info == type_info(Name) {
                    (cast(*Name)data).* = make_name(text);
                }
                else if info == type_info(string) {
                    (cast(*string)data).* = copy_string(text);
                }
            }
            else if info == type_info(Binary) {
                replacement := deep_copy_binary(bytes.data, bytes.count);
                clear_binary(cast(*Binary)data);
                (cast(*Binary)data).* = replacement;
            }
            else if info == type_info(BinaryView) {
                owned := deep_copy_binary(bytes.data, bytes.count);
                (cast(*BinaryView)data).* = make_binary_view(owned);
            }
            else {
                return serialize_error("Unsupported blob destination.", state);
            }
        }
        state.offset += count;
        return true;
    }
    return transfer_bytes(state, bytes.data, count);
}

visit_value_body :: (state: *Serialize_State, info: *Type_Info, data: *u8, depth: int) -> bool {
    if depth > MAX_SERIALIZE_DEPTH {
        return serialize_error("Serialization nesting limit exceeded.", state);
    }

    if (info == type_info(string) ||
        info == type_info(Name) ||
        info == type_info(Binary) ||
        info == type_info(BinaryView))
    {
        return visit_blob(state, info, data);
    }

    if info.type == .ARRAY {
        return visit_array(state, cast(*Type_Info_Array)info, data, depth);
    }

    if info.type == .BOOL {
        if state.mode == .Validate || state.mode == .Read {
            if state.offset >= state.bytes.count {
                return serialize_error("Serialized bool is truncated.", state);
            }

            if state.bytes[state.offset] > 1 {
                return serialize_error("Serialized bool must be zero or one.", state);
            }
        }

        return transfer_bytes(state, data, 1);
    }

    if info.type == .INTEGER || info.type == .FLOAT || info.type == .ENUM {
        return transfer_bytes(state, data, info.runtime_size);
    }

    if info.type == .STRUCT {
        metadata := Reflection.find_struct(info);
        if metadata == null {
            return serialize_error("Missing reflected metadata.", state);
        }
        if metadata.restrict_serialization {
            return serialize_error(tprint("Type '%' has @RestrictSerialization.", metadata.name), state);
        }
        if metadata.custom_serialization {
            return visit_custom(state, metadata, data);
        }
        if metadata.is_hash_table {
            return visit_hash_table(state, metadata.table_adapter, data, depth);
        }
        structure := cast(*Type_Info_Struct)info;
        if structure.textual_flags & .UNION_IS_TAGGED {
            return visit_tagged_union(state, structure, metadata, data, depth);
        }
        if structure.textual_flags & .UNION {
            return serialize_error("Untagged union serialization is unsupported.", state);
        }

        for member: structure.members {
            if member.flags & .OVERLAY {
                return serialize_error("Overlapping struct fields are unsupported.", state);
            }
        }

        for field: metadata.serialized_fields {
            if !visit_field(state, field.name, cast(*Type_Info)field.member_type,
                           ifx data != null then data + field.offset else null, depth + 1) {
                return false;
            }
        }

        return true;
    }

    return serialize_error("Unsupported type: raw pointers, procedures and external resources require a codec.", state);
}

visit_custom :: (state: *Serialize_State, metadata: *Reflection.Struct_Metadata, data: *u8) -> bool {
    if (metadata.custom_size == null ||
        metadata.custom_serialize == null ||
        metadata.custom_deserialize == null) {
        return serialize_error("Missing custom serialization callbacks.", state);
    }
    length: u64;
    writing := state.mode == .Measure || state.mode == .Write;
    if writing {
        size := metadata.custom_size(data);
        if size < 0 {
            return serialize_error("Custom size calculation failed.", state);
        }
        length = cast(u64)size;
    }
    if !transfer_bytes(state, cast(*u8)*length, size_of(u64)) {
        return false;
    }
    if state.mode == .Validate {
        memcpy(*length, state.bytes.data + state.offset - size_of(u64), size_of(u64));
    }
    if length > cast(u64)(MAX_SERIALIZE_SIZE - state.offset) {
        return serialize_error("Custom payload size overflow.", state);
    }
    size := cast(s64)length;
    if state.mode != .Measure && size > state.bytes.count - state.offset {
        return serialize_error("Custom payload exceeds the available buffer.", state);
    }
    if state.mode == .Write || state.mode == .Read {
        payload := make_binary_view(state.bytes.data + state.offset, size);
        count: s64;
        if state.mode == .Write {
            count = metadata.custom_serialize(payload, data);
        }
        else if state.mode == .Read {
            count = metadata.custom_deserialize(payload, data);
        }
        if count < 0 || count != size {
            return serialize_error("Custom codec returned a byte count different from its payload size.", state);
        }
    }
    state.offset += size;
    return true;
}
