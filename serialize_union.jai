#scope_module

visit_tagged_union :: (state: *Serialize_State,
                       info: *Type_Info_Struct,
                       metadata: *Reflection.Struct_Metadata,
                       data: *u8,
                       depth: int) -> bool
{
    if info.members.count == 0 {
        return serialize_error("Tagged union has no tag member.", state);
    }
    tag := info.members[0];
    if tag.type.type != .INTEGER && tag.type.type != .ENUM {
        return serialize_error("Tagged union requires an integer or enum tag; Type-tagged union dispatch is unsupported.", state);
    }
    width := tag.type.runtime_size;
    if width != 1 && width != 2 && width != 4 && width != 8 {
        return serialize_error("Unsupported tagged-union tag width.", state);
    }
    reading := state.mode == .Validate || state.mode == .Read;
    bits: u64;
    if !reading {
        memcpy(*bits, data + tag.offset_in_bytes, width);
    }
    if state.path_count >= state.path.count {
        return serialize_error("Serialization path depth exceeded.", state);
    }
    path := *state.path[state.path_count];
    path.kind = .Field;
    path.name = tag.name;
    state.path_count += 1;
    defer state.path_count -= 1;
    tag_position := state.offset;
    if !transfer_bytes(state, cast(*u8)*bits, width) {
        return false;
    }
    if state.mode == .Validate {
        memcpy(*bits, state.bytes.data + tag_position, width);
    }
    mask: u64 = 0xffffffffffffffff;
    if width < 8 {
        mask = (cast(u64)1 << (width * 8)) - 1;
    }
    member_index: s32 = -1;
    for binding: info.tagged_union_bindings {
        if (binding.constant_value & mask) == bits {
            member_index = binding.member_index;
            break;
        }
    }
    if member_index <= 0 || member_index >= info.members.count {
        if tag.type.type == .ENUM {
            if state.mode == .Validate {
                // There is no active payload schema to validate. Read will report the bad tag and apply its fallback.
                state.offset = state.bytes.count;
                return true;
            }
            if state.mode == .Read {
                memset(data + tag.offset_in_bytes, 0, width);
                return serialize_error("Unknown tagged-union enum tag; destination tag set to 0.", state);
            }
        }
        return serialize_error("Unknown tagged-union tag.", state);
    }
    member := info.members[member_index];
    if member.flags & .CONSTANT {
        return serialize_error("Tagged-union binding does not reference a value.", state);
    }
    selected := false;
    for field: metadata.serialized_fields {
        if field.name == member.name {
            selected = true;
            break;
        }
    }
    destination: *u8;
    if state.mode == .Read {
        // Old resources must have been released by the caller. Do not initialize inactive variants.
        memset(data, 0, info.runtime_size);
        destination = data + member.offset_in_bytes;
        initialize_array_element(member.type, destination);
        // Publish the chosen variant so partial custom-codec results can be released by the caller.
        memcpy(data + tag.offset_in_bytes, *bits, width);
    }
    else if !reading {
        destination = data + member.offset_in_bytes;
    }
    if !selected {
        return true;
    }
    // Replace the tag path component with the active variant name.
    path.name = member.name;
    return visit_value(state, member.type, destination, depth + 1);
}
