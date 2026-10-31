#scope_module

visit_array :: (state: *Serialize_State, info: *Type_Info_Array, data: *u8, depth: int) -> bool {
    reading := state.mode == .Validate || state.mode == .Read;
    fixed := info.array_type == .FIXED;
    count: s64;
    elements: *u8;
    if !reading {
        if fixed {
            count = info.array_count;
            elements = data;
        }
        else {
            view := cast(*Array_View_64)data;
            count = view.count;
            elements = view.data;
        }
        if count < 0 || (count > 0 && elements == null) {
            return serialize_error("Invalid array storage.", state);
        }
    }
    else if !fixed && data != null {
        view := cast(*Array_View_64)data;
        if view.count != 0 || view.data != null {
            return serialize_error("Dynamic array destination must be empty with no reserved storage.", state);
        }
        if info.array_type == .RESIZABLE && (cast(*Resizable_Array)data).allocated != 0 {
            return serialize_error("Dynamic array destination must not have reserved capacity.", state);
        }
    }
    length := cast(u64)count;
    if !transfer_bytes(state, cast(*u8)*length, size_of(u64)) {
        return false;
    }
    if state.mode == .Validate {
        memcpy(*length, state.bytes.data + state.offset - size_of(u64), size_of(u64));
    }
    if reading && fixed && length != cast(u64)info.array_count {
        return serialize_error("Fixed array element count does not match its type.", state);
    }
    if length > cast(u64)(MAX_ARRAY_ELEMENTS - state.array_elements) {
        return serialize_error("Total array element limit exceeded.", state);
    }
    count = cast(s64)length;
    state.array_elements += count;
    stride := info.element_type.runtime_size;
    if stride < 0 || (stride > 0 && count > MAX_SERIALIZE_SIZE / stride) {
        return serialize_error("Array storage size overflow.", state);
    }
    byte_count := count * stride;
    if reading && !fixed {
        if byte_count > MAX_ARRAY_STORAGE - state.array_storage {
            return serialize_error("Array allocation budget exceeded.", state);
        }
        state.array_storage += byte_count;
    }
    if state.mode == .Read {
        if fixed {
            elements = data;
        }
        else {
            if count > 0 {
                // Zero-size element types still need a usable storage pointer for callbacks.
                elements = cast(*u8)alloc(ifx byte_count > 0 then byte_count else 1);
                if elements == null {
                    return serialize_error("Could not allocate array storage.", state);
                }
                if byte_count > 0 {
                    memset(elements, 0, byte_count);
                }
                for index: 0..count - 1 {
                    initialize_array_element(info.element_type, elements + index * stride);
                }
            }
            view := cast(*Array_View_64)data;
            view.count = count;
            view.data = elements;
            if info.array_type == .RESIZABLE {
                array := cast(*Resizable_Array)data;
                array.allocated = count;
                remember_allocators(array);
            }
        }
    }
    else if state.mode == .Validate && fixed {
        elements = data;
    }
    for index: 0..count - 1 {
        element := ifx elements != null then elements + index * stride else null;
        if state.path_count >= state.path.count {
            return serialize_error("Serialization path depth exceeded.", state);
        }
        path := *state.path[state.path_count];
        path.kind = .Element;
        path.index = index;
        state.path_count += 1;
        success := visit_value(state, info.element_type, element, depth + 1);
        state.path_count -= 1;
        if !success {
            return false;
        }
    }
    return true;
}

initialize_array_element :: (info: *Type_Info, data: *u8) {
    if info.type == .STRUCT {
        structure := cast(*Type_Info_Struct)info;
        if structure.initializer != null {
            structure.initializer(data);
        }
    }
    else if info.type == .ARRAY {
        array := cast(*Type_Info_Array)info;
        if array.array_type == .FIXED {
            for index: 0..array.array_count - 1 {
                initialize_array_element(array.element_type, data + index * array.element_type.runtime_size);
            }
        }
    }
}
