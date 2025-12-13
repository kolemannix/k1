use std::ffi::c_void;

use libffi::raw::ffi_cif;
use libffi::{low::*, raw};
use log::debug;

use crate::bc::{self, InstKind, ProgramBytecode};
use crate::errf;
use crate::lex::SpanId;
use crate::typer::types::{AggType, PhysicalType, ScalarType};
use crate::typer::{FunctionId, Layout, TypedProgram, TyperResult};
use crate::vm;
use crate::vm::{Value, Vm};
use crate::{failf, kmem::MSlice, parse::Ident};

pub(super) fn handle_ffi_call(
    k1: &mut TypedProgram,
    vm: &mut Vm,
    frame_index: u32,
    inst_offset: u32,
    ret_inst_kind: InstKind,
    args: MSlice<bc::Value, ProgramBytecode>,
    lib_name: Option<Ident>,
    fn_name: Ident,
    function_id: FunctionId,
) -> TyperResult<Value> {
    let nargs = args.len() as usize;
    let mut ffi_args_value_storage = vm.stack.mem.new_list(nargs as u32);
    let mut ffi_args_value_ptrs = vm.stack.mem.new_list(nargs as u32);

    let function_params = k1.bytecode.functions.get(function_id).unwrap().fn_params;

    for (arg_value, arg_pt) in
        k1.bytecode.mem.getn(args).iter().zip(k1.bytecode.mem.getn(function_params))
    {
        let vm_value = vm::resolve_value(k1, vm, frame_index, inst_offset, *arg_value)?;

        // If aggregate, you already have the pointer that libffi wants
        if arg_pt.is_agg() {
            ffi_args_value_ptrs.push(vm_value.as_ptr() as *mut u8 as *mut c_void);
        } else {
            // If scalar, _get_ a pointer to it
            ffi_args_value_storage.push(vm_value.bits());

            // Now we have an address to push into the actual args array
            let ffi_arg_addr = ffi_args_value_storage.last_mut().unwrap();
            ffi_args_value_ptrs.push(ffi_arg_addr as *mut _ as *mut c_void)
        }
    }

    let mut ffi_handle = match k1.vm_ffi_functions.get(&function_id).copied() {
        None => {
            let handle_for_search = match lib_name {
                None => k1.vm_process_dlopen_handle,
                Some(lib_name) => k1.get_dylib_handle(function_id, lib_name, vm.eval_span)?,
            };

            let name_cstr = std::ffi::CString::new(k1.ident_str(fn_name)).unwrap();
            let fn_ptr: *mut c_void = unsafe { libc::dlsym(handle_for_search, name_cstr.as_ptr()) };
            if fn_ptr.is_null() {
                return failf!(
                    vm.eval_span,
                    "Could not find extern function symbol: {}",
                    k1.ident_str(fn_name)
                );
            }
            let cif = prep_ffi_cif(k1, function_id, ret_inst_kind, vm.eval_span)?;
            let handle = vm::VmFfiHandle {
                library_handle: handle_for_search,
                function_pointer: fn_ptr,
                cif,
            };
            k1.vm_ffi_functions.insert(function_id, handle);
            handle
        }
        Some(ffi_handle) => {
            debug!("reusing cif");
            ffi_handle
        }
    };

    debug!("ffi args were: {:?}", ffi_args_value_storage.as_slice());
    debug!("ffi args types were: {:?}", unsafe {
        core::slice::from_raw_parts(ffi_handle.cif.arg_types, nargs)
    });

    let result_storage = unsafe {
        let ret_size = (*(ffi_handle.cif.rtype)).size;
        let ret_align = (*(ffi_handle.cif.rtype)).alignment;
        let result_space: *mut u8 =
            vm.stack.push_layout_uninit(Layout { size: ret_size as u32, align: ret_align as u32 });
        debug!("result space is {} {}", ret_size, ret_align);

        let args = ffi_args_value_ptrs.as_slice_mut().as_mut_ptr();
        let code_ptr = CodePtr(ffi_handle.function_pointer);
        raw::ffi_call(
            &mut ffi_handle.cif,
            Some(*code_ptr.as_safe_fun()),
            result_space as *mut c_void,
            args,
        );
        result_space
    };
    let result = match ret_inst_kind {
        InstKind::Value(physical_type) => {
            let result = vm::load_value(physical_type, result_storage.cast_const());
            result
        }
        InstKind::Void => Value::UNIT,
        InstKind::Terminator => {
            eprintln!(
                "WARNING: Allegedly divergent FFI call actually returned; its likely mis-typed!!"
            );
            Value::UNIT
        }
    };
    debug!("ffi result is: {}", result);
    Ok(result)
}

fn prep_ffi_cif(
    k1: &mut TypedProgram,
    function_id: FunctionId,
    return_type: InstKind,
    span: SpanId,
) -> TyperResult<ffi_cif> {
    let Some(compiled_function) = k1.bytecode.functions.get(function_id) else {
        return failf!(
            span,
            "External call to uncompiled function: {}. ({} are pending)",
            k1.function_id_to_string(function_id, false),
            k1.bytecode.b_units_pending_compile.len()
        );
    };
    let nargs = compiled_function.fn_params.len() as usize;
    let fn_params = compiled_function.fn_params;
    let mut ffi_args_types_storage = k1.mem.new_list(nargs as u32);
    let mut ffi_args_types_ptrs = k1.mem.new_list(nargs as u32);
    for arg_pt in k1.bytecode.mem.getn(fn_params) {
        let ffi_type: ffi_type = inst_kind_to_ffi_type(k1, InstKind::Value(*arg_pt))
            .map_err(|msg| errf!(span, "Function type is not FFI compatible: {msg}"))?;

        // We need a stable-ish address to each Value here; so we push them to
        // a parallel collection
        ffi_args_types_storage.push(ffi_type);
        let type_addr: &mut ffi_type = ffi_args_types_storage.last_mut().unwrap();
        ffi_args_types_ptrs.push(type_addr as *mut ffi_type);
    }
    let atypes: *mut *mut ffi_type = ffi_args_types_ptrs.as_mut_ptr().cast();

    let ffi_ret_type: ffi_type = inst_kind_to_ffi_type(k1, return_type)
        .map_err(|msg| errf!(span, "Function type is not FFI compatible: {msg}"))?;
    let ffi_ret_type_alloced: &mut ffi_type = k1.mem.push(ffi_ret_type);

    let mut cif: ffi_cif = ffi_cif::default();
    unsafe {
        libffi::low::prep_cif(
            &mut cif,
            ffi_abi_FFI_DEFAULT_ABI,
            nargs,
            ffi_ret_type_alloced,
            atypes,
        )
        .map_err(|e| errf!(span, "failed to set up ffi context: {:?}", e))?;
    }
    Ok(cif)
}

fn scalar_to_ffi_type(st: ScalarType) -> libffi::low::ffi_type {
    unsafe {
        match st {
            ScalarType::U8 => types::uint8,
            ScalarType::U16 => types::uint16,
            ScalarType::U32 => types::uint32,
            ScalarType::U64 => types::uint64,
            ScalarType::I8 => types::sint8,
            ScalarType::I16 => types::sint16,
            ScalarType::I32 => types::sint32,
            ScalarType::I64 => types::sint64,
            ScalarType::F32 => types::float,
            ScalarType::F64 => types::double,
            ScalarType::Pointer => types::pointer,
        }
    }
}

fn inst_kind_to_ffi_type(
    k1: &mut TypedProgram,
    inst_kind: InstKind,
) -> std::result::Result<libffi::low::ffi_type, &'static str> {
    use libffi::low;
    unsafe {
        match inst_kind {
            InstKind::Value(PhysicalType::Scalar(st)) => Ok(scalar_to_ffi_type(st)),
            InstKind::Void => Ok(low::types::void),
            InstKind::Terminator => Ok(low::types::void),
            InstKind::Value(pt) => {
                let t = pt_to_ffi_type(k1, pt)?;
                Ok(t)
            }
        }
    }
}

fn pt_to_ffi_type(
    k1: &mut TypedProgram,
    pt: PhysicalType,
) -> std::result::Result<libffi::low::ffi_type, &'static str> {
    match pt {
        PhysicalType::Scalar(st) => Ok(scalar_to_ffi_type(st)),
        PhysicalType::Agg(agg_id) => match k1.types.phys_types.get(agg_id).agg_type {
            AggType::Struct1(physical_type) => {
                let mut element_storage = k1.mem.new_list(1);
                element_storage.push(pt_to_ffi_type(k1, physical_type)?);
                let t = make_struct_ffi_type(k1, element_storage.as_slice_mut());
                Ok(t)
            }
            AggType::EnumVariant(enum_variant_layout) => {
                let count = if enum_variant_layout.payload.is_some() { 2 } else { 1 };
                let mut element_storage = k1.mem.new_list(count);

                element_storage.push(scalar_to_ffi_type(enum_variant_layout.tag));
                if let Some(payload) = enum_variant_layout.payload {
                    let payload_ffi_type = pt_to_ffi_type(k1, payload)?;
                    element_storage.push(payload_ffi_type);
                }
                let t = make_struct_ffi_type(k1, element_storage.as_slice_mut());
                Ok(t)
            }
            AggType::Struct { fields } => {
                let mut element_storage = k1.mem.new_list(fields.len());
                for field in k1.types.mem.getn(fields).iter() {
                    let field_ffi_type = pt_to_ffi_type(k1, field.field_t)?;
                    element_storage.push(field_ffi_type);
                }
                let t = make_struct_ffi_type(k1, element_storage.as_slice_mut());
                Ok(t)
            }
            AggType::Array { .. } => {
                Err("Arrays can't be passed directly by value in C / via system ffi")
            }
            AggType::Opaque { .. } => {
                Err("Opaques can't be passed directly by value in C / via system ffi")
            }
        },
    }
}

#[allow(clippy::field_reassign_with_default)]
fn make_struct_ffi_type(
    k1: &mut TypedProgram,
    ffi_type_storage: &mut [ffi_type],
) -> libffi::low::ffi_type {
    let mut element_ptrs = k1.mem.new_list::<*mut ffi_type>(ffi_type_storage.len() as u32 + 1);

    // Explicit range loop to be really sure the pointers are stable and not tied to some iterator bullshit
    #[allow(clippy::needless_range_loop)]
    for i in 0..ffi_type_storage.len() {
        #[cfg(debug_assertions)]
        {
            let t = ffi_type_storage[i];
            eprintln!(
                "ffi struct elements {i}: size: {}, align: {}, type: {}",
                t.size, t.alignment, t.type_
            );
            eprintln!("ffi struct element ptrs: {:?}", element_ptrs.as_slice());
        }
        element_ptrs.push(&mut ffi_type_storage[i])
    }
    element_ptrs.push(core::ptr::null_mut());

    // size and alignment get filled in my ffi_prep_cif
    let my_struct: ffi_type = ffi_type {
        size: 0,
        alignment: 0,
        type_: type_tag::STRUCT,
        elements: element_ptrs.as_mut_ptr(),
    };
    my_struct
}
