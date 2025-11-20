use std::ffi::c_void;

use libffi::low::*;
use libffi::raw::ffi_cif;

use crate::bc::{self, InstKind, ProgramBytecode};
use crate::errf;
use crate::typer::types::{PhysicalType, ScalarType};
use crate::typer::{FunctionId, TypedProgram, TyperResult};
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
    eprintln!("ffi call {}", k1.function_id_to_string(function_id, false));
    // nocommit: Have to use Vec here since libffi::Type has `Drop` impl.
    // Switch to libffi::low
    // nocommit: Finish ffi
    // - Cache cif for each function
    // - Use stack space for args
    // - X Separate file
    // - X Switch to 'libffi::low' level bindings
    // - Support aggregate types
    let nargs = args.len() as usize;
    let mut ffi_args_types_storage = Vec::with_capacity(nargs);
    let mut ffi_args_types_ptrs = Vec::with_capacity(nargs);

    let mut ffi_args_value_storage = Vec::with_capacity(nargs);
    let mut ffi_args_value_ptrs = vm.stack.mem.new_list(nargs as u32);

    let Some(compiled_function) = k1.bytecode.functions.get(function_id) else {
        return failf!(
            vm.eval_span,
            "External call to uncompiled function: {}. ({} are pending)",
            k1.function_id_to_string(function_id, false),
            k1.bytecode.b_units_pending_compile.len()
        );
    };
    let mut ffi_ret_type = inst_kind_to_ffi_type(k1, ret_inst_kind);
    for (arg_value, arg_pt) in
        k1.bytecode.mem.getn(args).iter().zip(k1.bytecode.mem.getn(compiled_function.fn_params))
    {
        let ffi_type = inst_kind_to_ffi_type(k1, InstKind::Value(*arg_pt));
        ffi_args_types_storage.push(ffi_type);
        let type_addr: &mut ffi_type = ffi_args_types_storage.last_mut().unwrap();
        ffi_args_types_ptrs.push(type_addr as *mut ffi_type);

        // We need a stable-ish address to each Value here; so we push them to
        // a parallel collection
        let vm_value = vm::resolve_value(k1, vm, frame_index, inst_offset, *arg_value);

        ffi_args_value_storage.push(vm_value.bits());

        // Now we have an address to push into the actual args array
        let ffi_arg_addr = ffi_args_value_storage.last_mut().unwrap();
        ffi_args_value_ptrs.push(ffi_arg_addr as *mut _ as *mut c_void)
    }
    let name_cstr = std::ffi::CString::new(k1.ident_str(fn_name)).unwrap();
    let handle_for_search = match lib_name {
        None => k1.vm_process_dlopen_handle,
        Some(lib_name) => k1.get_dlopen_handle(lib_name, vm.eval_span)?,
    };
    let fn_ptr: *mut c_void = unsafe { libc::dlsym(handle_for_search, name_cstr.as_ptr()) };
    if fn_ptr.is_null() {
        return failf!(
            vm.eval_span,
            "Could not find extern function symbol: {}",
            k1.ident_str(fn_name)
        );
    }

    // eprintln!("ffi args were: {:?}", ffi_args_value_storage);
    // eprintln!("ffi args types were: {:?}", ffi_args_types_storage);
    // eprintln!("ffi args types ptrs were: {:?}", ffi_args_types_ptrs);

    let mut cif: ffi_cif = ffi_cif::default();
    let atypes: *mut *mut ffi_type = ffi_args_types_ptrs.as_mut_ptr().cast();
    let result = unsafe {
        libffi::low::prep_cif(&mut cif, ffi_abi_FFI_DEFAULT_ABI, nargs, &mut ffi_ret_type, atypes)
            .map_err(|e| errf!(vm.eval_span, "failed to set up ffi context: {:?}", e))?;
        let args = ffi_args_value_ptrs.as_slice_mut().as_mut_ptr();
        let result: Value = libffi::low::call(&mut cif, CodePtr(fn_ptr), args);
        result
    };

    eprintln!("result is: {}", result);
    Ok(result)
}

fn inst_kind_to_ffi_type(k1: &TypedProgram, inst_kind: InstKind) -> libffi::low::ffi_type {
    use libffi::low;
    unsafe {
        match inst_kind {
            InstKind::Value(PhysicalType::Scalar(ScalarType::U8)) => types::uint8,
            InstKind::Value(PhysicalType::Scalar(ScalarType::U16)) => types::uint16,
            InstKind::Value(PhysicalType::Scalar(ScalarType::U32)) => types::uint32,
            InstKind::Value(PhysicalType::Scalar(ScalarType::U64)) => types::uint64,
            InstKind::Value(PhysicalType::Scalar(ScalarType::I8)) => types::sint8,
            InstKind::Value(PhysicalType::Scalar(ScalarType::I16)) => types::sint16,
            InstKind::Value(PhysicalType::Scalar(ScalarType::I32)) => types::sint32,
            InstKind::Value(PhysicalType::Scalar(ScalarType::I64)) => types::sint64,
            InstKind::Value(PhysicalType::Scalar(ScalarType::F32)) => types::float,
            InstKind::Value(PhysicalType::Scalar(ScalarType::F64)) => types::double,
            InstKind::Value(PhysicalType::Scalar(ScalarType::Pointer)) => types::pointer,
            InstKind::Void => low::types::void,
            InstKind::Terminator => low::types::void,
            InstKind::Value(PhysicalType::Agg(_agg_id)) => {
                todo!("FFI aggregates")
            }
        }
    }
}
