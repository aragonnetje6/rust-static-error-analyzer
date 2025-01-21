use rustc_hir::def_id::DefId;
use rustc_middle::ty::{GenericArg, Ty, TyCtxt, TyKind};

/// Extracts the return type of a called function using just the function's `DefId`.
/// Should always succeed.
fn get_call_type_using_context(context: TyCtxt, called_id: DefId) -> Ty {
    let ty = context.type_of(called_id).instantiate_identity();
    if ty.is_fn() {
        context
            .fn_sig(called_id)
            .instantiate_identity()
            .skip_binder()
            .output()
    } else if let TyKind::Closure(_, args) = ty.kind() {
        args.as_closure().sig().skip_binder().output()
    } else if let TyKind::CoroutineClosure(_, args) = ty.kind() {
        args.as_coroutine_closure()
            .coroutine_closure_sig()
            .skip_binder()
            .return_ty
    } else {
        ty
    }
}

/// Extract the error type from Result, or return the full type if it doesn't contain a Result (along with a flag of whether it is an extract error).
#[allow(clippy::similar_names)]
pub fn get_error_or_type(context: TyCtxt, called_id: DefId) -> (String, bool) {
    let ret_ty = get_call_type_using_context(context, called_id);

    let result = if context.ty_is_opaque_future(ret_ty) {
        extract_result_from_future(context, ret_ty)
    } else {
        extract_result(ret_ty)
    };

    let res = extract_error_from_result(result);

    (res.clone().unwrap_or(ret_ty.to_string()), res.is_some())
}

/// Extract the Result type from any type.
fn extract_result(ty: Ty) -> Option<GenericArg> {
    for arg in ty.walk() {
        let format = format!("{arg}");
        if format.starts_with("std::result::Result<") && format.ends_with('>') {
            return Some(arg);
        }
    }

    None
}

/// Extract the Result type from any future.
fn extract_result_from_future<'a>(context: TyCtxt<'a>, ty: Ty<'a>) -> Option<GenericArg<'a>> {
    for t in ty.walk() {
        if let Some(typ) = t.as_type() {
            if let TyKind::Alias(_kind, alias) = typ.kind() {
                if let TyKind::Coroutine(_def_id, args) =
                    context.type_of(alias.def_id).instantiate_identity().kind()
                {
                    for arg in *args {
                        let format = format!("{arg}");
                        if format.starts_with("std::result::Result<") && format.ends_with('>') {
                            return Some(arg);
                        }
                    }
                }
            }
        }
    }

    None
}

/// Extract the error from a Result type.
fn extract_error_from_result(opt: Option<GenericArg>) -> Option<String> {
    let t = opt?;
    for arg in t.walk() {
        let f = format!("{arg}");
        if format!("{t}").ends_with(&format!(", {f}>")) {
            return Some(f);
        }
    }

    None
}
