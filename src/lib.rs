#![feature(rustc_private)]
extern crate getopts;
#[macro_use] extern crate log;
extern crate rustc;
extern crate rustc_driver;
extern crate syntax;

use rustc_driver::{driver, CompilerCalls, Compilation, RustcDefaultCalls};
use rustc::session::Session;
use rustc::session::config::{self, Input};
use rustc::middle::ty;

// Impressive that there are both `diagnostic` and `diagnostics` modules
use syntax::{ast, ast_map, ast_util, codemap, diagnostic, diagnostics, visit};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::env::Args;
use std::path::PathBuf;

use clean::{Arg, Clean, FnDecl, Return, Struct};

mod cdecl;
mod clean;

struct CustardCalls {
    default_calls: RustcDefaultCalls
}

impl CustardCalls {
    fn new() -> Self {
        CustardCalls {
            default_calls: RustcDefaultCalls
        }
    }
}

impl<'a> CompilerCalls<'a> for CustardCalls {
    fn early_callback(&mut self,
                      _: &getopts::Matches,
                      _: &diagnostics::registry::Registry) -> Compilation {
        Compilation::Continue
    }

    fn late_callback(&mut self,
                     m: &getopts::Matches,
                     s: &Session,
                     i: &Input,
                     odir: &Option<PathBuf>,
                     ofile: &Option<PathBuf>) -> Compilation {
        self.default_calls.late_callback(m, s, i, odir, ofile);
        Compilation::Continue
    }

    fn no_input(&mut self,
                _: &getopts::Matches,
                _: &config::Options,
                _: &Option<PathBuf>,
                _: &Option<PathBuf>,
                _: &diagnostics::registry::Registry) -> Option<(Input, Option<PathBuf>)> {
        None
    }

    fn build_controller(&mut self,
                        _: &Session) -> driver::CompileController<'a> {
        let mut ctrl = driver::CompileController::basic();
        ctrl.after_analysis.stop = Compilation::Stop;
        ctrl.after_analysis.callback = Box::new(|state| {
            let tcx = state.tcx.unwrap();
            let krate = state.expanded_crate.unwrap();
        });
        ctrl
    }

}
pub fn run_core(args: Args) {
    let args = args.collect::<Vec<_>>();
    rustc_driver::run_compiler(&args, &mut CustardCalls::new());
}

struct CFnDeclVisitor<'tcx> {
    funcs: Vec<FnDecl>,
    types: HashSet<ast::NodeId>,
    tcx: &'tcx ty::ctxt<'tcx>
}

impl<'tcx> CFnDeclVisitor<'tcx> {
    fn new<'a>(tcx: &'a ty::ctxt<'a>) -> CFnDeclVisitor<'a> {
        CFnDeclVisitor {
            funcs: vec![],
            types: HashSet::new(),
            tcx: tcx
        }
    }
}

struct CTypeVisitor<'tcx> {
    types: HashSet<ast::NodeId>,
    structs: Vec<Struct>,
    tcx: &'tcx ty::ctxt<'tcx>
}

impl<'tcx> CTypeVisitor<'tcx> {
    fn new<'a>(tcx: &'a ty::ctxt<'a>, types: HashSet<ast::NodeId>) -> CTypeVisitor<'a> {
        CTypeVisitor {
            types: types,
            structs: vec![],
            tcx: tcx
        }
    }
}

impl<'a> visit::Visitor<'a> for CFnDeclVisitor<'a> {
    fn visit_item(&mut self, item: &'a ast::Item) {
        use syntax::abi::Abi;
        use syntax::ast::Item_::ItemFn;
        use clean::Type::ResolvedPath;

        // FIXME: once rust-lang/rust#19834, can replace this with just
        // denying the improper_ctypes lint
        fn check_ty(tcx: &ty::ctxt, node: ast::NodeId) -> bool {
            let tty = match tcx.ast_ty_to_ty_cache.borrow().get(&node) {
                Some(tty) => tty.clone(),
                _ => panic!("ty missing from attt cache")
            };
            ty::is_ffi_safe(tcx, tty)
        }

        if item.vis != ast::Visibility::Public { return }

        if let ItemFn(ref fn_decl, _, Abi::C, _, _) = item.node {
            debug!("ItemFn {}", item.ident.as_str());

            for &ast::Arg { ref ty, .. } in fn_decl.inputs.iter() {
                if !check_ty(self.tcx, ty.id) {
                    self.tcx.sess.span_fatal(ty.span, "Type is not FFI safe.");
                }
            }
            if let ast::FunctionRetTy::Return(ref ty) = fn_decl.output {
                if !check_ty(self.tcx, ty.id) {
                    self.tcx.sess.span_fatal(ty.span, "Type is not FFI safe.");
                }
            }

            let func: FnDecl = item.clean(self.tcx);
            for &Arg { ref ty, .. } in func.inputs.iter() {
                if let &ResolvedPath { did, .. } = ty {
                    if ast_util::is_local(did) {
                        self.types.insert(did.node);
                    }
                }
            }
            if let Return {
                ty: ResolvedPath { did, .. }, ..
            } = func.output {
                if ast_util::is_local(did) {
                    self.types.insert(did.node);
                }
            }

            self.funcs.push(func);
        }
    }
}

impl<'a> visit::Visitor<'a> for CTypeVisitor<'a> {
    fn visit_item(&mut self, item: &'a ast::Item) {
        use syntax::ast::Item_::ItemStruct;
        if !self.types.contains(&item.id)  {
            return
        }

        match item.node {
            ItemStruct(_, _) => {
                let s = item.clean(self.tcx);
                self.structs.push(s)
            },
            _ => {}
        }
    }
}
