use std::{
    ffi::{CStr, CString},
    os::raw::c_char,
};

use llvm_sys::{
    core::{LLVMContextDispose, LLVMDisposeBuilder, LLVMDisposeModule, LLVMSetInitializer},
    prelude::{LLVMTypeRef, LLVMValueRef},
    LLVMBuilder, LLVMContext, LLVMModule,
};

use crate::{ast::TypedASTNode, symbol_table::SymbolTable, types::Type};

extern crate llvm_sys as llvm;

pub fn compile(root: TypedASTNode) {
    unsafe {
        let context = llvm::core::LLVMContextCreate();
        let module = llvm::core::LLVMModuleCreateWithName(cstr("main").as_ptr());
        let builder = llvm::core::LLVMCreateBuilderInContext(context);
        let mut table: SymbolTable<LLVMValueRef> = SymbolTable::new();

        build_module(root, context, builder, module, &mut table);
        llvm::core::LLVMDumpModule(module);
        println!("dumped module");

        LLVMDisposeBuilder(builder);
        LLVMDisposeModule(module);
        LLVMContextDispose(context);
    }
}

fn cstr(s: &str) -> CString {
    let f = CString::new(s).expect("CString::new failed");
    return f;
}

fn build_module(
    node: TypedASTNode,
    context: *mut LLVMContext,
    builder: *mut LLVMBuilder,
    module: *mut LLVMModule,
    table: &mut SymbolTable<LLVMValueRef>
) -> LLVMValueRef {
    unsafe {
        match node {
            TypedASTNode::Identifier {
                name,
                node_type,
                location,
            } => {
                table.lookup(&name).unwrap()
            },
            TypedASTNode::VariableDeclaration {
                variable,
                value,
                node_type,
                location: _,
            } => {
                println!("building declaration");
                let ty = generate_llvm_type(&value.get_type());
                let val = build_module(*value, context, builder, module, table);
                table.insert(&variable, val);
                println!("built val and type");
                let global = llvm::core::LLVMAddGlobal(module, ty, cstr(&variable).as_ptr());
                LLVMSetInitializer(global, val);
                println!("added global");

                val
            }
            TypedASTNode::Declarations {
                declarations,
                node_type,
                location: _,
            } => {
                for dec in declarations {
                    build_module(dec, context, builder, module, table);
                }
                let ty = llvm::core::LLVMVoidType();
                llvm::core::LLVMConstNull(ty)
            }
            TypedASTNode::Int(v, _) => {
                println!("building int");
                let ty = llvm::core::LLVMInt32TypeInContext(context);
                let val = llvm::core::LLVMConstInt(ty, v as u64, 1);
                val
            }
            TypedASTNode::Float(v, _) => {
                let ty = llvm::core::LLVMFloatType();
                let val = llvm::core::LLVMConstReal(ty, v);
                val
            }
            TypedASTNode::Bool(v, _) => {
                let ty = llvm::core::LLVMInt1Type();
                let val = llvm::core::LLVMConstInt(ty, v as u64, 0);
                val
            }
            TypedASTNode::Unary {
                node_type,
                op,
                expr,
                location,
            } => {
                let ty = generate_llvm_type(&expr.get_type());
                match *expr {
                    TypedASTNode::Int(v, _) => {
                        let val = llvm::core::LLVMConstInt(ty, -v as u64, 1);
                        val
                    }
                    TypedASTNode::Float(v, _) => {
                        let val = llvm::core::LLVMConstReal(ty, -v);
                        val
                    }
                    _ => panic!(),
                }
            }
            TypedASTNode::Binary {
                node_type,
                op,
                lhs,
                rhs,
                location,
            } => match op {
                crate::ast::Operator::Add => {
                    let left = build_module(*lhs, context, builder, module, table);
                    let right = build_module(*rhs, context, builder, module, table);
                    llvm::core::LLVMBuildAdd(builder, left, right, cstr("").as_ptr())
                }
                crate::ast::Operator::Subtract => {
                    let left = build_module(*lhs, context, builder, module, table);
                    let right = build_module(*rhs, context, builder, module, table);
                    llvm::core::LLVMBuildSub(builder, left, right, cstr("").as_ptr())
                }
                crate::ast::Operator::Divide => {
                    let left = build_module(*lhs, context, builder, module, table);
                    let right = build_module(*rhs, context, builder, module, table);
                    llvm::core::LLVMBuildSDiv(builder, left, right, cstr("").as_ptr())
                }
                crate::ast::Operator::Multiply => {
                    let left = build_module(*lhs, context, builder, module, table);
                    let right = build_module(*rhs, context, builder, module, table);
                    llvm::core::LLVMBuildMul(builder, left, right, cstr("").as_ptr())
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

fn generate_llvm_type(typ: &Type) -> LLVMTypeRef {
    unsafe {
        match typ {
            Type::Int => llvm::core::LLVMInt32Type(),
            Type::Float => llvm::core::LLVMFloatType(),
            Type::Bool => llvm::core::LLVMInt1Type(),
            Type::Tuple(v) => {
                let num_items = v.len();
                let mut type_list: Vec<_> = v.iter().map(generate_llvm_type).collect::<_>();

                llvm::core::LLVMStructType(
                    type_list.as_mut_ptr() as *mut LLVMTypeRef,
                    num_items as u32,
                    0_i32,
                )
            }
            _ => llvm::core::LLVMVoidType(),
        }
    }
}

pub fn compile_node() {}
