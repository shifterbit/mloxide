use std::ffi::CString;

use llvm_sys::{
    bit_writer::LLVMWriteBitcodeToFile,
    core::{LLVMContextDispose, LLVMDisposeBuilder, LLVMDisposeModule, LLVMSetInitializer},
    prelude::{LLVMTypeRef, LLVMValueRef},
    LLVMBuilder, LLVMContext, LLVMModule,
};

use crate::{ast::TypedASTNode, types::Type};

extern crate llvm_sys as llvm;

pub fn compile(root: TypedASTNode) {
    unsafe {
        let context = llvm::core::LLVMContextCreate();
        let module = llvm::core::LLVMModuleCreateWithName(cstr("main").as_ptr());
        let builder = llvm::core::LLVMCreateBuilderInContext(context);

        build_module(root, context, builder, module);
        llvm::core::LLVMDumpModule(module);
        println!("dumped module");
        LLVMWriteBitcodeToFile(module, cstr("main").as_ptr());
        LLVMDisposeBuilder(builder);
        LLVMDisposeModule(module);
        LLVMContextDispose(context);
    }
}

fn cstr(s: &str) -> CString {
    CString::new(s).expect("CString::new failed")
}

fn build_module(
    node: TypedASTNode,
    context: *mut LLVMContext,
    builder: *mut LLVMBuilder,
    module: *mut LLVMModule,
) -> LLVMValueRef {
    unsafe {
        match node {
            TypedASTNode::Identifier {
                name,
                node_type: _,
                location: _,
            } => {
                
                llvm::core::LLVMGetNamedGlobal(module, cstr(&name).as_ptr())
            }
            TypedASTNode::VariableDeclaration {
                variable,
                value,
                node_type: _,
                location: _,
            } => {
                println!("building declaration");
                let ty = generate_llvm_type(&value.get_type());
                let val = build_module(*value, context, builder, module);
                println!("built val and type");
                let global = llvm::core::LLVMAddGlobal(module, ty, cstr(&variable).as_ptr());
                LLVMSetInitializer(global, val);
                println!("added global");

                val
            }
            TypedASTNode::Declarations {
                declarations,
                node_type: _,
                location: _,
            } => {
                for dec in declarations {
                    build_module(dec, context, builder, module);
                }
                let ty = llvm::core::LLVMVoidType();
                llvm::core::LLVMConstNull(ty)
            }
            TypedASTNode::Int(v, _) => {
                println!("building int");
                let ty = llvm::core::LLVMInt32TypeInContext(context);
                
                llvm::core::LLVMConstInt(ty, v as u64, 1)
            }
            TypedASTNode::Float(v, _) => {
                let ty = llvm::core::LLVMFloatType();
                
                llvm::core::LLVMConstReal(ty, v)
            }
            TypedASTNode::Bool(v, _) => {
                let ty = llvm::core::LLVMInt1Type();
                
                llvm::core::LLVMConstInt(ty, v as u64, 0)
            }
            TypedASTNode::Unary {
                node_type: _,
                op:_,
                expr,
                location: _,
            } => {
                let ty = generate_llvm_type(&expr.get_type());
                match *expr {
                    TypedASTNode::Int(v, _) => {
                        
                        llvm::core::LLVMConstInt(ty, -v as u64, 1)
                    }
                    TypedASTNode::Float(v, _) => {
                        
                        llvm::core::LLVMConstReal(ty, -v)
                    }
                    _ => panic!(),
                }
            }
            TypedASTNode::Binary {
                node_type: _,
                op,
                lhs,
                rhs,
                location: _,
            } => match op {
                crate::ast::Operator::Add => {
                    let left = build_module(*lhs, context, builder, module);
                    let right = build_module(*rhs, context, builder, module);
                    llvm::core::LLVMBuildAdd(builder, left, right, cstr("").as_ptr())
                }
                crate::ast::Operator::Subtract => {
                    let left = build_module(*lhs, context, builder, module);
                    let right = build_module(*rhs, context, builder, module);
                    llvm::core::LLVMBuildSub(builder, left, right, cstr("").as_ptr())
                }
                crate::ast::Operator::Divide => {
                    let left = build_module(*lhs, context, builder, module);
                    let right = build_module(*rhs, context, builder, module);
                    llvm::core::LLVMBuildSDiv(builder, left, right, cstr("").as_ptr())
                }
                crate::ast::Operator::Multiply => {
                    let left = build_module(*lhs, context, builder, module);
                    let right = build_module(*rhs, context, builder, module);
                    llvm::core::LLVMBuildMul(builder, left, right, cstr("").as_ptr())
                }
                _ => todo!(),
            },
            TypedASTNode::Tuple {
                exprs,
                node_type:_ ,
                location: _,
            } => {
                let mut val: Vec<_> = exprs
                    .iter()
                    .map(|node| build_module(node.clone(), context, builder, module))
                    .collect::<_>();
                llvm::core::LLVMConstStruct(
                    val.as_mut_ptr() as *mut LLVMValueRef,
                    val.len() as u32,
                    0,
                )
            }
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
