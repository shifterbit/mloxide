

use llvm_sys::{prelude::{LLVMTypeRef, LLVMValueRef}, LLVMBuilder, LLVMModule};

use crate::{ast::TypedASTNode, types::Type};

extern crate llvm_sys as llvm;




pub fn compile(root: TypedASTNode) {
    unsafe {
        let context = llvm::core::LLVMContextCreate();
        let module = llvm::core::LLVMModuleCreateWithName(gen_cstr("main"));
        let builder = llvm::core::LLVMCreateBuilderInContext(context);
        build_module(root, builder, module);
    }
}

fn gen_cstr(s: &str) -> *const i8 {
    let module_id: *const i8 = s.as_ptr() as *const _;
    module_id
}


fn build_module(node: TypedASTNode, builder: *mut LLVMBuilder, module: *mut LLVMModule) -> LLVMValueRef  {
    unsafe {
    match node {
        TypedASTNode::VariableDeclaration { variable, value, node_type, location: _ } => {
            let ty = generate_llvm_type(&node_type);
            let var = llvm::core::LLVMBuildAlloca(builder, ty, gen_cstr(&variable));
            let llvm_value = build_module(*value, builder, module);
            let _ = llvm::core::LLVMBuildStore(builder, llvm_value, var);
            var
        },
        TypedASTNode::Declarations { declarations, node_type, location: _ } => {
            for dec in declarations {
                build_module(dec, builder, module);
            }
            let ty = generate_llvm_type(&node_type);
            llvm::core::LLVMConstNull(ty)
        },
        TypedASTNode::Int(v, _) => {
            let ty = llvm::core::LLVMInt64Type();
            let val = llvm::core::LLVMConstInt(ty, v as u64, 1);
            val
        },
        TypedASTNode::Float(v, _) => {
            let ty = llvm::core::LLVMFloatType();
            let val = llvm::core::LLVMConstReal(ty, v);
            val
            
        },
        TypedASTNode::Bool(v, _) => {
            let ty = llvm::core::LLVMInt1Type();
            let val = llvm::core::LLVMConstInt(ty, v as u64, 0);
            val
        },
        _ => todo!(),
    }
    }
    
    
}

fn generate_llvm_type(typ: &Type) -> LLVMTypeRef  {
    unsafe {
        
    match typ {
        Type::Int => llvm::core::LLVMInt64Type(),
        Type::Float => llvm::core::LLVMFloatType(),
        Type::Bool => llvm::core::LLVMInt1Type(),
        Type::Tuple(v) =>  {
            let num_items = v.len();
            let mut  type_list: Vec<_> = v.iter().map(generate_llvm_type).collect::<_>();
            
            llvm::core::LLVMStructType(
                type_list.as_mut_ptr() as *mut LLVMTypeRef,
                num_items as u32,
                0_i32)
        },
        _ => llvm::core::LLVMVoidType(),
    }
    }
}

pub fn compile_node() {
    
}