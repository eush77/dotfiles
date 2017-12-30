define llvm-ptype
    print (llvm::Value::ValueTy) $arg0->getValueID()
end
document llvm-ptype
llvm-ptype <value>

Print dynamic value type of LLVMValueRef instance, using
`Value::getValueID`.
end
alias llt = llvm-ptype

define llvm-dump-value
    call LLVMDumpValue($arg0)
end
document llvm-dump-value
llvm-dump-value <value>

Dump value of an LLVMValueRef instance.
end
alias llv = llvm-dump-value

define llvm-value-name
    print LLVMGetValueName($arg0)
end
document llvm-value-name
llvm-value-name <value>

Print name of an LLVMValueRef instance.
end
alias lln = llvm-value-name
