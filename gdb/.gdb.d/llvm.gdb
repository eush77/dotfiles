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

define pllvmlist
  if $arg0
    set $cell = $arg0->head
    while $cell
      llv $cell->data.ptr_value
      set $cell = $cell->next
    end
  end
end

define pllvmlist-blocks
  if $arg0
    set $cell = $arg0->head
    while $cell
      llv LLVMGetInstructionParent($cell->data.ptr_value)
      set $cell = $cell->next
    end
  end
end
