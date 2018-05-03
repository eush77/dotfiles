set disassembly-flavor intel
set history save
set history filename ~/.gdb_history
set history size unlimited
set debug-file-directory /usr/lib/debug

set listsize 30

py import re

alias rf = reverse-finish

define list-start
    py print(re.sub(r'^(=> [^:]*:\s*)(\S+)', lambda m: m.group(1) + m.group(2).upper(), gdb.execute("x/{}i $pc".format(gdb.parameter("listsize")), to_string=True), flags=re.MULTILINE))
end
document list-start
list-start

List instructions starting from the program counter ($pc).
end
alias ls = list-start

define list-instructions
    py print(re.sub(r'^(=> [^:]*:\s*)(\S+)', lambda m: m.group(1) + m.group(2).upper(), gdb.execute("x/{}i $pc - 0x30".format(gdb.parameter("listsize")), to_string=True), flags=re.MULTILINE))
end
document list-instructions
list-instructions

List instructions around program counter ($pc).
end
alias li = list-instructions
define l
    list-instructions
end
document list-instructions
list-instructions

List instructions around program counter ($pc).
end

define nextc
    py arch = gdb.selected_frame().architecture()
    py pc = lambda: int(gdb.selected_frame().pc())
    py inst = lambda: arch.disassemble(pc())[0]["asm"].split()
    py isstop = lambda inst: inst[0] in ("call", "ret")
    py gdb.execute("nexti", to_string=True)
    py while not isstop(inst()): gdb.execute("nexti", to_string=True)
    list-instructions
    py iscall = inst()[0] == "call"
    py if iscall: print("Callee:")
    py if iscall: gdb.execute("x/i " + \
        ("$" if inst()[1][:2] != "0x" else "") + inst()[1])
end
document nextc
nextc

Continue until next call or return.
end
alias nc = nextc

define reverse-nextc
    py arch = gdb.selected_frame().architecture()
    py pc = lambda: int(gdb.selected_frame().pc())
    py inst = lambda: arch.disassemble(pc())[0]["asm"].split()
    py isstop = lambda inst: inst[0] in ("call", "ret")
    py gdb.execute("reverse-nexti", to_string=True)
    py while not isstop(inst()): gdb.execute("reverse-nexti", to_string=True)
    list-instructions
    py iscall = inst()[0] == "call"
    py if iscall: print("Callee:")
    py if iscall: gdb.execute("x/i " + \
        ("$" if inst()[1][:2] != "0x" else "") + inst()[1])
end
document reverse-nextc
reverse-nextc

Continue backward until next call or return.
end
alias rnc = reverse-nextc

define ni
    nexti
    list-instructions
end

define rni
    reverse-nexti
    list-instructions
end

define si
    stepi
    list-start
end

define rsi
    reverse-stepi
    list-instructions
end

#source ~/.gdb.d/glibc.gdb
source ~/.gdb.d/llvm.gdb
source ~/.gdb.d/postgres.gdb
