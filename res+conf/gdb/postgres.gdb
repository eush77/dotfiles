handle SIGUSR1 nostop noprint

define explain
    set $exp = NewExplainState()
    if $argc == 0
        call ExplainPrintPlan($exp, queryDesc)
    else
        call ExplainPrintPlan($exp, $arg0)
    end
    printf "%s", $exp->str.data
end
document explain
explain [QUERYDESC]

Print SQL explain output for QueryDesc instance.
QUERYDESC defaults to the variable `queryDesc`.
end

define plist
    if $arg0
        set $cell = $arg0->head
        while $cell
            print $cell->data
            set $cell = $cell->next
        end
    end
end
document plist
plist <LIST>

Print List* cells in order.
end

define varsize
    print (((varattrib_4b *) ($arg0))->va_4byte.va_header & 0x3FFFFFFF)
end

define vardata
    print (((varattrib_4b *) ($arg0))->va_4byte.va_data)
end
