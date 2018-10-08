BEGIN { 
    ORS = ""
    print "structure CppPrelude = struct\n\
    val prelude = \""
}

{
    gsub(/\"/, "\\\"");
    printf "%s\\n", $0
}

END {
    print "\";\nend"
}
