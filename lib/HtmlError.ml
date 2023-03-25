open Span
open Util
open Error
open Identifier
open SourceContext
open ErrorText

let prefix = "
<!DOCTYPE html>
<html>
<head>
    <meta charset='utf-8'>
    <title>Error Report</title>
</head>
<body>
    <input type='checkbox' id='darkmode-toggle'>
    <label for='darkmode-toggle' id='darkmode-toggle-lbl'>Dark Mode</label>
    <div id='container'>
"

let css = "
html, body {
    margin: 0px;
    padding: 0px;
    color: var(--fg);
    height: 100%;
}

:root {
    --bg: #ffffff;
    --fg: #000000;
    --code-fg: #000;
    --line-hl: #00000011;
    --code-bg: #eee;
    --code-line-num: #ccc;
    --caption-bg: #bbb;
    --error-bg: #FF000022;
}


#darkmode-toggle {
    display: none;
}

#darkmode-toggle-lbl {
    position: absolute;
    right: 10px;
    top: 10px;
    padding: 10px;
    border-radius: 5px;
    background-color: #ccc;
    user-select: none;
}

#darkmode-toggle:checked ~ #darkmode-toggle-lbl {
    color: white;
    background-color: #222;
}

#darkmode-toggle:checked ~ #container {
    --bg: #111;
    --fg: #eee;
    --line-hl: #ffffff11;
    --code-fg: #eee;
    --code-bg: #222;
    --code-line-num: #333;
    --caption-bg: #444;
    --error-bg: #FF000044;
}

#container {
    color: var(--fg);
    background-color: var(--bg);
    min-height: 100%;
}

h1 {
    padding-top: 0.67em;
    padding-bottom: 0.67em;
    margin: 0px;
}

caption {
    background-color: var(--caption-bg);
    padding: 5px 5px 5px 5px;
    text-align: left;
    border-radius: 10px 10px 0px 0px;
}

.code_table {
    font-family: monospace;
    width: 100%;
    background-color: var(--code-bg);
    color: var(--code-fg);
    border-spacing: 0px;
}

.code_table .lines {
    background-color: var(--code-line-num);
    text-align: right;
}

.code_table .line {
    text-align: right;
    padding-right: 5px;
    width: 30px;
    user-select: none; /* so that copying code from the block is easier */
    vertical-align: top;
}

.code_table tr:hover {
    background-color: var(--line-hl);
}

.code_table pre {
    margin: 0px;
    text-align: left;
}

.code_table .error-hl {
    text-decoration: underline;
    text-decoration-color: red;
    background-color: var(--error-bg);
}

pre {
    white-space: pre-wrap;
}

#left {
    width: 40%;
}

#right {
    width: 60%;
}


#main {
    margin: auto;
    width: 90%;
}

#split {
    display: flex;
    flex-direction: row;
    gap: 5%;
}

code {
    background-color: var(--code-bg);
    color: var(--code-fg);
    border-radius: 5px;
    padding: 0px 5px 0px 5px;
}

footer {
    margin: auto;
    display: flex;
    flex-direction: column;
    align-items: center;
}

#footer-list {
    display: flex;
    margin: auto;
    flex-wrap: wrap;
    gap: 10px
}

#footer-list a {
    padding: 10px;
    background-color: var(--caption-bg);
    border-radius: 10px;
    color: var(--fg);
}"

let suffix = "
    </div> 
    <style>" ^ css ^ "</style>
</body>
</html>
"


let sanatize_string (to_be_injected: string): string = 
    let rep = function
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '&' -> "&amp;"
        | n -> String.make 1 n
    in
    String.concat "" (List.map rep (List.of_seq (String.to_seq to_be_injected)))

let module_name_to_text (module_name: module_name option): string =
    match module_name with
    | Some mn ->
        sanatize_string (mod_name_string mn)
    | None ->
        "[unknown module]"


(* determines how much of a line needs to be in error based on span and return it *)
let line_err (line: string) (line_num: int) (span: span option): string =
    (* quick way to destructure both without reusing var names
       there may be a better way, however i dont know ocaml well enough
       to do that*)
    let destructure_pos (p: position): int * int =
        let (Position {line; column}) = p in
        (line, column)
    in
    match span with
    | Some (Span { startp; endp; _ }) ->
        let (startl, startc) = destructure_pos(startp)
        and (endl, endc) = destructure_pos(endp) in
        if line_num < startl || line_num > endl then
            line
        else
            let startbreak = if startl = line_num then startc else 0
            (* todo replace else 0 with line length *)
            and endbreak = if endl = line_num then endc else (String.length line) in
            (String.sub line 0 startbreak)
            ^ "<span class='error-hl'>"
            ^ (String.sub line startbreak (endbreak - startbreak))
            ^ "<span>"
            ^ (String.sub line endbreak ((String.length line) - endbreak))
    | None ->
        line

(* Takes a source context and a span (needed to draw the error location and generates the html table for the lines with a html span(s) for the error 
 the output has already been sanitized *)
let code_to_html_table (span: span option) (ctx: source_ctx option): string =
    let caption =
        match span with
        | Some (Span { filename; _ }) ->
            sanatize_string filename
        | None ->
            "[Unknown file]"
    in
    let table_rows: string list =
        match ctx with
        | Some context ->
            let SourceContext lines = context in
            List.map (fun (line_num, line) ->
                (* the <br> is required so that the empty lines in the codeblock are copy-pasteable*)
                "<tr>
                    <th class='line'>" ^ string_of_int(line_num) ^ "</th>
                    <th><pre>" 
                        ^ (line_err line line_num span)
                        ^ "<br></pre></th>
                </tr>"
                    )
            lines
        | None -> [""]
    in
   "<table class='code_table'>
        <caption>" ^ caption ^ "</caption>
        <colgroup>
            <col class='lines'>
        </colgroup>
        <tbody>" ^ String.concat "\n" table_rows ^ "</tbody>
    </table>"



let render_error_to_html (error: austral_error): string =
    let (AustralError { span; kind; text; source_ctx; module_name }) = error
    in
    let main: string = "
    <div id='main'>
        <h1>Error Report</h1>
        <div id='split'>
            <div id='left'>
                <h3><code>" ^  sanatize_string (error_title kind) ^ "</code>
                     in module <code>" 
                        ^ sanatize_string (module_name_to_text module_name) 
                        ^ "</code></h3>
                <pre id='description'>" 
                    ^ error_text_to_html text
                    ^ "</pre>
            </div>

            <div id='right'>
                " ^ code_to_html_table span source_ctx ^ "
            </div>
        </div>
    </div>
    <footer>
        <h3>Quick Links</h3>
        <div id='footer-list'>
            <a target='_blank' href='./calltree.html'>
                Compiler Call Tree</a>
            <a target='_blank' href='http://austral-lang.org'>
                austral-lang.org</a>
            <a target='_blank' href='http://austral-lang.org/spec/spec.html'>
                Specification</a>
            <a target='_blank' href='http://github.com/austral/austral'>
                Language Source</a>
        <div>
    </footer>"
    in
    prefix ^ main ^ suffix

let html_error_dump (error: austral_error): unit =
    let filename: string = "error.html" in
    Printf.eprintf "Wrote error to %s\n" filename;
    write_string_to_file filename (render_error_to_html error)

