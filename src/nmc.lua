local pp = require('src.pretty').pformat

local M = {}

--[[

local LEX_START = 1
local LEX_STRING_0 = 2
local LEX_STRING_1 = 3

local TOK_LPAREN = 1
local TOK_RPAREN = 2
local TOK_STRING = 3

local LexStates = {
    [LEX_START] = {
        ['('] = {LEX_START, TOK_LPAREN, 0},
        [')'] = {LEX_START, TOK_RPAREN, 0},
        [' '] = {LEX_START},
        ['\t'] = {LEX_START},
        ['\n'] = {LEX_START},
        ['\''] = {LEX_STRING_0}
    },
    [LEX_STRING_0] = { --
        ['\''] = {LEX_START, TOK_STRING},
        ['\\'] = {LEX_STRING_1},
        function(char) return {LEX_STRING_0} end
    },
    [LEX_STRING_1] = { --
        function(char) return {LEX_STRING_0} end
    }
}

local lex_iterator = function(state)
    local reader = state.reader
    local partial_tok = {}
    for char in reader:bytes() do
        local handler = LexStates[state.state][char]
        if not handler and LexStates[state.state][1] then
            handler = LexStates[state.state][1](char)
        end
        if not handler then
            io.stderr:write(string.format(
                                'lexer: %d:%d: unexpected character \'%s\'.\n',
                                reader.linum, reader.colnum, char))
            os.exit(1)
        end

        local old_state = state.state

        local tok_tag = nil
        state.state, tok_tag = handler[1], handler[2]

        if old_state == LEX_START and state.state ~= LEX_START then
            partial_tok.start = reader:get_pos()
        end

        if tok_tag ~= nil then
            partial_tok.stop = reader:get_pos()

            return tok_tag, reader:get_span(partial_tok.start, partial_tok.stop)
        end
    end

    if state.state == LEX_START then
        return nil
    else
        io.stderr:write('lexer: unexpected end of file.\n')
        os.exit(1)
    end
end

local lex = function(reader)
    local state = {reader = reader, state = LEX_START}
    return lex_iterator, state, nil
end
]]

local enum = function(t)
    local ret = {}
    for i, n in ipairs(t) do ret[n] = i end
    return ret
end

-- AST node types
local N = enum {'ASGN', 'ASGN_LET', 'ASGN_CONST', 'VAR', 'NUMBER', 'FUNC'}

local lpeg = require 'lpeg'
local parse_errors = {pos = -1, toks = {}}
--[[
local grammar
do
    local V, P, R, S, C, Cp, Ct, Cc = lpeg.V, lpeg.P, lpeg.R, lpeg.S, lpeg.C,
                                      lpeg.Cp, lpeg.Ct, lpeg.Cc
    local E = function(tok)
        return lpeg.Cmt(true, function(_, pos)
            if parse_errors.pos == pos then
                parse_errors.toks[tok] = true
            elseif parse_errors.pos < pos then
                parse_errors.pos = pos
                parse_errors.toks = {[tok] = true}
            end
            return false
        end) * P(false)
    end

    -- Token
    local T = function(p)
        if type(p) == 'string' and #p ~= 0 then
            return (P(p) * V 'ws') + E(p)
        else
            return P(p) * V 'ws'
        end
    end
    local List = function(p)
        return ((P(p) * T ',') ^ 0) * ((P(p) * T ',' ^ -1) ^ -1)
    end

    grammar = P {
        'module',
        module = Ct(C(T '' * V 'stmt' * T ';')), --  * (-P(1) + E 'EOF')),

        stmt = V 'stmt_asgn',

        stmt_asgn = Ct(((((Cc(N.ASGN_LET) * T 'let') +
                           (Cc(N.ASGN_CONST) * T 'const')) * Ct(V 'ident')) +
                           (Cc(N.ASGN) * V 'expr_var')) * V 'stmt_asgn_impl'),

        stmt_asgn_impl = (V 'stmt_asgn_impl_v') + (V 'stmt_asgn_impl_f'),
        stmt_asgn_impl_v = T '=' * V 'expr',
        stmt_asgn_impl_f = (T '(' * Ct(List(Ct(V 'ident'))) * T ')' * T '=' *
            V 'expr') / function(args, body) return {N.FUNC, args, body} end,

        expr = V 'expr_table' + V 'expr_number' + V 'expr_var',
        expr_var = Ct(Cc(N.VAR) * V 'ident'),
        expr_number = (C((R '09' ^ 1 * P '.' ^ -1) +
                             (R '09' ^ 0 * '.' * R '09' ^ 1)) * T '') +
            E 'number',
        expr_table = Ct(T '{' * List(V 'table_key' * V 'stmt_asgn_impl') * T '}'),

        table_key = Ct(V 'ident') + (T '[' * V 'expr' * T ']'),

        ws = (S ' \t\n\r' + V 'line_comment') ^ 0,
        line_comment = P '//' * (1 - P '\n') ^ 0 * (P '\n') ^ -1,

        ident = (C(Cp() * ('_' + R('AZ', 'az')) *
                       ('_' + R('AZ', 'az', '09') ^ 0)) * Cp() * T '') +
            E 'identifier'
    }
end
]]

local grammar
do
    local re = require 're'
    local E = function(_, pos, tok)
        if parse_errors.pos == pos then
            parse_errors.toks[tok] = true
        elseif parse_errors.pos < pos then
            parse_errors.pos = pos
            parse_errors.toks = {[tok] = true}
        end
        return false
    end
    local line_comment = lpeg.P '//' * (1 - lpeg.P '\n') ^ 0 * (lpeg.P '\n') ^
                             -1
    local ws = (lpeg.S ' \t\n\r' + line_comment) ^ 0
    -- Token
    local T = function(p)
        return ws * (lpeg.P(p) + lpeg.Cmt(lpeg.Cc(p), E) * lpeg.P(false))
    end

    grammar = re.compile([[
    chunk <- (%nb '' -> 'chunk'
                  {| (stmt %SEMIC)* expr? |} %ws EOF
              %ne) -> node

    stmt <- stmt_asgn
          / stmt_return
          / stmt_break
          / stmt_for
          / expr

    stmt_asgn <- (%nb
                     (( %LET   -> 'let' 
                      / %CONST -> 'const') {| ident stmt_asgn_impl |}
                      / ''     -> 'assign' {| expr_var stmt_asgn_impl |})
                 %ne) -> node

    stmt_asgn_impl <- stmt_asgn_impl_v / stmt_asgn_impl_f
    stmt_asgn_impl_v <- %EQ expr
    stmt_asgn_impl_f <- (%nb '' -> 'func'
                            {| %LPAREN {| def_args_list |} %RPAREN %EQ expr |}
                        %ne) -> node

    stmt_return <- (%nb %RETURN -> 'return' expr? %ne) -> node
    stmt_break <- (%nb %BREAK -> 'break' %ne) -> node

    stmt_for <- (%nb %FOR -> 'for_range'
                    {| %LPAREN ident %IN at_range %RPAREN expr |} 
                %ne) -> node
              / (%nb %FOR -> 'for_in'
                    {| %LPAREN ident %IN expr %RPAREN expr |}
                %ne) -> node
    at_range <- {| %AT_RANGE %LPAREN expr %COMMA expr (%COMMA expr)? %RPAREN |}

    expr <- ({| %nb expr_prefix |} {| expr_suffix %ne |}* '' -> 'END')
            ~> expr_suffix_node
    expr_prefix <- expr_table
                / expr_number
                / expr_string
                / expr_block
                / expr_func
                / expr_var
    expr_suffix <- '' -> 'call' %LPAREN {| call_args_list |} %RPAREN

    expr_var    <- (%nb '' -> 'var'
                       {| (ident_impl %DOT)* ident_impl |}
                   %ne) -> node
    expr_number <- (%nb '' -> 'number' number_impl %ne) -> node
    expr_string <- (%nb '' -> 'string'
                       {"'" [^']* "'" / '' -> 'string' => E %F}
                   %ne) -> node
    expr_table  <- (%nb '' -> 'table'
                       %LBRACE {|
                       (({| table_key stmt_asgn_impl |} / expr) %COMMA)*
                       (({| table_key stmt_asgn_impl |} / expr))?
                       |} %RBRACE
                   %ne) -> node
    expr_block  <- (%nb '' -> 'block' %DOT_LBRACE func_body %RBRACE %ne) -> node
    expr_bloc_f <- (%nb '' -> 'block' %LBRACE     func_body %RBRACE %ne) -> node
    expr_func   <- (%nb '' -> 'func'
                       {| %DOT_LPAREN {| def_args_list |} %RPAREN expr_bloc_f |}
                   %ne) -> node

    ident <- (%nb '' -> 'ident' ident_impl %ne) -> node

    table_key <- ident / %LBRACK expr %RBRACK

    -- TODO: add type annotations to def_args_list
    def_args_list <- (ident %COMMA)* ident?
    call_args_list <- (expr %COMMA)* expr?

    func_body <- {| {| (stmt %SEMIC)* |} expr? |}

    -- TOKS --

    ident_impl  <- %ws ({[_%a] [_%w]*}
                        / '' -> 'identifier' => E %F)
    number_impl <- %ws ({[0-9]+ '.'? / [0-9]* '.' [0-9]+}
                        / '' -> 'number' => E %F)

    EOF <- (!. / '' -> 'EOF' => E %F )
]], {
        E = E,
        F = lpeg.P(false),
        ws = ws,
        line_comment = line_comment,
        node = function(f, n, c, b)
            if b == nil then
                b = c
                c = nil
            end
            return {n, f, b, c}
        end,
        expr_suffix_node = function(prefix, suffix)
            if suffix == 'END' then
                return prefix[2]
            else
                local f, e = prefix[1], prefix[2]
                local n, c, b = suffix[1], suffix[2], suffix[3]
                if n == 'call' then
                    return {f, {'call', f, b, {e, c}}}
                end
            end
        end,
        nb = ws * lpeg.Cp(),
        ne = lpeg.Cp(),
        SEMIC = T ';',
        COMMA = T ',',
        EQ = T '=',
        DOT = T '.',
        DOT_LBRACE = T '.{',
        DOT_LPAREN = T '.(',
        LBRACE = T '{',
        RBRACE = T '}',
        LBRACK = T '[',
        RBRACK = T ']',
        LPAREN = T '(',
        RPAREN = T ')',
        LET = T 'let',
        CONST = T 'const',
        RETURN = T 'return',
        BREAK = T 'break',
        FOR = T 'for',
        IN = T 'in',
        AT_RANGE = T '@range'
    })
end

local calcline = function(s, i)
    if i == 1 then return 1, 1 end
    local rest, line = s:sub(1, i):gsub('[^\n]*\n', '')
    local col = #rest
    return 1 + line, col ~= 0 and col or 1
end

local parse = function(src)
    parse_errors = {pos = -1, toks = {}}
    local r = grammar:match(src)
    if not r then
        local pos = parse_errors.pos
        local toks = {}
        for tok, _ in pairs(parse_errors.toks) do table.insert(toks, tok) end
        toks = table.concat(toks, ', ')

        local line, col = calcline(src, pos)

        local msg = "parser error: " .. line .. ":" .. col ..
                        ": expected one of "
        return r, msg .. toks
    end
    return r
end

M.compile = function(opts)
    local src = opts.input:read('a*')
    local ast, err = parse(src)

    if err then
        io.stderr:write(err)
        io.stderr:write('\n')
        return
    end
    pp(ast)

    local emit = require'src.emit'.emit
    local out = emit('-', {['-'] = ast})
    pp(out)

    -- local reader = require('src.reader').FileReader(opts.input)
    -- pp(reader)
    --
    -- local lex = require('src.lexer').lex
    --
    -- for token, span in lex(reader) do --
    --     pp(token)
    --     pp(span)
    -- end
    --
    -- pp(reader)

end

return M
