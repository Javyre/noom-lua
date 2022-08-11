local M = {}
local pp = require('src.pretty').pformat

local tappend = function(t, es) for i = 1, #es do table.insert(t, es[i]) end end

local unnode = function(node_ty, node)
    if node[1] ~= node_ty then
        error('Wrong node_ty. Expected ' .. node_ty .. ' got ' ..
                  tostring(node[1]) .. '.', 2)
    end
    return {node[2], node[3]}, node[4]
end

local sym_id = 0
local gensym = function(tag)
    sym_id = sym_id + 1
    return {'ident', 0, 0, '__nm_' .. (tag or '') .. '_' .. sym_id}
end

--
-- LUIFY
--
local luify_stmt, luify_expr

local luify_number = function(node) return node, {} end

local luify_var = function(node) return node, {} end

local luify_call = function(node)
    local args = {}
    local stmts = {}

    local f, a = node[4][1], node[4][2]

    local f_val, f_stmts = luify_expr(f)
    for i = 1, #f_stmts do table.insert(stmts, f_stmts[i]) end

    for i = 1, #a do
        local a_val, a_stmts = luify_expr(a[i])
        for j = 1, #a_stmts do table.insert(stmts, a_stmts[j]) end
        table.insert(args, a_val)
    end

    local call = {'call', node[2], node[3], {f_val, args}}

    return call, stmts
end

local luify_block = function(node, target)
    local ret = {}
    local stmts, val = node[4][1], node[4][2]

    local body = {}
    for i = 1, #stmts do
        local stmt = luify_stmt(stmts[i])
        for j = 1, #stmt do table.insert(body, stmt[j]) end
    end

    if #body == 0 then
        if val then
            return luify_expr(val, target)
        else
            return {'nil', node[2], node[3]}, ret
        end
    end

    if val then
        local is_new_target
        if target == nil then
            target = gensym('BR')
            is_new_target = true
        end

        local l_val, l_val_stmts
        if target == false then
            -- i.e. purposely ignore value.
            l_val, l_val_stmts = {'nil', node[2], node[3]}, luify_stmt(val)
        else
            l_val, l_val_stmts = luify_expr(val, target)
        end

        for i = 1, #l_val_stmts do table.insert(body, l_val_stmts[i]) end

        table.insert(ret, {'block', node[2], node[3], body})

        if l_val[1] == 'nil' then
            return l_val, ret
        else
            if is_new_target then
                table.insert(ret, 1, {'local', val[2], val[3], {target, nil}})
            end

            if l_val ~= target then
                table.insert(body, {'assign', node[2], node[3], {target, l_val}})
                return target, ret
            else
                return target, ret
            end
        end
    else
        table.insert(ret, {'block', node[2], node[3], body})

        return {'nil', node[2], node[3]}, ret
    end
end

local luify_func = function(node, target)
    local body = node[4][2]
    if body[1] == 'block' then
        local body_stmts = {}
        local block_stmts = body[4][1]
        for i = 1, #block_stmts do
            local stmt = luify_stmt(block_stmts[i])
            for j = 1, #stmt do table.insert(body_stmts, stmt[j]) end
        end
        if body[4][2] then
            local l_val, l_val_stmts = luify_expr(body[4][2])
            for i = 1, #l_val_stmts do
                table.insert(body_stmts, l_val_stmts[i])
            end
            table.insert(body_stmts, l_val)
        end

        return {'func', node[2], node[3], {node[4][1], body_stmts}}, {}
    else

        local body_val, body_stmts = luify_expr(node[4][2])
        table.insert(body_stmts, body_val)

        return {'func', node[2], node[3], {node[4][1], body_stmts}}, {}
    end
end

local luify_table = function(node, target)
    local stmts = {}
    local entries = {}

    for i, entry in ipairs(node[4]) do
        if #entry == 2 then
            local key

            if entry[1][1] == 'ident' then
                key = entry[1]
            else
                local k_val, k_stmts = luify_expr(entry[1])
                for i = 1, #k_stmts do
                    table.insert(stmts, k_stmts[i])
                end
                key = k_val
            end

            local v_val, v_stmts = luify_expr(entry[2])
            for i = 1, #v_stmts do table.insert(stmts, v_stmts[i]) end

            table.insert(entries, {key, v_val})
        else
            local e_val, e_stmts = luify_expr(entry)
            for i = 1, #e_stmts do table.insert(stmts, e_stmts[i]) end

            table.insert(entries, {e_val})
        end
    end

    return {'table', node[2], node[3], entries}, stmts
end

local luify_expr_T = {
    number = luify_number,
    var = luify_var,
    call = luify_call,
    block = luify_block,
    func = luify_func,
    table = luify_table
}
-- NOTE: The target should be a gensym()'ed symbol or guaranteed not to be
--       shadowed.
luify_expr =
    function(node, target) return luify_expr_T[node[1]](node, target) end

local luify_let = function(node)
    local name, val = node[4][1], node[4][2]

    local val_expr, stmts = luify_expr(val)

    if #stmts > 0 then
        table.insert(stmts, 1, {'local', node[2], node[3], {name, nil}})
        if val_expr[1] ~= 'nil' then
            table.insert(stmts, {'assign', node[2], node[3], {name, val_expr}})
        end
    else
        stmts = {{'local', node[2], node[3], {name, val_expr}}}
    end

    return stmts
end
local luify_const = luify_let
local luify_assign = function(node)
    local name, val = node[4][1], node[4][2]

    local val_expr, stmts = luify_expr(val)
    table.insert(stmts, {'assign', node[2], node[3], {name, val_expr}})

    return stmts
end

local luify_return = function(node)
    if node[4] then
        local l_val, stmts = luify_expr(node[4])
        table.insert(stmts, {'return', node[2], node[3], l_val})
        return stmts
    else
        return {node}
    end
end
local luify_break = function(node) return {node} end

local luify_for_range = function(node)
    local stmts = {}
    local params = {}

    -- @range() params
    for i = 1, #node[4][2] do
        local l_param, l_param_stmts = luify_expr(node[4][2][i])
        for i = 1, #l_param_stmts do
            table.insert(stmts, l_param_stmts[i])
        end
        table.insert(params, l_param)
    end

    local body = node[4][3]
    local l_body_stmts = luify_stmt(body)

    if #l_body_stmts == 1 and l_body_stmts[1][1] == 'block' then
        l_body_stmts = l_body_stmts[1][4]
    end

    table.insert(stmts, {
        'for_range', node[2], node[3], {node[4][1], params, l_body_stmts}
    })

    return stmts
end
local luify_for_in = function(node)
    local stmts = {}

    local l_gen, l_gen_stmts = luify_expr(node[4][2])
    for i = 1, #l_gen_stmts do table.insert(stmts, l_gen_stmts[i]) end

    local body = node[4][3]
    local l_body_stmts = luify_stmt(body)

    if #l_body_stmts == 1 and l_body_stmts[1][1] == 'block' then
        l_body_stmts = l_body_stmts[1][4]
    end

    table.insert(stmts, {
        'for_in', node[2], node[3], {node[4][1], l_gen, l_body_stmts}
    })

    return stmts
end

-- we can omit luifying the entire thing
local is_discardable = {
    number = true,
    string = true,
    func = true,
    ident = true,
    var = true
}
local luify_expr_stmt = function(node)
    if is_discardable[node[1]] then
        return {}
    else
        -- TODO: enforce target == false implies val == nil
        local val, stmts = luify_expr(node, false)
        if val[1] ~= 'nil' then table.insert(stmts, val) end
        return stmts
    end
end

local luify_stmt_T = {
    let = luify_let,
    const = luify_const,
    assign = luify_assign,
    ['return'] = luify_return,
    ['break'] = luify_break,
    for_range = luify_for_range,
    for_in = luify_for_in
    -- call = luify_call_stmt,
    -- block = luify_block_stmt
}
luify_stmt = function(stmt)
    local stmt_handler = luify_stmt_T[stmt[1]]
    if stmt_handler then
        return stmt_handler(stmt)
    else
        -- then the stmt is an expr.
        return luify_expr_stmt(stmt)
        -- return luify_stmt({
        --     'assign', stmt[2], stmt[3],
        --     {{'ident', stmt[2], stmt[3], '__nm_void_'}, stmt}
        -- })
    end
end

local luify_chunk = function(chunk)
    local span, stmts = unnode('chunk', chunk)
    local luish_stmts = {}

    for _, stmt in ipairs(stmts) do
        -- pp(stmt, {max_depth = 3})

        tappend(luish_stmts, luify_stmt(stmt))
    end

    return {'chunk', chunk[2], chunk[3], luish_stmts}
end

--
-- EMIT
--
local is_callable = {call = true, var = true, ident = true}

local Ctx = {}

function Ctx.emit_number(self, node) self:e{tostring(node[4])} end
function Ctx.emit_var(self, node) self:e{table.concat(node[4], '.')} end
function Ctx.emit_ident(self, node) self:e{node[4]} end
function Ctx.emit_nil(self, node) self:e{'nil'} end
function Ctx.emit_call(self, node)
    local func = node[4][1]
    if is_callable[func[1]] then
        self:emit_expr(func)
    else
        self:e{'('}
        self:emit_expr(func)
        self:e{')'}
    end
    self:e{'('}
    local args = node[4][2]
    if #args > 0 then
        for i = 1, #args - 1 do
            self:emit_expr(args[i])
            self:e{', '}
        end
        self:emit_expr(args[#args])
    end
    self:e{')'}
end
function Ctx.emit_func(self, node)
    self:e{'function('}
    local args = node[4][1]
    if #args > 0 then
        for i = 1, #args - 1 do
            self:emit_ident(args[i])
            self:e{', '}
        end
        self:emit_ident(args[#args])
    end
    self:e{')\n'}
    self:indent()
    local body = node[4][2]
    for i = 1, #body - 1 do self:emit_stmt(body[i]) end
    self:e{'return '}
    self:emit_expr(body[#body])
    self:dedent()
    self:e{'\n'}
    self:e{'end'}
end

function Ctx.emit_table(self, node)
    self:e{'{\n'}
    self:indent()
    for i, entry in ipairs(node[4]) do
        if #entry == 1 then
            self:emit_expr(entry[1])
            self:e{',\n'}
        else
            if entry[1][1] == 'var' or entry[1][1] == 'ident' then
                pp(entry)
                self:emit_ident(entry[1])
                self:e{' = '}
            else
                self:e{'['}
                pp(entry[1])
                self:emit_expr(entry[1])
                self:e{'] = '}
            end
            self:emit_expr(entry[2])
            self:e{',\n'}
        end
    end
    self:dedent()
    self:e{'}'}
end

local emit_expr_T = {
    number = Ctx.emit_number,
    var = Ctx.emit_var,
    ident = Ctx.emit_ident,
    ['nil'] = Ctx.emit_nil,
    call = Ctx.emit_call,
    func = Ctx.emit_func,
    table = Ctx.emit_table
}
function Ctx.emit_expr(self, node) return emit_expr_T[node[1]](self, node) end

function Ctx.emit_local(self, node)
    local name, expr = node[4][1], node[4][2]
    if expr then
        self:e{'local ', name[4], ' = '}
        self:emit_expr(expr)
        self:e{'\n'}
    else
        self:e{'local ', name[4], '\n'}
    end
end
function Ctx.emit_assign(self, node)
    local target, expr = node[4][1], node[4][2]
    self:emit_expr(target)
    self:e{' = '}
    self:emit_expr(expr)
    self:e{'\n'}
end
function Ctx.emit_block(self, node)
    self:e{'do\n'}
    self:indent()
    local stmts = node[4]
    for _, stmt in ipairs(stmts) do self:emit_stmt(stmt) end
    self:dedent()
    self:e{'end\n'}
end
function Ctx.emit_call_stmt(self, node)
    self:emit_call(node)
    self:e{'\n'}
end
function Ctx.emit_nil_stmt(self, node) return end

function Ctx.emit_return(self, node)
    if node[4] then
        self:e{'return '}
        self:emit_expr(node[4])
        self:e{'\n'}
    else
        self:e{'return\n'}
    end
end

function Ctx.emit_break(self, node) self:e{'break\n'} end

function Ctx.emit_for_range(self, node)
    self:e{'for '}
    self:emit_ident(node[4][1])
    self:e{' = '}

    do
        local range = node[4][2]
        self:emit_expr(range[1])
        self:e{', '}
        self:emit_expr(range[2])
        if #range == 3 then
            self:e{', '}
            self:emit_expr(range[3])
        end
    end

    self:e{' do\n'}
    self:indent()
    local stmts = node[4][3]
    for _, stmt in ipairs(stmts) do self:emit_stmt(stmt) end
    self:dedent()
    self:e{'end\n'}
end
function Ctx.emit_for_in(self, node)
    self:e{'for '}
    self:emit_ident(node[4][1])
    self:e{' in '}
    self:emit_expr(node[4][2])
    self:e{' do\n'}
    self:indent()
    local stmts = node[4][3]
    for _, stmt in ipairs(stmts) do self:emit_stmt(stmt) end
    self:dedent()
    self:e{'end\n'}
end

local emit_stmt_T = {
    ['local'] = Ctx.emit_local,
    assign = Ctx.emit_assign,
    block = Ctx.emit_block,
    call = Ctx.emit_call_stmt,
    ['nil'] = Ctx.emit_nil_stmt,
    ['return'] = Ctx.emit_return,
    ['break'] = Ctx.emit_break,
    for_range = Ctx.emit_for_range,
    for_in = Ctx.emit_for_in
}
function Ctx.emit_stmt(self, node)
    local stmt_handler = emit_stmt_T[node[1]]
    if stmt_handler then
        return stmt_handler(self, node)
    else
        return self:emit_assign({
            'assign', node[2], node[3],
            {{'ident', node[2], node[3], '__nm_void_'}, node}
        })
    end
end

function Ctx.emit_chunk(self, node)
    local _, stmts = unnode('chunk', node)
    for _, stmt in ipairs(stmts) do self:emit_stmt(stmt) end
end

function Ctx.indent(self) self.indent_depth = self.indent_depth + 1 end
function Ctx.dedent(self) self.indent_depth = self.indent_depth - 1 end
function Ctx.e(self, strings)
    if self.fresh_line then
        table.insert(strings, 1, string.rep(' ', self.indent_depth * 2))
    end
    self.fresh_line = string.sub(strings[#strings], -1, -1) == '\n'

    for i = 1, #strings do
        local s = strings[i]
        assert(type(s) == 'string', 'emit only strings')
        table.insert(self.output, s)
    end
end

M.emit = function(entry_name, chunks)
    local luish_chunks = {}
    for mname, m in pairs(chunks) do luish_chunks[mname] = luify_chunk(m) end

    local ctx = {
        chunks = luish_chunks,
        output = {},
        indent_depth = 0,
        fresh_line = true
    }
    setmetatable(ctx, {__index = Ctx})

    ctx.emit_chunk(ctx, luish_chunks[entry_name])
    return table.concat(ctx.output)
end

return M
