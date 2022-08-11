local M = {}

local fmt = function(state, s)
    if state.fresh_line then s = string.rep(' ', state.depth * 2) .. s end
    state.fresh_line = string.sub(s, -1, -1) == '\n'

    io.stdout:write(s)
end

local Fmt = {
    indent = function(self) self.depth = self.depth + 1 end,
    dedent = function(self) self.depth = self.depth - 1 end,

    format_plain = function(self, val)
        if type(val) == 'function' then
            local id = self.fn_cache[val]
            if not id then
                id = self.fns
                self.fn_cache[val] = id
                self.fns = id + 1
            end
            self:fmt('<fn ' .. id .. '>')
        elseif type(val) == 'table' then
            local id = self.table_cache[val]
            if not id then
                id = self.tables
                self.table_cache[val] = id
                self.tables = id + 1
            end
            self:fmt('<table ' .. id .. '>')

        elseif val == nil then
            self:fmt('nil')
        else
            self:fmt(tostring(val))
        end
    end,
    format_key = function(self, key)
        if self.objs[key] ~= nil then
            self:fmt('[')
            self:format_plain(key)
            self:fmt(']')
            self.objs[key] = true
            return
        end

        if type(key) == 'string' then
            self:fmt(key)
        else
            self:fmt('[')
            self:format_plain(key)
            self:fmt(']')
        end
    end,
    format = function(self, val)
        if self.objs[val] ~= nil then
            self:format_plain(val)
            self.objs[val] = true
            return
        end

        if type(val) == 'string' then
            self:fmt("'" .. val .. "'")
        elseif type(val) == 'table' then
            self.objs[val] = false

            if self.max_depth and self.depth >= self.max_depth then
                self:fmt('{ <' .. #val .. ' items> }')
            else
                self:fmt('{\n')
                self:indent()

                for i = 1, #val do
                    self:format(val[i])
                    self:fmt(',\n')
                end

                for k, v in pairs(val) do
                    if not (type(k) == 'number' and k > 0 and k <= #val) then
                        self:format_key(k)
                        self:fmt(' = ')
                        self:format(v)
                        self:fmt(',\n')
                    end
                end

                self:dedent()
                self:fmt('}')
            end

            if self.objs[val] then
                self:fmt(' -- ')
                self:format_plain(val)
            end
            self.objs[val] = nil
        else
            self:format_plain(val)
        end
    end
}
Fmt.mt = {__index = Fmt}

M.pformat = function(val, opts)
    opts = opts or {}

    local state = {
        fmt = opts.fmt or fmt,
        depth = opts.depth or 0,
        max_depth = opts.max_depth,
        objs = {},

        fns = 0,
        fn_cache = {},
        tables = 0,
        table_cache = {}
    }
    setmetatable(state, Fmt.mt)

    state:format(val)
    state:fmt('\n')

    return val
end

return M
