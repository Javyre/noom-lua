local M = {}

local bytes_iterator = function(self)
    if self.lines == nil then self.lines = {self.file:read('*l')} end

    if self.lines[self.linum] and self.colnum == #self.lines[self.linum] + 1 then
        table.insert(self.lines, self.file:read('*l'))
        self.linum = self.linum + 1
        self.colnum = 0
    end

    if self.lines[self.linum] and self.colnum == #self.lines[self.linum] then
        self.colnum = self.colnum + 1
        self.bytenum = self.bytenum + 1 -- newline byte
        return '\n', self.linum, self.colnum, self.bytenum
    end

    if self.lines[self.linum] then
        self.colnum = self.colnum + 1
        self.bytenum = self.bytenum + 1
        return self.lines[self.linum]:sub(self.colnum, self.colnum), self.linum,
               self.colnum, self.bytenum
    else
        return nil
    end
end
local bytes = function(self, reset)
    if reset then
        self.bytenum = 0
        self.colnum = 0
        self.linum = 1
    end
    return bytes_iterator, self, nil
end

local get_pos = function(self)
    return {linum = self.linum, colnum = self.colnum, bytenum = self.bytenum}
end

local get_span = function(self, start, stop)
    local str
    if start.linum == stop.linum then
        str = self.lines[start.linum]:sub(start.colnum, stop.colnum)
    else
        local lines = {}
        table.insert(lines, self.lines[start.linum]:sub(start.colnum, -1))
        for i in start.colnum, 1, stop.colnum do
            table.insert(lines, self.lines[i])
        end
        table.insert(lines, self.lines[stop.linum]:sub(1, stop.colnum))
        str = table.concat(lines)
    end

    return {str = str, start = start, stop = stop}
end

M.FileReader = function(file)
    return {
        file = file,
        lines = nil,
        bytenum = 0,
        linum = 1,
        colnum = 0,
        bytes = bytes,
        get_pos = get_pos,
        get_span = get_span
    }
end

return M
