local M = {}

M.parse_args = function(spec_, args)
    local spec = {}
    for _, opt in ipairs(spec_) do
        opt.key = opt[1]

        spec['-' .. opt[1]] = opt
        if opt[2] then spec['--' .. opt[2]] = opt end
    end

    local opts = {}
    local only_posargs = false
    local arg_for_flag = nil
    for _, arg in ipairs(args) do
        if arg_for_flag then
            opts[spec[arg_for_flag].key] = arg
            arg_for_flag = nil

        elseif only_posargs then
            table.insert(opts, arg)

        elseif arg == '--' then
            only_posargs = true
        else
            local opt = spec[arg]
            if opt then
                if opt.arg then
                    arg_for_flag = arg
                else
                    opts[opt.key] = true
                end
            else
                table.insert(opts, arg)
            end
        end
    end

    if arg_for_flag then
        io.stderr:write('missing value for flag: '..arg_for_flag..'\n')
        os.exit(1)
    end

    return opts
end

return M
