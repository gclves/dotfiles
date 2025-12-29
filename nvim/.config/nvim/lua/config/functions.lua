-- functions.lua - Custom functions

-- Run file function
_G.project_run_commands = {}

function _G.SetProjectRunCommand(filetype, command)
    local project_root = vim.fn.getcwd()

    if not _G.project_run_commands[project_root] then
        _G.project_run_commands[project_root] = {}
    end

    _G.project_run_commands[project_root][filetype] = command
end

function _G.RunFile()
    if not vim.fn.filereadable(vim.fn.expand('%')) then
        print("Error: File not saved")
        return
    end

    local default_commands = {
        python = 'python3 %',
        ruby = 'ruby %',
        go = 'go run %',
        javascript = 'node %',
        typescript = 'ts-node %',
        sh = 'sh %',
        c = 'gcc % -o %:r && ./%:r',
        rust = 'rustc % -o %:r && ./%:r'
    }

    local project_root = vim.fn.getcwd()
    local filetype = vim.bo.filetype
    local cmd = ''

    -- Check for project-specific override
    if _G.project_run_commands[project_root] and
        _G.project_run_commands[project_root][filetype] then
        cmd = _G.project_run_commands[project_root][filetype]
    else
        -- Fall back to default
        cmd = default_commands[filetype] or ''
    end

    if cmd == '' then
        print("No run configuration for filetype: " .. filetype)
        return
    end

    -- Save the file first
    vim.cmd('write')

    -- Store the current directory
    local cur_dir = vim.fn.getcwd()

    -- Change to file directory, run command, and change back
    vim.cmd('lcd %:p:h')
    vim.cmd('silent !clear')
    vim.cmd('!' .. cmd)
    vim.cmd('lcd ' .. cur_dir)
end

-- Whitespace cleanup function
function _G.CleanWhitespace()
    local save = vim.fn.winsaveview()
    vim.cmd([[keeppatterns %s/\s\+$//e]])
    vim.fn.winrestview(save)
end

-- LSP fallback functions
function _G.GoToDefinition()
    if #vim.lsp.get_clients() > 0 then
        vim.lsp.buf.definition()
    else
        vim.cmd('normal! gd')
    end
end

function _G.ShowDocs()
    if #vim.lsp.get_clients() > 0 then
        vim.lsp.buf.hover()
    else
        vim.cmd('normal! K')
    end
end

function _G.GoToReferences()
    if #vim.lsp.get_clients() > 0 then
        vim.lsp.buf.references()
    else
        vim.cmd('normal! gr')
    end
end

-- Create user commands
vim.api.nvim_create_user_command('SetProjectRunCommand',
    function(opts)
        local args = opts.fargs
        if #args >= 2 then
            SetProjectRunCommand(args[1], table.concat(args, ' ', 2))
        else
            print("Usage: SetProjectRunCommand filetype command")
        end
    end,
    { nargs = '+' }
)

