local dark_theme = "colorscheme duskfox"
local light_theme = "colorscheme dawnfox"

vim.cmd(dark_theme)

local function set_keymap_n(key, cmd)
  vim.keymap.set("n", key, cmd, { silent = true })
end


-- Move it to user customizations 
set_keymap_n("|", "<cmd>lua toggle_theme()<CR>")

function toggle_theme()
  if(vim.g.current_colorscheme == "light") then
    vim.cmd(dark_theme)
    vim.g.current_colorscheme = "dark"
  else
    vim.cmd(light_theme)
    vim.g.current_colorscheme = "light"
  end
end

