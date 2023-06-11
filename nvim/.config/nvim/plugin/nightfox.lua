local dark_theme = "colorscheme duskfox"
local light_theme = "colorscheme dawnfox"

local function hour_based_theme() 
  nowTable = os.date('*t')
  nowHour = nowTable.hour

  -- Compare UTC time
  if (nowHour >= 19) then
    return dark_theme
  else
    return light_theme
  end 
end 

local current_theme = hour_based_theme()
vim.cmd(current_theme)


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

