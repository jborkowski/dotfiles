-- --- App Switcher ---
-- Press Cmd+<number> to switch to an app.
-- If the app is already active, pressing the shortcut again will cycle through its windows.
--
local modifiers = { "cmd" }

local app_map = {
  ["1"] = "Safari",
  ["2"] = "Ghostty",
  ["3"] = "Zed",
  ["4"] = "Tidal",
  ["9"] = "Finder",
}

for key, app_name in pairs(app_map) do
  hs.hotkey.bind(modifiers, key, function()
    local front_app = hs.application.frontmostApplication()
    local target_app = hs.application.find(app_name)

    -- If the app isn't running, launch it.
    if not target_app then
      hs.application.launchOrFocus(app_name)
      return
    end

    -- If the app is already in front, cycle its windows.
    if front_app and front_app:name() == target_app:name() then
        local all_windows = target_app:allWindows()
        if #all_windows > 1 then
            local current_win = hs.window.focusedWindow()
            local current_win_index = -1
            for i, win in ipairs(all_windows) do
                if win:id() == current_win:id() then
                    current_win_index = i
                    break
                end
            end

            if current_win_index ~= -1 then
                local next_win_index = (current_win_index % #all_windows) + 1
                all_windows[next_win_index]:focus()
            end
        end
    else
      -- Otherwise, just bring the app to the front.
      target_app:activate()
    end
  end)
end

-- Add a notification to confirm when your config is loaded/reloaded
hs.notify.new({ title = "Hammerspoon", informativeText = "Configuration reloaded" }):send()
