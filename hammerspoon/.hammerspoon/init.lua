-- Colemak-DHm + Home Row Mods + Space Nav for Hammerspoon
--
-- Replaces Kanata: full Colemak-DHm remapping (with angle mod),
-- GACS home row mods with per-key tap-hold, and space-hold nav layer.
--
-- Architecture: single eventtap, buffered event queue, pcall-protected
-- handler with watchdog timer for automatic recovery.
--
-- Synthetic events are tagged via kCGEventSourceUserData (property 42)
-- to guarantee they are never re-intercepted, regardless of the
-- eventSourceStateID behaviour of the current Hammerspoon build.

local eventtap = hs.eventtap
local event    = eventtap.event
local types    = event.types
local props    = event.properties
local log      = hs.logger.new("keys", "info")

-- ── Synthetic event marker ──────────────────────────────────────────
local SYN_PROP = 42           -- kCGEventSourceUserData
local SYN_VAL  = 0x484D4F44  -- "HMOD"

local function is_ours(evt)
    return evt:getProperty(SYN_PROP) == SYN_VAL
end

local function post(e)
    e:setProperty(SYN_PROP, SYN_VAL)
    e:post()
end

-- ── Keycode constants (macOS virtual keycodes) ──────────────────────
local KC = {
    A = 0,  S = 1,  D = 2,  F = 3,  H = 4,  G = 5,
    Z = 6,  X = 7,  C = 8,  V = 9,  B = 11,
    Q = 12, W = 13, E = 14, R = 15, Y = 16, T = 17,
    O = 31, U = 32, LBRACKET = 33, I = 34, P = 35,
    RETURN = 36, L = 37, J = 38, QUOTE = 39, K = 40,
    SEMICOLON = 41, BACKSLASH = 42, COMMA = 43,
    SLASH = 44, N = 45, M = 46, PERIOD = 47,
    TAB = 48, SPACE = 49, GRAVE = 50, DELETE = 51, ESCAPE = 53,
    LCMD = 55, LSHIFT = 56, CAPSLOCK = 57, LALT = 58, LCTRL = 59,
    RSHIFT = 60, RALT = 61, RCTRL = 62, RCMD = 54,
    LEFT = 123, RIGHT = 124, DOWN = 125, UP = 126,
}

-- ── QWERTY → Colemak-DHm remap ─────────────────────────────────────
local remap = {
    [KC.E] = KC.F,  [KC.R] = KC.P,  [KC.T] = KC.B,
    [KC.Y] = KC.J,  [KC.U] = KC.L,  [KC.I] = KC.U,
    [KC.O] = KC.Y,  [KC.P] = KC.SEMICOLON,
    [KC.H] = KC.K,
    [KC.LSHIFT] = KC.Z,
    [KC.Z] = KC.X,  [KC.X] = KC.C,  [KC.C] = KC.D,
    [KC.N] = KC.M,  [KC.M] = KC.H,
}

-- ── Home row mods ───────────────────────────────────────────────────
local home_mods = {
    [KC.A]         = { tap = KC.A, mod = "cmd",   timeout = 0.180 },
    [KC.S]         = { tap = KC.R, mod = "alt",   timeout = 0.160 },
    [KC.D]         = { tap = KC.S, mod = "ctrl",  timeout = 0.160 },
    [KC.F]         = { tap = KC.T, mod = "shift", timeout = 0.150 },
    [KC.J]         = { tap = KC.N, mod = "shift", timeout = 0.150 },
    [KC.K]         = { tap = KC.E, mod = "ctrl",  timeout = 0.160 },
    [KC.L]         = { tap = KC.I, mod = "alt",   timeout = 0.160 },
    [KC.SEMICOLON] = { tap = KC.O, mod = "cmd",   timeout = 0.180 },
}

-- ── Nav layer (space hold) ──────────────────────────────────────────
local nav_remap = {
    [KC.J]         = KC.LEFT,
    [KC.K]         = KC.DOWN,
    [KC.L]         = KC.UP,
    [KC.SEMICOLON] = KC.RIGHT,
}

local SPACE_TIMEOUT = 0.220

-- ── State ───────────────────────────────────────────────────────────
---@type table<integer, "waiting"|"held">
local mod_states = {}
---@type table<integer, hs.timer>
local mod_timers = {}
---@type "waiting"|"held"|nil
local space_state = nil
---@type hs.timer|nil
local space_timer = nil
local lshift_down = false

---@type BufferEntry[]
local buffer = {}
---@type table<integer, integer|boolean>
local consume_ups = {}

-- ── Reset ───────────────────────────────────────────────────────────
local function reset_state()
    for _, timer in pairs(mod_timers) do timer:stop() end
    mod_timers = {}
    mod_states = {}
    if space_timer then space_timer:stop(); space_timer = nil end
    space_state = nil
    lshift_down = false
    buffer = {}
    consume_ups = {}
end

-- ── Helpers ─────────────────────────────────────────────────────────
local function is_repeat(evt)
    return evt:getProperty(props.keyboardEventAutorepeat) ~= 0
end

local function any_waiting()
    if space_state == "waiting" then return true end
    for _, st in pairs(mod_states) do
        if st == "waiting" then return true end
    end
    return false
end

local function has_held_mods()
    for _, st in pairs(mod_states) do
        if st == "held" then return true end
    end
    return false
end

local function active_flags()
    local flags = {}
    local seen  = {}
    local phys = eventtap.checkKeyboardModifiers()
    for _, name in ipairs({"cmd", "alt", "ctrl", "shift", "fn"}) do
        if phys[name] then
            seen[name] = true
            flags[#flags + 1] = name
        end
    end
    for code, st in pairs(mod_states) do
        if st == "held" then
            local mod = home_mods[code] and home_mods[code].mod
            if mod and not seen[mod] then
                seen[mod] = true
                flags[#flags + 1] = mod
            end
        end
    end
    return flags
end

local function resolve_keycode(orig_code)
    if space_state == "held" and nav_remap[orig_code] then
        return nav_remap[orig_code]
    end
    if home_mods[orig_code] then
        return home_mods[orig_code].tap
    end
    return remap[orig_code] or orig_code
end

local function emit_key(keycode, flags)
    post(event.newKeyEvent(flags, keycode, true))
    post(event.newKeyEvent(flags, keycode, false))
end

local function flush_buffer()
    local flags = active_flags()
    for _, entry in ipairs(buffer) do
        local emit_code = resolve_keycode(entry.orig_code)
        emit_key(emit_code, flags)
        if not entry.up_received then
            consume_ups[entry.orig_code] = true
        end
    end
    buffer = {}
end

local function cancel_timer(keycode)
    local t = mod_timers[keycode]
    if t then t:stop(); mod_timers[keycode] = nil end
end

local function promote_to_held(keycode)
    if mod_states[keycode] == "waiting" then
        cancel_timer(keycode)
        mod_states[keycode] = "held"
        flush_buffer()
    end
end

local function promote_space()
    if space_state == "waiting" then
        if space_timer then space_timer:stop(); space_timer = nil end
        space_state = "held"
        flush_buffer()
    end
end

local function mark_buffer_up(code)
    for _, entry in ipairs(buffer) do
        if entry.orig_code == code and not entry.up_received then
            entry.up_received = true
            return true
        end
    end
    return false
end

-- ── Core handler (called inside pcall) ──────────────────────────────
local function handle(evt)
    if is_ours(evt) then return false end

    local etype = evt:getType()

    -- ── flagsChanged: angle-mod lshift, caps lock ───────────────
    if etype == types.flagsChanged then
        local code = evt:getKeyCode()

        if code == KC.LSHIFT then
            lshift_down = not lshift_down
            local target = remap[KC.LSHIFT]
            if lshift_down then
                if any_waiting() then
                    buffer[#buffer + 1] = { orig_code = KC.LSHIFT }
                    return true
                end
                post(event.newKeyEvent(active_flags(), target, true))
                consume_ups[KC.LSHIFT] = target
                return true
            else
                if consume_ups[KC.LSHIFT] then
                    local up_code = consume_ups[KC.LSHIFT]
                    if type(up_code) == "number" then
                        post(event.newKeyEvent({}, up_code, false))
                    end
                    consume_ups[KC.LSHIFT] = nil
                    return true
                end
                if mark_buffer_up(KC.LSHIFT) then return true end
                return true
            end
        end

        if code == KC.CAPSLOCK then
            local flags = evt:getFlags()
            if flags.capslock then
                emit_key(KC.ESCAPE, {})
            end
            return true
        end

        return false
    end

    -- ── keyDown / keyUp ─────────────────────────────────────────
    local code    = evt:getKeyCode()
    local is_down = (etype == types.keyDown)

    if is_down then consume_ups[code] = nil end

    -- Early consume_ups for keyUp
    if not is_down and consume_ups[code] then
        local action = consume_ups[code]
        consume_ups[code] = nil
        if type(action) == "number" then
            post(event.newKeyEvent({}, action, false))
        end
        return true
    end

    -- Consume keyUp for keys still in the buffer
    if not is_down and mark_buffer_up(code) then
        return true
    end

    -- ── SPACE ───────────────────────────────────────────────────
    if code == KC.SPACE then
        if is_down then
            if is_repeat(evt) then return true end
            if space_state then return true end
            space_state = "waiting"
            space_timer = hs.timer.doAfter(SPACE_TIMEOUT, function()
                local ok, err = pcall(promote_space)
                if not ok then
                    log.e("space timer: " .. tostring(err))
                    space_state = nil; space_timer = nil
                end
            end)
            return true
        else
            local st = space_state
            if space_timer then space_timer:stop(); space_timer = nil end
            space_state = nil
            if st == "waiting" then
                emit_key(KC.SPACE, active_flags())
                flush_buffer()
            elseif st == "held" then
                flush_buffer()
            end
            return true
        end
    end

    -- ── Buffer ALL keyDown while space is waiting ───────────────
    if space_state == "waiting" and is_down then
        buffer[#buffer + 1] = { orig_code = code }
        return true
    end

    -- ── Nav interception (space held + nav-mapped key) ──────────
    if space_state == "held" and nav_remap[code] then
        if is_down then
            post(event.newKeyEvent(active_flags(), nav_remap[code], true))
            consume_ups[code] = nav_remap[code]
            return true
        end
        return true
    end

    local mod_cfg = home_mods[code]

    -- ── MOD KEY DOWN ────────────────────────────────────────────
    if mod_cfg and is_down then
        if is_repeat(evt) then return true end
        if mod_states[code] then return true end
        -- If another mod is already held, this key is just a tap
        -- (e.g., hold J for shift, tap K → Shift+E, not ctrl)
        if has_held_mods() or space_state == "held" then
            local mapped = mod_cfg.tap
            if space_state == "held" and nav_remap[code] then
                mapped = nav_remap[code]
            end
            emit_key(mapped, active_flags())
            consume_ups[code] = true
            return true
        end
        mod_states[code] = "waiting"
        mod_timers[code] = hs.timer.doAfter(mod_cfg.timeout, function()
            local ok, err = pcall(promote_to_held, code)
            if not ok then
                log.e("mod timer: " .. tostring(err))
                mod_states[code] = nil; mod_timers[code] = nil
            end
        end)
        return true

    -- ── MOD KEY UP ──────────────────────────────────────────────
    elseif mod_cfg and not is_down then
        local st = mod_states[code]
        cancel_timer(code)
        mod_states[code] = nil
        if st == "waiting" then
            emit_key(mod_cfg.tap, active_flags())
            flush_buffer()
        elseif st == "held" then
            flush_buffer()
        end
        return true

    -- ── NON-MOD KEY DOWN ────────────────────────────────────────
    elseif not mod_cfg and is_down then
        local mapped = remap[code] or code

        if any_waiting() then
            buffer[#buffer + 1] = { orig_code = code }
            return true
        end

        if has_held_mods() then
            emit_key(mapped, active_flags())
            consume_ups[code] = true
            return true
        end

        if remap[code] then
            evt:setKeyCode(mapped)
        end
        return false

    -- ── NON-MOD KEY UP ──────────────────────────────────────────
    elseif not mod_cfg and not is_down then
        if remap[code] then
            evt:setKeyCode(remap[code])
        end
        return false
    end

    return false
end

-- ── Eventtap with pcall protection ──────────────────────────────────
local tap = eventtap.new(
    { types.keyDown, types.keyUp, types.flagsChanged },
    function(evt)
        local ok, result = pcall(handle, evt)
        if not ok then
            log.e("handler error: " .. tostring(result))
            return false
        end
        return result
    end
)

-- ── Watchdog: restart eventtap if it dies ────────────────────────────
local watchdog = hs.timer.new(3, function()
    if not tap:isEnabled() then
        log.w("eventtap died — resetting state and restarting")
        reset_state()
        tap:start()
        hs.alert.show("Eventtap restarted")
    end
end)

-- ── Auto-reload on config file change ───────────────────────────────
local watcher = hs.pathwatcher.new(hs.configdir, function(files)
    for _, f in ipairs(files) do
        if f:find("init%.lua$") then
            hs.reload()
            return
        end
    end
end)

-- ── Start ───────────────────────────────────────────────────────────
tap:start()
watchdog:start()
watcher:start()
hs.alert.show("Colemak-DHm loaded")
