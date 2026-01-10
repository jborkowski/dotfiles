return {
  "jborkowski/cobupilot-nvim",
  event = "InsertEnter",
  config = function()
    require("cobupilot").setup({
      suggestion = {
        enabled = true,
        auto_trigger = true,
        debounce_ms = 75,
      },
      panel = {
        auto_refresh = true,
        layout = {
          position = "bottom",
          ratio = 0.4,
        },
      },
    })
  end,
}
