(local fennel (require :fennel))

;; Queue-based home row mods
;;
;; Architecture:
;; - Single eventtap intercepts all keyDown/keyUp
;; - Mod keys enter "waiting" state with a per-key timer (180ms)
;; - Non-mod events are BUFFERED while any mod is in "waiting"
;; - Timer fires → waiting becomes "held" (modifier), buffer flushed WITH modifier
;; - Mod released while waiting → it was a TAP, emit character, flush buffer as-is
;; - Mod released while held → release modifier, flush remaining buffer
;;
;; This matches Kanata's tap-hold behavior: fast typing stays normal,
;; deliberate holds become modifiers.

;; Matching Kanata finger positions: GACS (Gui-Alt-Ctrl-Shift)
(local mod-map {:a :cmd :s :alt :d :ctrl :f :shift
                :j :rightshift :k :rightctrl :l :alt ";" :rightcmd})

(local HOLD-TIMEOUT 0.18) ;; seconds - matches Kanata tap-hold 180

;; Per-key state: nil (idle) | :waiting | :held
(var mod-states {})
;; Per-key timer
(var mod-timers {})
;; Buffered non-mod keydown events: [{:code N}]
(var buffer [])
;; Track keycodes whose real key-up we should consume
;; (because we already emitted synthetic down+up)
(var consume-ups {})

(fn synthesis? [evt]
  "Is this a synthetic event we generated?"
  (not (= 1 (evt:getProperty hs.eventtap.event.properties.eventSourceStateID))))

(fn any-mod-waiting? []
  "Is any mod key in the waiting state?"
  (accumulate [r false _k v (pairs mod-states)]
    (or r (= v :waiting))))

(fn any-mod-held? []
  "Is any mod key in the held state?"
  (accumulate [r false _k v (pairs mod-states)]
    (or r (= v :held))))

(fn get-held-flags []
  "Build HS modifier flags from all currently held mod keys"
  (local flags [])
  (local seen {})
  (each [code st (pairs mod-states)]
    (when (= st :held)
      (let [char (. hs.keycodes.map code)
            mod (. mod-map char)
            ;; Map to HS flag name (left/right both map to same flag)
            flag (case mod
                   :ctrl :ctrl  :rightctrl :ctrl
                   :alt :alt    :rightalt :alt
                   :cmd :cmd    :rightcmd :cmd
                   :shift :shift :rightshift :shift)]
        (when (and flag (not (. seen flag)))
          (tset seen flag true)
          (table.insert flags flag)))))
  flags)

(fn emit-key! [code flags-list]
  "Emit synthetic key down+up"
  (: (hs.eventtap.event.newKeyEvent flags-list code true) :post)
  (: (hs.eventtap.event.newKeyEvent flags-list code false) :post)
  ;; Mark this keycode so we consume the real key-up later
  (tset consume-ups code true))

(fn flush-buffer! []
  "Emit all buffered keydowns with current held modifier flags"
  (let [flags (get-held-flags)]
    (each [_ evt (ipairs buffer)]
      (emit-key! evt.code flags)))
  (set buffer []))

(fn cancel-timer! [keycode]
  (when (. mod-timers keycode)
    (: (. mod-timers keycode) :stop)
    (tset mod-timers keycode nil)))

(fn promote-to-held! [keycode]
  "Timer fired: waiting → held, flush buffer with modifier"
  (when (= (. mod-states keycode) :waiting)
    (cancel-timer! keycode)
    (tset mod-states keycode :held)
    (flush-buffer!)))

;; Single eventtap for all key events
(local tap
  (hs.eventtap.new
    [hs.eventtap.event.types.keyDown hs.eventtap.event.types.keyUp]
    (fn [evt]
      (when (synthesis? evt) (lua "return false"))

      (let [code (evt:getKeyCode)
            char (. hs.keycodes.map code)
            mod (. mod-map char)
            is-down (= (evt:getType) hs.eventtap.event.types.keyDown)]

        (if
          ;; ── MOD KEY DOWN ──
          (and mod is-down)
          (let [st (. mod-states code)]
            (if st
                ;; Already waiting or held (key repeat), ignore
                true
                ;; Fresh press: start waiting with timer
                (do
                  (tset mod-states code :waiting)
                  (tset mod-timers code
                        (hs.timer.doAfter HOLD-TIMEOUT
                          (fn [] (promote-to-held! code))))
                  true)))

          ;; ── MOD KEY UP ──
          (and mod (not is-down))
          (let [st (. mod-states code)]
            (cancel-timer! code)
            (tset mod-states code nil)
            (if
              ;; Was waiting → it's a TAP, emit character with any held mods
              ;; (handles f+j → J: f is held as shift, j tapped with shift)
              (= st :waiting)
              (do
                (emit-key! code (get-held-flags))
                (flush-buffer!)
                true)
              ;; Was held → modifier released, flush remaining buffer
              (= st :held)
              (do
                (flush-buffer!)
                true)
              ;; Unknown/nil
              true))

          ;; ── NON-MOD KEY DOWN ──
          (and (not mod) is-down)
          (if
            ;; Any mod waiting? Buffer this event (we don't know yet if it's tap or hold)
            (any-mod-waiting?)
            (do
              (table.insert buffer {:code code})
              true)
            ;; No waiting, but mods held? Emit immediately with flags
            (any-mod-held?)
            (do
              (emit-key! code (get-held-flags))
              true)
            ;; Nothing pending, pass through
            false)

          ;; ── NON-MOD KEY UP ──
          (and (not mod) (not is-down))
          (if
            ;; We emitted synthetic down+up for this key, consume real up
            (. consume-ups code)
            (do
              (tset consume-ups code nil)
              true)
            ;; Any mod active? Consume (shouldn't normally happen)
            (any-mod-waiting?)
            true
            ;; Pass through
            false)

          ;; ── FALLTHROUGH ──
          false)))))

(tap:start)
(hs.alert.show "Home row mods loaded")
