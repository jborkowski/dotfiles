(local fennel (require :fennel))
(local pp fennel.view)

;; Test framework for home row mods
;; Simulates key events and verifies output

;; ============================================================
;; Mock Hammerspoon APIs
;; ============================================================

(var posted-events [])
(var next-source-id 1)  ;; 1 = real, 0 = synthetic

(fn mock-new-key-event [flags code-or-char is-down]
  (let [evt {:type (if is-down :keyDown :keyUp)
             :keyCode (if (= (type code-or-char) :number)
                          code-or-char
                          (. {:a 0 :s 1 :d 2 :f 3
                              :j 38 :k 40 :l 37
                              ";" 41
                              :u 32 :i 34 :e 14 :o 31
                              :c 8 :v 9 :b 11 :n 45 :m 46
                              :q 12 :w 13 :r 15 :t 17
                              :space 49}
                             code-or-char))
             :flags (if (= (type flags) :table) flags {})
             :sourceStateID next-source-id}]
    (tset evt :getKeyCode (fn [self] self.keyCode))
    (tset evt :getProperty (fn [self prop] self.sourceStateID))
    (tset evt :post (fn [self]
                      (table.insert posted-events self)))
    (tset evt :getCharacters (fn [self]
                                (each [k v (pairs {:a 0 :s 1 :d 2 :f 3
                                                   :j 38 :k 40 :l 37
                                                   ";" 41
                                                   :u 32 :i 34 :e 14 :o 31
                                                   :c 8 :v 9 :b 11 :n 45 :m 46
                                                   :q 12 :w 13 :r 15 :t 17
                                                   :space 49})]
                                  (when (= v self.keyCode) (lua "return k")))
                                "?"))
    evt))

;; Reverse keycode map
(local keycode-map
  {:a 0 :s 1 :d 2 :f 3
   :j 38 :k 40 :l 37
   ";" 41
   :u 32 :i 34 :e 14 :o 31
   :c 8 :v 9 :b 11 :n 45 :m 46
   :q 12 :w 13 :r 15 :t 17
   :space 49})

(local reverse-keycode-map
  (collect [k v (pairs keycode-map)]
    (values v k)))

;; Mock hs globals
(set _G.hs {:eventtap {:event {:types {:keyDown :keyDown :keyUp :keyUp}
                               :properties {:eventSourceStateID :eventSourceStateID}
                               :newKeyEvent mock-new-key-event}}
            :keycodes {:map reverse-keycode-map}
            :alert {:show (fn [msg] (print (.. "ALERT: " msg)))}})

;; ============================================================
;; Load the module under test
;; ============================================================

;; We need to capture the handlers rather than starting real eventtaps
(var down-handler nil)
(var up-handler nil)

;; Override hs.eventtap.new to capture handlers
(tset _G.hs.eventtap :new
      (fn [types handler]
        (let [tap {:started false}]
          ;; Figure out which handler this is
          (each [_ t (ipairs types)]
            (if (= t :keyDown) (set down-handler handler)
                (= t :keyUp) (set up-handler handler)))
          (tset tap :start (fn [self] (set self.started true) self))
          (tset tap :stop (fn [self] (set self.started false) self))
          tap)))

;; Now load the actual module
(local mod-map {:a :ctrl :s :alt :d :cmd :f :shift
                :j :rightshift :k :rightcmd :l :rightalt ";" :rightctrl})

(var state (collect [_k v (pairs mod-map) &into {:pending nil}]
             (values v false)))

(fn waiting? []
  (accumulate [ret state.pending _k v (pairs mod-map)]
    (or ret (. state v))))

(fn synthesize [evt]
  (local flags [])
  (when (or state.rightcmd state.cmd)
    (table.insert flags :cmd))
  (when (or state.rightalt state.alt)
    (table.insert flags :alt))
  (when (or state.rightctrl state.ctrl)
    (table.insert flags :ctrl))
  (when (or state.rightshift state.shift)
    (table.insert flags :shift))
  (let [code (evt:getKeyCode)
        down-evt (mock-new-key-event flags code true)
        up-evt (mock-new-key-event flags code false)]
    [down-evt up-evt]))

(fn synthesis? [evt]
  (let [id (evt:getProperty _G.hs.eventtap.event.properties.eventSourceStateID)]
    (not (= id 1))))

(fn promote-pending! []
  (when state.pending
    (let [pending-char (. _G.hs.keycodes.map state.pending)
          pending-mod (. mod-map pending-char)]
      (tset state pending-mod true)
      (set state.pending nil))))

(set down-handler
     (fn [evt]
       (if (synthesis? evt)
           false
           (let [code (evt:getKeyCode)
                 char (. _G.hs.keycodes.map code)
                 mod (. mod-map char)]
             (if mod
                 (do
                   (if (. state mod)
                       true
                       (= state.pending code)
                       (do
                         (tset state mod true)
                         (set state.pending nil)
                         true)
                       state.pending
                       (do
                         (promote-pending!)
                         (set state.pending code)
                         true)
                       (do
                         (set state.pending code)
                         true)))
                 (when (waiting?)
                   (promote-pending!)
                   true))))))

(set up-handler
     (fn [evt]
       (if (synthesis? evt)
           false
           (let [code (evt:getKeyCode)
                 char (. _G.hs.keycodes.map code)
                 mod (. mod-map char)]
             (if mod
                 (do
                   (tset state mod false)
                   (if (= state.pending code)
                       (do
                         (set state.pending nil)
                         (: (mock-new-key-event {} char true) :post)
                         (: (mock-new-key-event {} char false) :post)
                         true)
                       true))
                 (when (waiting?)
                   (promote-pending!)
                   (each [_ v (ipairs (synthesize evt))]
                     (v:post))
                   true))))))

;; ============================================================
;; Test helpers
;; ============================================================

(var test-count 0)
(var pass-count 0)
(var fail-count 0)

(fn reset-state! []
  (set posted-events [])
  (set next-source-id 1)
  (set state.pending nil)
  (each [_k v (pairs mod-map)]
    (tset state v false)))

(fn sim-key-down [char]
  "Simulate a real key down event"
  (set next-source-id 1)
  (let [code (. keycode-map char)
        evt (mock-new-key-event {} code true)]
    (set evt.sourceStateID 1)
    (down-handler evt)))

(fn sim-key-up [char]
  "Simulate a real key up event"
  (set next-source-id 1)
  (let [code (. keycode-map char)
        evt (mock-new-key-event {} code false)]
    (set evt.sourceStateID 1)
    (up-handler evt)))

(fn get-posted-chars []
  "Get the posted events as a readable sequence"
  (icollect [_ evt (ipairs posted-events)]
    (let [char (evt:getCharacters)
          flags (if (= (type evt.flags) :table) evt.flags [])
          flag-str (table.concat
                     (icollect [_ f (ipairs flags)]
                       f)
                     "+")]
      (.. (if (> (length flag-str) 0) (.. flag-str "+") "")
          char
          (if (= evt.type :keyDown) "v" "^")))))

(fn get-posted-summary []
  "Get simplified summary: just the characters that would be typed"
  (var result "")
  (each [_ evt (ipairs posted-events)]
    (when (= evt.type :keyDown)
      (let [char (evt:getCharacters)
            flags (if (= (type evt.flags) :table) evt.flags [])]
        (if (> (length flags) 0)
            (set result (.. result "[" (table.concat flags "+") "+" char "]"))
            (set result (.. result char))))))
  result)

(fn assert-eq [name expected actual]
  (set test-count (+ test-count 1))
  (if (= expected actual)
      (do
        (set pass-count (+ pass-count 1))
        (print (.. "  PASS: " name)))
      (do
        (set fail-count (+ fail-count 1))
        (print (.. "  FAIL: " name))
        (print (.. "    expected: " (pp expected)))
        (print (.. "    actual:   " (pp actual))))))

;; ============================================================
;; Tests
;; ============================================================

(print "\n========================================")
(print "Home Row Mods Test Suite")
(print "========================================\n")

;; --- Test 1: Tap a mod key quickly ---
(print "--- Tap tests (press and release quickly) ---")

(reset-state!)
(sim-key-down :a)
(sim-key-up :a)
(assert-eq "Tap 'a' produces 'a'" "a" (get-posted-summary))

(reset-state!)
(sim-key-down :s)
(sim-key-up :s)
(assert-eq "Tap 's' produces 's'" "s" (get-posted-summary))

(reset-state!)
(sim-key-down :d)
(sim-key-up :d)
(assert-eq "Tap 'd' produces 'd'" "d" (get-posted-summary))

(reset-state!)
(sim-key-down :f)
(sim-key-up :f)
(assert-eq "Tap 'f' produces 'f'" "f" (get-posted-summary))

(reset-state!)
(sim-key-down :j)
(sim-key-up :j)
(assert-eq "Tap 'j' produces 'j'" "j" (get-posted-summary))

(reset-state!)
(sim-key-down :k)
(sim-key-up :k)
(assert-eq "Tap 'k' produces 'k'" "k" (get-posted-summary))

(reset-state!)
(sim-key-down :l)
(sim-key-up :l)
(assert-eq "Tap 'l' produces 'l'" "l" (get-posted-summary))

(reset-state!)
(sim-key-down ";")
(sim-key-up ";")
(assert-eq "Tap ';' produces ';'" ";" (get-posted-summary))

;; --- Test 2: Hold mod + press another key ---
(print "\n--- Hold mod + key tests ---")

(reset-state!)
(sim-key-down :f)      ;; f pending
(sim-key-down :u)      ;; promote f to shift, consume u
(sim-key-up :u)        ;; synthesize u with shift
(sim-key-up :f)        ;; release shift
(assert-eq "Hold f + u produces Shift+u" "[shift+u]" (get-posted-summary))

(reset-state!)
(sim-key-down :a)      ;; a pending
(sim-key-down :u)      ;; promote a to ctrl, consume u
(sim-key-up :u)        ;; synthesize u with ctrl
(sim-key-up :a)        ;; release ctrl
(assert-eq "Hold a + u produces Ctrl+u" "[ctrl+u]" (get-posted-summary))

(reset-state!)
(sim-key-down :s)
(sim-key-down :u)
(sim-key-up :u)
(sim-key-up :s)
(assert-eq "Hold s + u produces Alt+u" "[alt+u]" (get-posted-summary))

(reset-state!)
(sim-key-down :d)
(sim-key-down :u)
(sim-key-up :u)
(sim-key-up :d)
(assert-eq "Hold d + u produces Cmd+u" "[cmd+u]" (get-posted-summary))

(reset-state!)
(sim-key-down :j)
(sim-key-down :u)
(sim-key-up :u)
(sim-key-up :j)
(assert-eq "Hold j + u produces Shift+u" "[shift+u]" (get-posted-summary))

(reset-state!)
(sim-key-down :k)
(sim-key-down :u)
(sim-key-up :u)
(sim-key-up :k)
(assert-eq "Hold k + u produces Cmd+u" "[cmd+u]" (get-posted-summary))

(reset-state!)
(sim-key-down :l)
(sim-key-down :u)
(sim-key-up :u)
(sim-key-up :l)
(assert-eq "Hold l + u produces Alt+u" "[alt+u]" (get-posted-summary))

(reset-state!)
(sim-key-down ";")
(sim-key-down :u)
(sim-key-up :u)
(sim-key-up ";")
(assert-eq "Hold ; + u produces Ctrl+u" "[ctrl+u]" (get-posted-summary))

;; --- Test 3: Non-mod key passthrough ---
(print "\n--- Non-mod key passthrough ---")

(reset-state!)
(sim-key-down :u)
(sim-key-up :u)
(assert-eq "Regular 'u' passes through (no posted events)" "" (get-posted-summary))

(reset-state!)
(sim-key-down :q)
(sim-key-up :q)
(assert-eq "Regular 'q' passes through" "" (get-posted-summary))

;; --- Test 4: Two mod keys ---
(print "\n--- Two mod key combos ---")

(reset-state!)
(sim-key-down :d)      ;; d pending
(sim-key-down :f)      ;; promote d to cmd, f pending
(sim-key-down :u)      ;; promote f to shift, consume u
(sim-key-up :u)        ;; synthesize u with cmd+shift
(sim-key-up :f)        ;; release shift
(sim-key-up :d)        ;; release cmd
(assert-eq "Hold d+f + u produces Cmd+Shift+u" "[cmd+shift+u]" (get-posted-summary))

(reset-state!)
(sim-key-down :a)      ;; a pending
(sim-key-down :s)      ;; promote a to ctrl, s pending
(sim-key-down :u)      ;; promote s to alt, consume u
(sim-key-up :u)        ;; synthesize u with ctrl+alt
(sim-key-up :s)        ;; release alt
(sim-key-up :a)        ;; release ctrl
(assert-eq "Hold a+s + u produces Ctrl+Alt+u" "[ctrl+alt+u]" (get-posted-summary))

;; --- Test 5: Mod key released before non-mod key ---
(print "\n--- Release order tests ---")

(reset-state!)
(sim-key-down :f)      ;; f pending
(sim-key-down :u)      ;; promote f to shift, consume u
(sim-key-up :f)        ;; release shift (before u release!)
(sim-key-up :u)        ;; u up - shift already released, but should still have been shift+u
(assert-eq "f down, u down, f up, u up - shift+u on u-up" "[shift+u]" (get-posted-summary))

;; --- Test 6: State is clean after operations ---
(print "\n--- State cleanup tests ---")

(reset-state!)
(sim-key-down :f)
(sim-key-up :f)
(assert-eq "After tap f, pending is nil" nil state.pending)
(assert-eq "After tap f, shift is false" false state.shift)

(reset-state!)
(sim-key-down :f)
(sim-key-down :u)
(sim-key-up :u)
(sim-key-up :f)
(assert-eq "After f+u combo, pending is nil" nil state.pending)
(assert-eq "After f+u combo, shift is false" false state.shift)

;; ============================================================
;; Summary
;; ============================================================

(print "\n========================================")
(print (.. "Results: " pass-count "/" test-count " passed, " fail-count " failed"))
(print "========================================\n")
