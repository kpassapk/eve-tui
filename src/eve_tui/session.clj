(ns eve-tui.session
  "Multi-session TUI framework: persistent state + F-key session control.

   F1: take control  F2: force-take (steal)  F3: release

   Usage:
     (session/run
       {:db-path  \"/tmp/myapp/\"
        :init     (fn [] {:data @my-atom})
        :update   my-update-fn
        :view     my-view-fn})

   The app's update-fn receives two extra message types:
     {:type :session/tick}    — every tick, sync your atoms here
     {:type :session/stolen}  — your lock was stolen, reset to read-only mode

   The framework handles ctrl+c quit (releases lock). To add more quit keys:
     :quit? (fn [state msg] (msg/key-match? msg \"q\"))

   Exported helpers for app use:
     (holding? state)        — true if this process holds the lock
     (lock-bar state)        — renders the lock status line
     (printable-key? msg)    — true for typeable single chars"

  (:require [charm.program :as program]
            [charm.message :as msg]
            [charm.style.core :as style]
            [eve.atom :as ea])
  (:import [java.nio.channels FileChannel]
           [java.nio.file Paths StandardOpenOption OpenOption]
           java.lang.ProcessHandle))

;; ---------------------------------------------------------------------------
;; Session context — one per process, set up by run
;; ---------------------------------------------------------------------------

(def ^:private !ctx (atom nil))
(defn- ctx [] @!ctx)

;; ---------------------------------------------------------------------------
;; Process identity
;; ---------------------------------------------------------------------------

(def ^:private my-pid
  (delay (str (.pid (ProcessHandle/current)))))

;; ---------------------------------------------------------------------------
;; OS file lock
;; bb's sci blocks FileLockImpl.release(), so we release by closing the channel.
;; ---------------------------------------------------------------------------

(defn- open-lock-channel [lock-file]
  (FileChannel/open
   (Paths/get lock-file (into-array String []))
   (into-array OpenOption [StandardOpenOption/CREATE
                           StandardOpenOption/WRITE])))

(defn- try-acquire! []
  (let [{:keys [lock-ch]} (ctx)]
    (try (when (.tryLock @lock-ch) :locked)
         (catch Exception _ nil))))

(defn- release-os-lock! []
  (let [{:keys [lock-ch lock-file]} (ctx)]
    (try (.close @lock-ch) (catch Exception _))
    (reset! lock-ch (open-lock-channel lock-file))))

(defn- take-control! []
  (let [{:keys [lock-info-atom]} (ctx)]
    (when-let [fl (try-acquire!)]
      (reset! lock-info-atom {:owner @my-pid :taken-at (System/currentTimeMillis)})
      fl)))

(defn- release-control! []
  (let [{:keys [lock-info-atom]} (ctx)]
    (release-os-lock!)
    (reset! lock-info-atom nil)))

;; ---------------------------------------------------------------------------
;; Tick command
;; ---------------------------------------------------------------------------

(defn- tick-cmd []
  (let [{:keys [tick-ms]} (ctx)]
    (program/cmd (fn []
                   (Thread/sleep tick-ms)
                   {:type :tick}))))

;; ---------------------------------------------------------------------------
;; Public helpers
;; ---------------------------------------------------------------------------

(defn holding?
  "True if this process currently holds the edit lock."
  [state]
  (some? (:lock-ref state)))

(defn printable-key?
  "True for a single printable character key with no ctrl/alt modifier."
  [msg]
  (and (msg/key-press? msg)
       (string? (:key msg))
       (= 1 (count (:key msg)))
       (not (:ctrl msg))
       (not (:alt msg))))

(def ^:private locked-s   (delay (style/style :fg style/green :bold true)))
(def ^:private unlocked-s (delay (style/style :fg 240)))
(def ^:private stolen-s   (delay (style/style :fg style/red :bold true)))

(defn lock-bar
  "Renders a one-line lock status string for inclusion in app view."
  [state]
  (let [{:keys [lock-ref lock-info]} state
        other-owner (:owner lock-info)]
    (cond
      lock-ref
      (style/render @locked-s (str " [YOU HAVE CONTROL - pid " @my-pid "]"))

      (and other-owner (not= other-owner @my-pid))
      (style/render @stolen-s (str " [READ-ONLY - pid " other-owner " is editing  F2: force-take]"))

      :else
      (style/render @unlocked-s " [UNLOCKED  F1: take control]"))))

;; ---------------------------------------------------------------------------
;; Session message handling
;; ---------------------------------------------------------------------------

(defn- handle-tick [state app-update]
  (let [{:keys [lock-info-atom]} (ctx)
        remote-lock (deref lock-info-atom)
        stolen?     (and (holding? state)
                         (not= (:owner remote-lock) @my-pid))
        wants?      (and (:wants-control state) (not (holding? state)) (not stolen?))
        acquired    (when wants? (take-control!))]
    (when stolen? (release-os-lock!))
    ;; Build updated session fields
    (let [post-session (cond-> state
                         (not= remote-lock (:lock-info state)) (assoc :lock-info remote-lock)
                         stolen?  (assoc :lock-ref nil)
                         stolen?  (assoc :wants-control false)
                         acquired (assoc :lock-ref acquired)
                         acquired (assoc :lock-info {:owner @my-pid
                                                     :taken-at (System/currentTimeMillis)})
                         acquired (assoc :wants-control false))
          ;; Let app sync its atoms and reset mode on steal
          [app-state app-cmd] (app-update post-session
                                          {:type (if stolen? :session/stolen :session/tick)})]
      [(merge post-session app-state) (program/batch app-cmd (tick-cmd))])))

(defn- wrap-update [app-update quit?]
  (fn [state msg]
    (cond
      ;; ctrl+c — always quit, release lock
      (msg/key-match? msg "ctrl+c")
      (do (when (holding? state) (release-control!))
          [state program/quit-cmd])

      ;; app-defined quit keys
      (quit? state msg)
      (do (when (holding? state) (release-control!))
          [state program/quit-cmd])

      ;; tick — session sync + notify app
      (= (:type msg) :tick)
      (handle-tick state app-update)

      ;; F1 — take control
      (and (msg/key-match? msg "f1") (not (holding? state)))
      (if-let [fl (take-control!)]
        [(-> state (assoc :lock-ref fl)
             (assoc :lock-info @(:lock-info-atom (ctx))))
         nil]
        [state nil])

      ;; F2 — force-take: clear lock-info to trigger holder's stolen? check,
      ;;      then retry tryLock on each tick via :wants-control
      (and (msg/key-match? msg "f2") (not (holding? state)))
      (do (reset! (:lock-info-atom (ctx)) nil)
          [(assoc state :wants-control true) nil])

      ;; F3 — release control
      (and (msg/key-match? msg "f3") (holding? state))
      (do (release-control!)
          [(-> state (assoc :lock-ref nil) (assoc :lock-info nil)) nil])

      ;; everything else → app
      :else
      (app-update state msg))))

;; ---------------------------------------------------------------------------
;; Session init state fields
;; ---------------------------------------------------------------------------

(defn- session-state []
  (let [{:keys [lock-info-atom]} (ctx)]
    {:lock-ref      nil
     :lock-info     @lock-info-atom
     :wants-control false}))

;; ---------------------------------------------------------------------------
;; Public entry point
;; ---------------------------------------------------------------------------

(defn run
  "Run a session-aware charm.clj TUI app.

   Required keys:
     :db-path  — path to eve db directory (created if absent)
     :init     — (fn [] app-state) or (fn [] [app-state cmd])
     :update   — (fn [state msg] [new-state cmd])
     :view     — (fn [state] string)

   Optional keys:
     :quit?      — (fn [state msg] bool), extra quit condition beyond ctrl+c
     :tick-ms    — tick interval in ms (default 300)
     :alt-screen — bool (default true)"
  [{:keys [db-path init update view quit? tick-ms alt-screen]
    :or   {quit?      (fn [_ _] false)
           tick-ms    300
           alt-screen true}}]
  (.mkdirs (java.io.File. db-path))
  (let [lock-file (str db-path ".editlock")
        lock-info-atom (ea/persistent-atom
                        {:id :session/lock-info :persistent db-path} nil)]
    (reset! !ctx {:db-path       db-path
                  :lock-file     lock-file
                  :lock-ch       (atom (open-lock-channel lock-file))
                  :lock-info-atom lock-info-atom
                  :tick-ms       tick-ms})
    (program/run
     {:init       (fn []
                    (let [result    (init)
                          [app-state init-cmd] (if (vector? result) result [result nil])]
                      [(merge app-state (session-state))
                       (program/batch init-cmd (tick-cmd))]))
      :update     (wrap-update update quit?)
      :view       view
      :alt-screen alt-screen})))
