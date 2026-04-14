#!/usr/bin/env bb
;; sheet.bb — Google-Sheets-like TUI backed by eve mmap atom
;; Session control: F1 take  F2 force-take  F3 release  q quit

(require '[babashka.deps :as deps])

(deps/add-deps
 '{:deps {io.github.timokramer/charm.clj {:git/tag "v0.2.69"
                                          :git/sha "119c5efe2eb488bd75c6fe7511971843d6f8de71"}
          eve/eve                         {:git/url "https://github.com/SeniorCareMarket/eve"
                                           :git/sha "fdf021daa0e3463743308d811b7712096a7c6381"}}})

;; Load framework from local path (replace with git dep once published)
(require '[babashka.classpath :as cp])
(cp/add-classpath "/Users/kyle/src/tmp/eve-tui/src")

(require '[charm.message :as msg]
         '[charm.style.core :as style]
         '[charm.components.text-input :as ti]
         '[eve.atom :as ea]
         '[eve-tui.session :as session]
         '[clojure.string :as str])

;; ---------------------------------------------------------------------------
;; Persistence
;; ---------------------------------------------------------------------------

(def db-path "/tmp/eve-sheet/")
(.mkdirs (java.io.File. db-path))
(def sheet-atom (ea/persistent-atom {:id :sheet/grid :persistent db-path} {}))

;; ---------------------------------------------------------------------------
;; Grid config
;; ---------------------------------------------------------------------------

(def COLS 6)
(def ROWS 12)
(def CELL-W 12)
(def ROW-HDR 4)
(def GRID-W (+ ROW-HDR (* (inc COLS) CELL-W)))

(def col-labels (mapv #(str (char (+ (int \A) %))) (range COLS)))

(defn cell-key [row col]
  (str (col-labels col) (inc row)))

;; ---------------------------------------------------------------------------
;; Styles
;; ---------------------------------------------------------------------------

(def title-s   (style/style :fg style/magenta :bold true))
(def col-hdr-s (style/style :fg style/white   :bold true))
(def row-hdr-s (style/style :fg style/white   :bold true))
(def cell-s    (style/style :fg style/white))
(def sel-s     (style/style :fg style/black :bg style/cyan :bold true))
(def edit-s    (style/style :fg style/green :bold true))
(def ycol-s    (style/style :fg style/yellow :bold true))
(def yrow-s    (style/style :fg style/yellow :bold true))
(def hint-s    (style/style :fg 240))
(def bar-key-s (style/style :fg style/cyan   :bold true))
(def ro-sel-s  (style/style :fg style/black :bg 240 :bold true))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn pad [s n]
  (let [s (str s) len (count s)]
    (if (>= len n) (subs s 0 n) (str s (apply str (repeat (- n len) \space))))))

(defn cur-key [state]
  (cell-key (get-in state [:cursor :row])
            (get-in state [:cursor :col])))

(defn cell-val [state k] (get (:grid state) k ""))

(defn move [{:keys [cursor] :as state} dr dc]
  (assoc state :cursor
         {:row (max 0 (min (dec ROWS) (+ (:row cursor) dr)))
          :col (max 0 (min (dec COLS) (+ (:col cursor) dc)))}))

(defn enter-edit [state pre]
  (let [k (cur-key state)
        v (if (seq pre) pre (cell-val state k))]
    (-> state
        (assoc :mode :edit)
        (assoc :input (ti/text-input :prompt "" :value v :focused true)))))

(defn commit [state]
  (let [k (cur-key state)
        v (str/trim (ti/value (:input state)))
        new-grid (swap! sheet-atom #(if (str/blank? v) (dissoc % k) (assoc % k v)))]
    (-> state (assoc :grid new-grid) (assoc :mode :nav) (assoc :input nil))))

(defn cancel [state]
  (-> state (assoc :mode :nav) (assoc :input nil)))

(defn clear-cell [state]
  (assoc state :grid (swap! sheet-atom #(dissoc % (cur-key state)))))

;; ---------------------------------------------------------------------------
;; Init
;; ---------------------------------------------------------------------------

(defn init []
  {:grid   @sheet-atom
   :cursor {:row 0 :col 0}
   :mode   :nav
   :input  nil})

;; ---------------------------------------------------------------------------
;; Update
;; ---------------------------------------------------------------------------

(defn update-fn [state msg]
  (cond
    ;; Sync grid on tick; cancel edit if lock was stolen
    (= (:type msg) :session/tick)
    [(assoc state :grid @sheet-atom) nil]

    (= (:type msg) :session/stolen)
    [(cancel state) nil]

    ;; Edit mode
    (= (:mode state) :edit)
    (cond
      (msg/key-match? msg "enter") [(commit state) nil]
      (msg/key-match? msg "esc")   [(cancel state) nil]
      :else
      (let [[new-in cmd] (ti/text-input-update (:input state) msg)]
        [(assoc state :input new-in) cmd]))

    ;; Nav mode
    :else
    (let [can-edit (session/holding? state)]
      (cond
        (msg/key-match? msg :up)         [(move state -1  0) nil]
        (msg/key-match? msg :down)       [(move state  1  0) nil]
        (msg/key-match? msg :left)       [(move state  0 -1) nil]
        (msg/key-match? msg :right)      [(move state  0  1) nil]
        (msg/key-match? msg "tab")       [(move state  0  1) nil]
        (msg/key-match? msg "shift+tab") [(move state  0 -1) nil]
        (and can-edit (msg/key-match? msg "enter"))  [(enter-edit state "") nil]
        (and can-edit (msg/key-match? msg "d"))      [(clear-cell state) nil]
        (and can-edit (session/printable-key? msg))  [(enter-edit state (:key msg)) nil]
        :else [state nil]))))

;; ---------------------------------------------------------------------------
;; View
;; ---------------------------------------------------------------------------

(defn view [state]
  (let [{:keys [grid cursor mode input]} state
        cur-row  (:row cursor)
        cur-col  (:col cursor)
        k        (cur-key state)
        can-edit (session/holding? state)]
    (str
     (style/render title-s "Eve Sheets")
     "  "
     (style/render bar-key-s k)
     (when-let [v (get grid k)]
       (str "  " (style/render hint-s v)))
     "\n"
     (session/lock-bar state)
     "\n\n"

     ;; Column headers
     (apply str (repeat ROW-HDR \space))
     (str/join " "
               (map-indexed
                (fn [ci label]
                  (style/render (if (= ci cur-col) ycol-s col-hdr-s)
                                (pad (str " " label) CELL-W)))
                col-labels))
     "\n"
     (apply str (repeat GRID-W \-))
     "\n"

     ;; Rows
     (str/join "\n"
               (for [ri (range ROWS)]
                 (str
                  (style/render (if (= ri cur-row) yrow-s row-hdr-s)
                                (pad (str " " (inc ri) " ") ROW-HDR))
                  (str/join " "
                            (for [ci (range COLS)]
                              (let [ck       (cell-key ri ci)
                                    v        (get grid ck "")
                                    selected (and (= ri cur-row) (= ci cur-col))
                                    s        (cond
                                               (and selected (= mode :edit)) edit-s
                                               (and selected can-edit)       sel-s
                                               selected                      ro-sel-s
                                               :else                         cell-s)]
                                (style/render s (pad v CELL-W))))))))

     "\n"
     (apply str (repeat GRID-W \-))
     "\n"
     (if (= mode :edit)
       (str (style/render edit-s (str " " k ": "))
            (ti/text-input-view input)
            "\n"
            (style/render hint-s " enter: confirm  esc: cancel"))
       (if can-edit
         (style/render hint-s " arrows/tab: move  type/enter: edit  d: clear  F3: release  q: quit")
         (style/render hint-s " arrows/tab: move  F1: take  F2: force-take  q: quit"))))))

;; ---------------------------------------------------------------------------
;; Run
;; ---------------------------------------------------------------------------

(session/run
 {:db-path    db-path
  :init       init
  :update     update-fn
  :view       view
  :quit?      (fn [state msg]
                (and (= (:mode state) :nav)
                     (msg/key-match? msg "q")))})
