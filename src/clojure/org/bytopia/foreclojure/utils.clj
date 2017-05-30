(ns org.bytopia.foreclojure.utils
  (:require [neko.activity :as a]
            [neko.listeners.view :refer [on-click-call]]
            [neko.notify :refer [toast]]
            [neko.resource :refer [get-string]]
            [neko.threading :refer [on-ui]]
            [neko.ui :as ui]
            [neko.ui.mapping :refer [defelement]]
            [neko.ui.menu :as menu]
            [neko.ui.traits :as traits]
            [neko.-utils :as u]
            [neko.debug :refer [*a]])
  (:import android.content.Context
           android.app.Activity
           android.view.View
           android.os.Bundle
           android.os.Environment
           android.support.v4.widget.SwipeRefreshLayout$OnRefreshListener
           android.support.design.widget.Snackbar
           android.support.v4.view.ViewCompat
           [android.support.v4.widget DrawerLayout DrawerLayout$DrawerListener]
           [android.support.v7.app AppCompatActivity ActionBarDrawerToggle]
           java.util.HashMap
           ;; 讯飞的jar包
           [com.iflytek.cloud
            SpeechUtility
            ErrorCode
            InitListener
            SpeechConstant
            SpeechError
            SpeechSynthesizer
            SynthesizerListener
            InitListener
            RecognizerListener
            RecognizerResult
            SpeechRecognizer]
           com.iflytek.cloud.resource.Resource
           [com.iflytek.cloud.ui
            RecognizerDialog
            RecognizerDialogListener]))

;; 讯飞语音模块 =========
;; context => 当前页面所在位置: (*a :main)
;;  => #object[org.bytopia.foreclojure.ProblemGridActivity 0x1cf5d25f "org.bytopia.foreclojure.ProblemGridActivity@1cf5d25f"]

(def appid (str SpeechConstant/APPID "=5763509a"))

;; (initialize-xunfei (*a :main))
;; 初始化即创建语音配置对象，只有初始化后才可以使用MSC的各项服务。建议将初始化放在程序入口处（如Application、Activity的onCreate方法）
(defn initialize-xunfei
  [^Context context]
  (SpeechUtility/createUtility context appid))

(defn mSynListener
  []
  (proxy [SynthesizerListener] []
    (onCompleted [_])
    (onBufferProgress [^Integer percent  ^Integer beginPos  ^Integer endPos ^String info])
    (onSpeakBegin [])
    (onSpeakPaused [])
    (onSpeakProgress [^Integer percent ^Integer beginPos ^Integer endPos])
    (onSpeakResumed [])
    )
  )

;; 第一次必须先初始化: (initialize-xunfei (*a :main)) 
;; (str-to-voice (*a :main) "Hi,瞧布施" (mSynListener))
(defn str-to-voice
  [^Context context text mSynListener]
  (let [mTts (SpeechSynthesizer/createSynthesizer  context nil)
        _ (.setParameter mTts SpeechConstant/ENGINE_TYPE SpeechConstant/TYPE_CLOUD)
        _ (.setParameter mTts SpeechConstant/ENGINE_MODE SpeechConstant/MODE_MSC)
        _ (.setParameter mTts SpeechConstant/VOICE_NAME "xiaoyan")]
    (.startSpeaking mTts text mSynListener)
    )
  )


;; ===============
;; 语音识别

(def reco-result (atom ""))

(defn mlistener
  []
  (proxy [RecognizerListener] []
    (onVolumeChanged
      [^Integer volume data]
      ;; D/my.company.xunfeiclj.main(13303): 当前正在说话，音量大小:  0
      (neko.log/d "当前正在说话，音量大小: " volume)
      ;; D/my.company.xunfeiclj.main(13303): 返回音频数据:  #object[[B 0x18faca3d [B@18faca3d]
      (neko.log/d "返回音频数据: " data)
      )
    (onEndOfSpeech []
      (neko.log/d "结束说话" 1111111)
      )
    (onBeginOfSpeech []
      (neko.log/d "开始说话" 2222222)
      )
    (onError [^SpeechError error]
      (.getPlainDescription error true)
      (neko.log/d "识别错误33333333:" error)
      )
    (onEvent [^Integer arg0 ^Integer arg1 ^Integer arg2 ^Bundle arg3]
      (neko.log/d "事件ARG0~3: 5555555" (str arg0 "======" arg1 "======" arg2 "========" arg3))
      )
    (onResult [^RecognizerResult results ^Boolean isLast]
      (neko.log/d "返回识别结果: 6666666" (str results "##############" isLast))
      (neko.log/d "识别结果数据:" (.getResultString results))
      (reset! reco-result (.getResultString results))
      )
    )
  )

;; 初始化监听器
(defn mInitListener
  []
  (proxy [InitListener] []
    (onInit [^Integer code])
    ))

;; (initialize-xunfei (*a))
;; (try (start-listening (*a) (mlistener)) (catch Exception e (prn e)))
(defn start-listening
  [context mlistener]
  (let [mIat (SpeechRecognizer/createRecognizer context (mInitListener))
        _ (.setParameter mIat SpeechConstant/CLOUD_GRAMMAR nil)
        _ (.setParameter mIat SpeechConstant/SUBJECT nil)
        _ (.setParameter mIat SpeechConstant/ENGINE_TYPE SpeechConstant/TYPE_CLOUD)
        ;; 没有用本地的呀: SpeechConstant/TYPE_LOCAL
        _ (.setParameter mIat SpeechConstant/ENGINE_MODE SpeechConstant/MODE_MSC)
        _ (.setParameter mIat SpeechConstant/DOMAIN "iat")
        _ (.setParameter mIat SpeechConstant/LANGUAGE "zh_cn")
        _ (.setParameter mIat SpeechConstant/ACCENT "mandarin")
        _ (.setParameter mIat SpeechConstant/NET_CHECK "false")
        ]
    (neko.log/d "开始监听" mIat)
    (.startListening mIat mlistener)
    )
  )

;; ===============

(defn ellipsize [s max-length]
  (let [lng (count s)]
    (if (> lng max-length)
      (str (subs s 0 max-length) "…") s)))

(defmacro long-running-job
  "Runs body in a future, initializing progress with `progress-start`
  expression, and ending it with `progress-stop`."
  [progress-start progress-stop & body]
  (let [asym (with-meta 'a {:tag android.app.Activity})]
    `(future
       (on-ui ~progress-start)
       (try ~@body
            (catch Exception ex# (on-ui (toast (str ex#))))
            (finally (on-ui ~progress-stop))))))

(defn on-refresh-listener-call
  [callback swipe-layout]
  (reify SwipeRefreshLayout$OnRefreshListener
    (onRefresh [_]
      (u/call-if-nnil callback swipe-layout))))

(defn snackbar
  ([view-or-activity text duration]
   (snackbar view-or-activity text duration nil nil))
  ([view-or-activity text duration action-text action-callback]
   (let [sb (Snackbar/make ^View (if (instance? Activity view-or-activity)
                                   (a/get-decor-view view-or-activity)
                                   view-or-activity)
                           (get-string text)
                           duration)]
     (when action-text
       (.setAction sb action-text (on-click-call action-callback)))
     (.show sb))))

(neko.ui.mapping/defelement :drawer-layout
  :classname android.support.v4.widget.DrawerLayout
  :inherits :view-group
  :traits [:drawer-toggle])

(neko.ui.mapping/defelement :navigation-view
  :classname android.support.design.widget.NavigationView
  :inherits :frame-layout
  :traits [:navbar-menu :navbar-header-view])

(neko.ui.traits/deftrait :drawer-layout-params
  "docs"
  {:attributes (concat (deref #'neko.ui.traits/margin-attributes)
                       [:layout-width :layout-height
                        :layout-weight :layout-gravity])
   :applies? (= container-type :drawer-layout)}
  [^View wdg, {:keys [layout-width layout-height layout-weight layout-gravity]
               :as attributes}
   {:keys [container-type]}]
  (let [^int width (->> (or layout-width :wrap)
                        (neko.ui.mapping/value :layout-params)
                        (neko.ui.traits/to-dimension (.getContext wdg)))
        ^int height (->> (or layout-height :wrap)
                         (neko.ui.mapping/value :layout-params)
                         (neko.ui.traits/to-dimension (.getContext wdg)))
        weight (or layout-weight 0)
        params (android.support.v4.widget.DrawerLayout$LayoutParams. width height weight)]
    (#'neko.ui.traits/apply-margins-to-layout-params (.getContext wdg) params attributes)
    (when layout-gravity
      (set! (. params gravity)
            (neko.ui.mapping/value :layout-params layout-gravity :gravity)))
    (.setLayoutParams wdg params)))

(neko.ui.traits/deftrait :drawer-toggle
  "docs"
  {:attributes [:drawer-open-text :drawer-closed-text :drawer-indicator-enabled
                :on-drawer-closed :on-drawer-opened]}
  [^DrawerLayout wdg, {:keys [drawer-open-text drawer-closed-text
                              drawer-indicator-enabled
                              on-drawer-opened on-drawer-closed]}
   {:keys [^View id-holder]}]
  (let [toggle (proxy [ActionBarDrawerToggle DrawerLayout$DrawerListener]
                   [^android.app.Activity (.getContext wdg)
                    wdg
                    ^int (or drawer-open-text android.R$string/untitled)
                    ^int (or drawer-closed-text android.R$string/untitled)]
                 (onDrawerOpened [view]
                   (neko.-utils/call-if-nnil on-drawer-opened view))
                 (onDrawerClosed [view]
                   (neko.-utils/call-if-nnil on-drawer-closed view)))]
    (.setDrawerIndicatorEnabled toggle (boolean drawer-indicator-enabled))
    (.setDrawerListener wdg toggle)
    (when id-holder
      (.put ^HashMap (.getTag id-holder) :neko.ui/drawer-toggle toggle))))

(neko.ui.mapping/defelement :swipe-refresh-layout
  :classname android.support.v4.widget.SwipeRefreshLayout
  :traits [:on-refresh])

(neko.ui.traits/deftrait :on-refresh
  "docs "
  [^android.support.v4.widget.SwipeRefreshLayout wdg, {:keys [on-refresh]} _]
  (.setOnRefreshListener wdg (on-refresh-listener-call on-refresh wdg)))

(neko.ui.traits/deftrait :elevation
  "docs "
  [^View wdg, {:keys [elevation]} _]
  (ViewCompat/setElevation wdg (neko.ui.traits/to-dimension
                                (.getContext wdg) elevation)))

(neko.ui.traits/deftrait :navbar-header-view
  "docs "
  {:attributes [:header]}
  [^android.support.design.widget.NavigationView wdg, {:keys [header]} opts]
  (.addHeaderView wdg (ui/make-ui-element (.getContext wdg) header opts)))

(neko.ui.traits/deftrait :navbar-menu
  "docs "
  {:attributes [:menu]}
  [^android.support.design.widget.NavigationView wdg, {:keys [menu]} _]
  (menu/make-menu (.getMenu wdg) menu))

(neko.ui.mapping/add-trait! :view :drawer-layout-params)
(neko.ui.mapping/add-trait! :view :elevation)
(neko.ui.mapping/add-trait! :swipe-refresh-layout :drawer-layout-params)

(swap! (deref #'neko.ui.mapping/reverse-mapping) assoc android.widget.ProgressBar :progress-bar)
