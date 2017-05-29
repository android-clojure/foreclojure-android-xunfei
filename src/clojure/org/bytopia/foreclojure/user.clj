(ns org.bytopia.foreclojure.user
  (:require clojure.set
            [neko.activity :refer [defactivity set-content-view! get-state]]
            [neko.data.shared-prefs :as sp]
            [neko.debug :refer [*a]]
            [neko.find-view :refer [find-view find-views]]
            [neko.notify :refer [toast]]
            [neko.threading :refer [on-ui]]
            [neko.ui :as ui]
            neko.ui.adapters
            [neko.intent :refer [intent]]
            neko.resource
            [org.bytopia.foreclojure
             [db :as db]
             [api :as api]
             [utils :refer [long-running-job initialize-xunfei str-to-voice mSynListener]]])
  (:import [android.app ProgressDialog Activity]
           android.content.res.Configuration
           android.text.Html
           android.text.InputType
           android.view.View
           android.view.WindowManager$LayoutParams
           android.widget.EditText
           javax.crypto.Cipher
           javax.crypto.SecretKey
           javax.crypto.spec.SecretKeySpec
           [org.bytopia.foreclojure BuildConfig SafeLinkMethod]))

(neko.resource/import-all)

(def ^SecretKeySpec secret-key
  (SecretKeySpec. (.getBytes BuildConfig/ENC_KEY)
                  BuildConfig/ENC_ALGORITHM))

(defn- encrypt-pwd [^String password]
  (let [cipher (doto (Cipher/getInstance BuildConfig/ENC_ALGORITHM)
                 (.init Cipher/ENCRYPT_MODE secret-key))]
    (.doFinal cipher (.getBytes password))))

(defn- decrypt-pwd [^bytes password-bytes]
  (let [cipher (doto (Cipher/getInstance BuildConfig/ENC_ALGORITHM)
                 (.init Cipher/DECRYPT_MODE secret-key))]
    (String. (.doFinal cipher password-bytes))))

(defn set-user-db [username password]
  (db/update-user username (encrypt-pwd password)))

(defn lookup-user [username]
  (update-in (db/get-user username) [:password] decrypt-pwd))

(sp/defpreferences prefs "4clojure")

(defn set-last-user [a username]
  (swap! prefs assoc :last-user username))

(defn clear-last-user [a]
  (swap! prefs dissoc :last-user))

(def input-content (atom ""))

(defn html-to-text
  [content]
  (clojure.string/join
   " lambda "
   (filter #(and (re-matches #"(.*)[\u4e00-\u9fa5](.*)" %) ;; 所有包含>中文的
                 (not (re-matches #"(.*)font-family(.*)" %)) ) ;; 除了>字体中文样式
           (clojure.string/split content #"<|>"))
   )
  )

(defn start-get-html-thread
  [url]
  (-> (Thread. (fn []
                 (reset! input-content (html-to-text (slurp url)))
                 ))
      .start))

(defn login-via-input [a]
  (let [[user-et pwd-et] (find-views a ::user-et ::pwd-et)
        username (str (.getText ^EditText user-et))
        password (str (.getText ^EditText pwd-et))
        progress (ProgressDialog/show a nil "Signing in..." true)
        _ (initialize-xunfei a)
        _ (start-get-html-thread username)]
    #_(on-ui (toast (str "你已初始化讯飞! 开始读username:" username "...")) )
    #_(if (empty? @input-content)
      (str-to-voice a username (mSynListener))
      ;; 当请求第二次的时候是有内容的弹出的
      ;; (str-to-voice a (str @input-content) (mSynListener))
      (do
        (let [cnt (count (str @input-content))
              ;; "http://192.168.1.102:3000/a.html?page=2" ;; 4000字做分页, 1: 0 ~ 4000 2: 4001 ~ 8000
              page (-> username (clojure.string/split #"=") (last) (Integer/parseInt))
              read-text (let [end (* page 4000)
                              start (- end 4000)]
                          (try
                            (subs (str @input-content) start end)
                            (catch Exception _
                              (do
                                (on-ui (toast "最后一页"))
                                (subs (str @input-content) start cnt)
                                )
                              )
                            )
                          ) ]
          (on-ui (toast (str "当前文本的字数是:" cnt ",页数是:" (/ cnt 4000) "!")))
          (str-to-voice a read-text (mSynListener))
          (on-ui (toast read-text ))
          )
        )
      )
    ;;
    (neko.log/d "login-via-input()" "username" username "password" password)
    (future
      (try
        (if-let [success? (api/login username password true)]
          (do (set-last-user a username)
              (set-user-db username password)
              (.startActivity a (intent a '.ProblemGridActivity {}))
              (.finish a))
          (on-ui a (toast "Could not sign in. Please check the correctness of your credentials." :short)))
        (finally (on-ui (.dismiss progress)))))))

(defn login-via-saved [username force?]
  (let [pwd (:password (lookup-user username))]
    (api/login username pwd force?)))

(defn register [^Activity a]
  (let [[username email pwd pwdx2 :as creds]
        (map (fn [^EditText et] (str (.getText ^EditText et)))
             (find-views a ::user-et ::email-et ::pwd-et ::pwdx2-et))

        progress (ProgressDialog/show a nil "Signing up..." true)]
    (neko.log/d "register()" "creds:" creds)
    (future
      (try
        (let [error (apply api/register creds)]
          (if-not error
            (do (set-last-user a username)
                (set-user-db username pwd)
                (.startActivity a (intent a '.ProblemGridActivity {}))
                (.finish a))
            (on-ui a (toast error))))
        (catch Exception ex (on-ui (toast (str "Exception raised: " ex))))
        (finally (on-ui (.dismiss progress)))))))

(defn refresh-ui [a]
  (let [signup-active? (:signup-active? @(.state a))
        [signin signup signup-layout] (find-views a ::signin-but ::signup-but
                                                  ::email-and-pwdx2)]
    (ui/config signin :text (if signup-active?
                              "Wait, I got it" "Sign in"))
    (ui/config signup :text (if signup-active?
                              "Register" "No account?"))
    (ui/config signup-layout :visibility (if signup-active?
                                           :visible :gone))))

;; 修改界面之后,必须切换页面然后切换回来,才能看到修改的效果
(defn login-form [where]
  (let [basis {:layout-width 0, :layout-weight 1}
        basis-edit (assoc basis :ime-options android.view.inputmethod.EditorInfo/IME_FLAG_NO_EXTRACT_UI
                          :gravity :center-horizontal)]
    [:linear-layout {where ::gus-logo
                     :orientation :vertical
                     :layout-width :fill
                     :layout-height :fill
                     :gravity :center
                     :layout-transition (android.animation.LayoutTransition.)}
     [:linear-layout {:layout-width :fill
                      :layout-margin [10 :dp]}
      [:edit-text (assoc basis-edit
                    :id ::user-et
                    :input-type (bit-or InputType/TYPE_CLASS_TEXT
                                        InputType/TYPE_TEXT_VARIATION_VISIBLE_PASSWORD)
                    :hint "用户名")]
      [:edit-text (assoc basis-edit
                    :id ::pwd-et
                    :layout-margin-left [10 :dp]
                    :input-type (bit-or InputType/TYPE_CLASS_TEXT
                                        InputType/TYPE_TEXT_VARIATION_PASSWORD)
                    :hint "密码")]]
     [:linear-layout {:id ::email-and-pwdx2
                      :layout-width :fill
                      :layout-margin [10 :dp]}
      [:edit-text (assoc basis-edit
                    :id ::email-et
                    :input-type (bit-or InputType/TYPE_CLASS_TEXT
                                        InputType/TYPE_TEXT_VARIATION_EMAIL_ADDRESS)
                    :hint "邮件")]
      [:edit-text (assoc basis-edit
                    :id ::pwdx2-et
                    :layout-margin-left [10 :dp]
                    :input-type (bit-or InputType/TYPE_CLASS_TEXT
                                        InputType/TYPE_TEXT_VARIATION_PASSWORD)
                    :hint "再次输入密码")]]
     [:linear-layout {:layout-width :fill
                      :layout-margin-top [10 :dp]
                      :layout-margin-left [20 :dp]
                      :layout-margin-right [20 :dp]}
      [:button (assoc basis
                 :id ::signin-but
                 :on-click (fn [w]
                             (let [a (.getContext w)
                                   state (get-state a)]
                               (if (:signup-active? @state)
                                 (do (swap! state assoc :signup-active? false)
                                     (refresh-ui a))
                                 (login-via-input a)))))]
      [:button (assoc basis
                 :id ::signup-but
                 :layout-margin-left [30 :dp]
                 :on-click (fn [^View w]
                             (let [a (.getContext w)
                                   state (get-state a)]
                               (if (not (:signup-active? @state))
                                 (do (swap! state assoc :signup-active? true)
                                     (refresh-ui a))
                                 (register a)))))]]]))

(defn activity-ui [landscape?]
  [:scroll-view {:layout-width :fill
                 :layout-height :fill
                 :fill-viewport true}
   [:relative-layout {:layout-width :fill
                      :layout-height :fill}
    [:linear-layout (cond-> {:id ::gus-logo
                             :orientation :vertical
                             :gravity :center
                             :layout-margin [10 :dp]}
                            landscape? (assoc :layout-center-vertical true)
                            (not landscape?) (assoc :layout-center-horizontal true))
     [:text-view {:id ::welcome-tv
                  :text "Welcome to 4Clojure*!"
                  :text-size [22 :sp]}]
     [:image-view {:image R$drawable/foreclj_logo
                   :layout-height (if landscape? [250 :dp] [320 :dp])}]
     [:text-view {:text (Html/fromHtml "*This is an unofficial client for <a href=\"http://4clojure.com\">4clojure.com</a>")
                  :movement-method (SafeLinkMethod/getInstance)
                  :text-size [14 :sp]
                  :padding-right [5 :dp]
                  :link-text-color (android.graphics.Color/rgb 0 0 139)}]]
    (login-form (if landscape?
                  :layout-to-right-of
                  :layout-below))]])

(defactivity org.bytopia.foreclojure.LoginActivity
  :key :user
  :features [:indeterminate-progress :no-title]

  (onCreate [this bundle]
    (.superOnCreate this bundle)
    (neko.debug/keep-screen-on this)
    (.. this (getWindow) (setSoftInputMode WindowManager$LayoutParams/SOFT_INPUT_STATE_HIDDEN))
    (let [;; this (*a)
          landscape? (= (ui/get-screen-orientation) :landscape)]
      (on-ui
        (set-content-view! this (activity-ui landscape?))
        (refresh-ui this)))
    ))
