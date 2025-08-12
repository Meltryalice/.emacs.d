;; init-org.el --- Initialize Org configurations.	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org-auto-tangle
  :ensure t
  :hook
  (org-mode . org-auto-tangle-mode)
  (org-mode . org-indent-mode)
  (org-mode . variable-pitch-mode)
  :config
  (setq org-auto-tangle-default t)
  )

(use-package org
  :hook
  (org-mode . visual-line-mode)
  :config
  (custom-set-faces
   '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
   ;;'(org-block-begin-line ((t (:extend t :background "#f7e0c3" :foreground "gray"
   ;;                        :weight semi-bold :height 151 :family "Maple Mono NF CN"))))
   '(org-code ((t (:foreground "#957f5f" :family "Maple Mono NF CN"))))
   '(org-document-title ((t (:foreground "midnight blue" :weight bold :height 1.75))))
   '(org-hide ((t (:foreground "#E5E9F0" :height 0.1))))

   '(org-list-dt ((t (:foreground "#7382a0"))))
   ;;'(org-verbatim ((t (:foreground "#81895d" :family "Latin Modern Mono"))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   ;; TODO set the color following this
   ;;'(org-block ((t (:extend t :background "#f7e0c3" :foreground "#5b5143" :family "Latin Modern Mono"))))
   ;;'(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(variable-pitch ((t (:family "Maple Mono NF CN" :height 150))))
   '(fixed-pitch ((t (:family "Maple Mono NF CN" :height 130))))
   ;;'(org-level-8 ((t (,@headline ,@variable-tuple))))
   ;;'(org-level-7 ((t (,@headline ,@variable-tuple))))
   ;;'(org-level-6 ((t (,@headline ,@variable-tuple))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0 :family ))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.05 :family ))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1 ))))
   '(org-level-2 ((t (:inherit outline-2 :foreground "#EEC591" :height 1.15 ))))
   '(org-level-1 ((t (:inherit outline-1 :foreground "#076678" :weight extra-bold
		      :height 1.2 ))))
   '(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
   '(org-block-begin-line ((t (:underline t :background unspecified))))
   '(org-block-end-line ((t (:overline t :underline nil :background unspecified)))))
  (add-hook 'org-mode-hook  (lambda ()
		     (setq prettify-symbols-alist
		         '(("lambda" . ?λ)
			   (":PROPERTIES:" . ? )
			   (":ID:" . ?)
			   (":END:" . ?)
			   ("#+TITLE:" . ? )
			   ("#+title:" . ? )
			   ("#+DATE:" . ? )
			   ("#+date:" . ? )
			   ("#+AUTHOR:" . ?)
			   ("#+BEGIN_QUOTE" . ? )
			   ("#+END_QUOTE" . ? )
			   ("#+RESULTS:" . ? )
			   ("[ ]" . ? )
			   ("[-]" . ? )
			   ("[X]" . ?)
			   ("#+begin_src" . ?✎)
			   ("#+BEGIN_SRC" . ?✎)
			   ("#+END_SRC"   . ?□)
			   ("#+end_src"   . ?□)
			   ("[#A]" . ?🅐)
			   ("[#B]" . ?🅑 )
			   ("[#C]" . ?🅒 )))
		     (prettify-symbols-mode)))
  :custom
  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "capture.org" org-directory))
  (org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))
  (org-fontify-whole-heading-line t)
  (org-ellipsis " ▾")
  (org-loop-over-headlines-in-active-region t)
  (org-fontify-todo-headline t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-macro-markers t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native script entities))
  (org-pretty-entities t)
  (org-use-sub-superscripts '{})
  (org-startup-indented nil)
  (org-adapt-indentation nil)
  (org-startup-with-inline-images t)
  (org-startup-folded 'overview)
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h@/!)" "WIP(i@/!)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCELLED(c@/!)")
 		         (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")))
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
 				       ("HOLD"       :foreground "#feb24c" :weight bold)
 				       ("WIP"        :foreground "#0098dd" :weight bold)
 				       ("WAIT"       :foreground "#9f7efe" :weight bold)
 				       ("DONE"       :foreground "#50a14f" :weight bold)
 				       ("CANCELLED"  :foreground "#ff6480" :weight bold)
 				       ("REPORT"     :foreground "magenta" :weight bold)
			    ("BUG"        :foreground "red"     :weight bold)
 				    ("KNOWNCAUSE" :foreground "yellow"  :weight bold)
 				    ("FIXED"      :foreground "green"   :weight bold)))
  (org-log-done 'note))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-modern
  :ensure t
  :defer t
  :hook (after-init . (lambda ()
                 (setq org-modern-hide-stars 'leading)
                 (global-org-modern-mode t)))
  :config
  (setq-default line-spacing 0.1)
  (setq org-modern-label-border 1)
  (setq org-modern-list
      '((?- . "•")
       (?+ . "◦")
       (?* . "▹")))
  :custom
  (org-modern-timestamp nil)
  (org-modern-progress nil)
  (org-modern-star nil)
  (org-modern-priority nil)
  (org-modern-list nil)
  (org-modern-checkbox nil)
  (org-modern-todo t)
  (org-modern-block-fringe t)
  (org-modern-block-name nil)
  (org-modern-keyword nil)

  ;; Editor settings
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t))

(use-package org-appear
  :ensure t
  :defer t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t)
  )

(use-package calendar
  :ensure nil
  :hook (calendar-today-visible . calendar-mark-today)
  :custom
  (calendar-chinese-all-holidays-flag nil)
  (calendar-mark-holidays-flag t)
  (calendar-mark-diary-entries-flag nil)
  (calendar-time-zone-style 'numeric)
  (calendar-date-style 'iso)
  (calendar-chinese-celestial-stem ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (calendar-chinese-terrestrial-branch ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
  (calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"])
  (calendar-day-name-array ["日" "一" "二" "三" "四" "五" "六"])
  (calendar-week-start-day 1)
  )

(use-package parse-time
   :ensure nil
   :defer t
   :config
   (setq parse-time-months
         (append '(("yiy" . 1) ("ery" . 2) ("sany" . 3)
                   ("siy" . 4) ("wuy" . 5) ("liuy" . 6)
                   ("qiy" . 7) ("bay" . 8) ("jiuy" . 9)
                   ("shiy" . 10) ("shiyiy" . 11) ("shiery" . 12)
                   ("yiyue" . 1) ("eryue" . 2) ("sanyue" . 3)
                   ("siyue" . 4) ("wuyue" . 5) ("liuyue" . 6)
                   ("qiyue" . 7) ("bayue" . 8) ("jiuyue" . 9)
                   ("shiyue" . 10) ("shiyiyue" . 11) ("shieryue" . 12))
                 parse-time-months))
 
   (setq parse-time-weekdays
         (append '(("zri" . 0) ("zqi" . 0)
                   ("zyi" . 1) ("zer" . 2) ("zsan" . 3)
                   ("zsi" . 4) ("zwu" . 5) ("zliu" . 6)
                   ("zr" . 0) ("zq" . 0)
                   ("zy" . 1) ("ze" . 2) ("zs" . 3)
                   ("zsi" . 4) ("zw" . 5) ("zl" . 6))
               parse-time-weekdays)))

(use-package cal-china-x
  :ensure t
  :commands cal-china-x-setup
  :hook (after-init . cal-china-x-setup)
  :config
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays
      '(;;公历节日
       (holiday-fixed 1 1 "元旦")
       (holiday-fixed 2 14 "情人节")
       (holiday-fixed 3 8 "妇女节")
       (holiday-fixed 3 14 "白色情人节")
       (holiday-fixed 4 1 "愚人节")
       (holiday-fixed 5 1 "劳动节")
       (holiday-fixed 5 4 "青年节")
       (holiday-float 5 0 2 "母亲节")
       (holiday-fixed 6 1 "儿童节")
       (holiday-float 6 0 3 "父亲节")
       (holiday-fixed 9 10 "教师节")
       (holiday-fixed 10 1 "国庆节")
       (holiday-fixed 10 2 "国庆节")
       (holiday-fixed 10 3 "国庆节")
       (holiday-fixed 10 24 "程序员节")
       (holiday-fixed 11 11 "双11购物节")
       (holiday-fixed 12 25 "圣诞节")
       ;; 农历节日
       (holiday-lunar 12 30 "春节" 0)
       (holiday-lunar 1 1 "春节" 0)
       (holiday-lunar 1 2 "春节" 0)
       (holiday-lunar 1 15 "元宵节" 0)
       (holiday-solar-term "清明" "清明节")
       (holiday-solar-term "小寒" "小寒")
       (holiday-solar-term "大寒" "大寒")
       (holiday-solar-term "立春" "立春")
       (holiday-solar-term "雨水" "雨水")
       (holiday-solar-term "惊蛰" "惊蛰")
       (holiday-solar-term "春分" "春分")
       (holiday-solar-term "谷雨" "谷雨")
       (holiday-solar-term "立夏" "立夏")
       (holiday-solar-term "小满" "小满")
       (holiday-solar-term "芒种" "芒种")
       (holiday-solar-term "夏至" "夏至")
       (holiday-solar-term "小暑" "小暑")
       (holiday-solar-term "大暑" "大暑")
       (holiday-solar-term "立秋" "立秋")
       (holiday-solar-term "处暑" "处暑")
       (holiday-solar-term "白露" "白露")
       (holiday-solar-term "秋分" "秋分")
       (holiday-solar-term "寒露" "寒露")
       (holiday-solar-term "霜降" "霜降")
       (holiday-solar-term "立冬" "立冬")
       (holiday-solar-term "小雪" "小雪")
       (holiday-solar-term "大雪" "大雪")
       (holiday-solar-term "冬至" "冬至")
       (holiday-lunar 5 5 "端午节" 0)
       (holiday-lunar 8 15 "中秋节" 0)
       (holiday-lunar 7 7 "七夕情人节" 0)
       (holiday-lunar 12 8 "腊八节" 0)
       (holiday-lunar 9 9 "重阳节" 0)))
  (setq calendar-holidays (append cal-china-x-general-holidays)))

(use-package org-capture
  :ensure nil
  :bind ("\e\e c" . (lambda () (interactive) (org-capture)))
  :hook ((org-capture-mode . (lambda ()
                     (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
       (org-capture-mode . delete-other-windows))
  :custom
  (org-capture-use-agenda-date nil)
  ;; define common template
  (org-capture-templates `(("t" "Tasks" entry (file+headline "tasks.org" "Reminders")
                   "* TODO %i%?"
                   :empty-lines-after 1
                   :prepend t)
                   ("n" "Notes" entry (file+headline "capture.org" "Notes")
                    "* %? %^g\n%i\n"
                    :empty-lines-after 1)
                   ;; For EWW
                   ("b" "Bookmarks" entry (file+headline "capture.org" "Bookmarks")
                    "* %:description\n\n%a%?"
                    :empty-lines 1
                    :immediate-finish t)
                   ("d" "Diary")
                   ("dt" "Today's TODO list" entry (file+olp+datetree "diary.org")
                    "* Today's TODO list [/]\n%T\n\n** TODO %?"
                    :empty-lines 1
                    :jump-to-captured t)
                   ("do" "Other stuff" entry (file+olp+datetree "diary.org")
                    "* %?\n%T\n\n%i"
                    :empty-lines 1
                    :jump-to-captured t)
                   ))
  )

(use-package org-agenda
  :ensure nil
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :bind (("\e\e a" . org-agenda)
       :map org-agenda-mode-map
       ("i" . (lambda () (interactive) (org-capture nil "d")))
       ("J" . consult-org-agenda))
  :config
  ;; 日程模式的日期格式设置
  (setq org-agenda-format-date 'org-agenda-format-date-aligned)
  (defun org-agenda-format-date-aligned (date)
   "Format a DATE string for display in the daily/weekly agenda, or timeline.
 
 This function makes sure that dates are aligned for easy reading."
   (require 'cal-iso)
   (let* ((dayname (aref cal-china-x-days
                  (calendar-day-of-week date)))
        (day (cadr date))
        (month (car date))
        (year (nth 2 date))
        (day-of-week (calendar-day-of-week date))
        (iso-week (org-days-to-iso-week
               (calendar-absolute-from-gregorian date)))
        (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
        (cn-month (cl-caddr cn-date))
        (cn-day (cl-cadddr cn-date))
        (cn-month-string (concat (aref cal-china-x-month-name
                             (1- (floor cn-month)))
                         (if (integerp cn-month)
                            ""
                           "（闰月）")))
        (cn-day-string (aref cal-china-x-day-name
                      (1- cn-day)))
        (extra (format " 农历%s%s%s%s"
                  (if (or (eq org-agenda-current-span 'day)
                       (= day-of-week 1)
                       (= cn-day 1))
                     cn-month-string
                   "")
                  (if (or (= day-of-week 1)
                       (= cn-day 1))
                     (if (integerp cn-month) "" "[闰]")
                   "")
                  cn-day-string
                  (if (or (= day-of-week 1)
                       (eq org-agenda-current-span 'day))
                     (format " 今年第%02d周" iso-week)
                   "")
                  ))
        )
     (format "%04d-%02d-%02d 星期%s%s%s\n" year month
           day dayname extra (concat " 第" (format-time-string "%j") "天"))))
  
  ;; 显示时间线
  (setq org-agenda-use-time-grid t)
  ;; 设置面包屑分隔符
  ;; (setq org-agenda-breadcrumbs-separator " ❱ ")
  ;; 设置时间线的当前时间指示串
  (setq org-agenda-current-time-string "⏰------------now")
  ;; 时间线范围和颗粒度设置
  (setq org-agenda-time-grid (quote ((daily today)
                         (0600 0800 1000 1200
                             1400 1600 1800
                             2000 2200 2400)
                         "......" "----------------")))
  ;; 日程视图的前缀设置
  (setq org-agenda-prefix-format '((agenda . " %i %-25:c %5t %s")
                        (todo   . " %i %-25:c ")
                        (tags   . " %i %-25:c ")
                        (search . " %i %-25:c ")))
  ;; 对于计划中的任务在视图里的显示
  (setq org-agenda-scheduled-leaders
      '("计划 " "应在%02d天前开始 "))
  ;; 对于截止日期的任务在视图里的显示
  (setq org-agenda-deadline-leaders
      '("截止 " "还有%02d天到期 " "已经过期%02d天 "))
  
  ;; =====================
  ;; 自定义日程视图，分别显示TODO，WIP，WIAT中的任务
  ;; n键显示自定义视图，p键纯文本视图，a键默认视图
  ;; =====================
  (defvar my-org-custom-daily-agenda
   `((todo "TODO"
         ((org-agenda-block-separator nil)
          (org-agenda-overriding-header "所有待办任务\n")))
     (todo "WIP"
         ((org-agenda-block-separator nil)
          (org-agenda-overriding-header "\n进行中的任务\n")))
     (todo "WAIT"
         ((org-agenda-block-separator nil)
          (org-agenda-overriding-header "\n等待中的任务\n")))
     (agenda "" ((org-agenda-block-separator nil)
             (org-agenda-overriding-header "\n今日日程\n"))))
   "Custom agenda for use in `org-agenda-custom-commands'.")
  (setq org-agenda-custom-commands
      `(("n" "Daily agenda and top priority tasks"
        ,my-org-custom-daily-agenda)
       ("p" "Plain text daily agenda and top priorities"
        ,my-org-custom-daily-agenda
        ((org-agenda-with-colors nil)
         (org-agenda-prefix-format "%t %s")
         (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
         (org-agenda-fontify-priorities nil)
         (org-agenda-remove-tags t))
        ("agenda.txt"))))
  
  ;; 时间戳格式设置，会影响到 `svg-tag' 等基于正则的设置
  ;; 这里设置完后是 <2022-12-24 星期六> 或 <2022-12-24 星期六 06:53>
  (setq system-time-locale "zh_CN.UTF-8")
  (setq org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
  ;; 不同日程类别间的间隔
  (setq org-cycle-separator-lines 2)
  :custom
  ;; 设置需要被日程监控的org文件
  (org-agenda-files
   (list (expand-file-name "tasks.org" org-directory)
       (expand-file-name "diary.org" org-directory)
       (expand-file-name "habits.org" org-directory)
       (expand-file-name "mail.org" org-directory)
       ))
  ;; 设置org的日记文件
  (org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  ;; 日记插入精确时间戳
  (org-agenda-insert-diary-extract-time t)
  ;; 设置日程视图更加紧凑
  ;; (org-agenda-compact-blocks t)
  ;; 日程视图的块分隔符
  (org-agenda-block-separator ?─)
  ;; 日视图还是周视图，通过 v-d, v-w, v-m, v-y 切换视图，默认周视图
  (org-agenda-span 'day)
  ;; q退出时删除agenda缓冲区
  (org-agenda-sticky t)
  ;; 是否包含直接日期
  (org-agenda-include-deadlines t)
  ;; 禁止日程启动画面
  (org-agenda-inhibit-startup t)
  ;; 显示每一天，不管有没有条目
  (org-agenda-show-all-dates t)
  ;; 时间不足位时前面加0
  (org-agenda-time-leading-zero t)
  ;; 日程同时启动log mode
  (org-agenda-start-with-log-mode t)
  ;; 日程同时启动任务时间记录报告模式
  (org-agenda-start-with-clockreport-mode t)
  ;; 截止的任务完成后不显示
  ;; (org-agenda-skip-deadline-if-done t)
  ;; 当计划的任务完成后不显示
  (org-agenda-skip-scheduled-if-done t)
  ;; 计划过期上限
  (org-scheduled-past-days 365)
  ;; 计划截止上限
  (org-deadline-past-days 365)
  ;; 计划中的任务不提醒截止时间
  (org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  ;; 设置工时记录报告格式
  (org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 5 :fileskip0 t :compact nil :narrow 80))
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  ;; 标签显示的位置，第100列往前右对齐
  (org-agenda-tags-column -100)
  ;; 从星期一开始作为一周第一天
  (org-agenda-start-on-weekday 1)
  ;; 是否使用am/pm
  ;; (org-agenda-timegrid-use-ampm nil)
  ;; 搜索是不看时间
  (org-agenda-search-headline-for-time nil)
  ;; 提前3天截止日期到期告警
  (org-deadline-warning-days 3)
  )

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode-in-directories)
  :bind (("C-c d n" . denote)
       ("C-c d d" . denote-date)
       ("C-c d t" . denote-type)
       ("C-c d s" . denote-subdirectory)
       ("C-c d f" . denote-open-or-create)
       ("C-c d r" . denote-dired-rename-file))
  :init
  (with-eval-after-load 'org-capture
   (setq denote-org-capture-specifiers "%l\n%i\n%?")
   (add-to-list 'org-capture-templates
            '("N" "New note (with denote.el)" plain
             (file denote-last-path)
             #'denote-org-capture
             :no-save t
             :immediate-finish nil
             :kill-buffer t
             :jump-to-captured t)))
  :config
  (setq denote-directory (expand-file-name "~/org/denote/"))
  (setq denote-known-keywords '("entertainment" "reading" "studying"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  ;; org is default, set others such as text, markdown-yaml, markdown-toml
  (setq denote-file-type nil)
  (setq denote-prompts '(title keywords))

  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more rigid workflow.
  (setq denote-allow-multi-word-keywords t)
  (setq denote-date-format nil)

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (setq denote-dired-rename-expert nil)

  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  )

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
