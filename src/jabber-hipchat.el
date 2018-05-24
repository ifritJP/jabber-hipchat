(require 'jabber)

(defvar jabber-hipchat-token-view-mess "") ;; mandatory
(defvar jabber-hipchat-token-view-room "") ;; mandatory
(defvar jabber-hipchat-token-view-group "") ;; mandatory
(defvar jabber-hipchat-token-adm-room "") ;; optional

(defvar jabber-hipchat-ignore-cert-p nil)
(defvar jabber-hipchat-proxy "")


(defun jabber-hipchat-my-jid ()
  (if (and jabber-buffer-connection
	   (memq jabber-buffer-connection jabber-connections))
      (jabber-connection-bare-jid jabber-buffer-connection)
    (car (car jabber-account-list))))

(defun jabber-hipchat-jid-2-user-id (jid)
  (string-match "[0-9]+_\\([0-9]+\\)@.*" jid)
  (string-to-number (replace-match "\\1" t nil jid)))

(defun jabber-hipchat-jid-2-group-id (jid)
  (string-match "\\([0-9]+\\)_[0-9]+@.*" jid)
  (string-to-number (replace-match "\\1" t nil jid)))


(defun jabber-hipchat-my-user-id ()
  (jabber-hipchat-jid-2-user-id (jabber-hipchat-my-jid)))

(defun jabber-hipchat-exec-rest-api (buffer path token &optional method)
  (when (not method)
    (setq method "GET"))
  (call-process "curl" nil (list buffer nil) nil
		(format "https://%s/%s"
			(cdr (assoc :network-server (car jabber-account-list))) path)
		"-X" method
		"--proxy" jabber-hipchat-proxy
		"-H" (concat "Authorization: Bearer " token)
		(when jabber-hipchat-ignore-cert-p
		  "-k")
		))


(setq jabber-hipchat-my-name nil)
(defun jabber-hipchat-my-name ()
  (when (not jabber-hipchat-my-name)
    (with-temp-buffer
      (jabber-hipchat-exec-rest-api (current-buffer)
				    (format "/v2/user/%d" (jabber-hipchat-my-user-id))
				    jabber-hipchat-token-view-group)
      (setq jabber-hipchat-my-name
	    (cdr (assoc 'name (json-read-from-string (buffer-string)))))
      ))
  jabber-hipchat-my-name)

(defun jabber-hipchat-get-room-id ()
  (let ((group-info-list (jabber-hipchat-get-all-room-json))
	candidates-list
	room-id room-jid)
    (setq candidates-list (mapcar (lambda (X)
				    (format "%s:%s"
					    (cdr (assoc 'id X))
					    (cdr (assoc 'name X))))
				  group-info-list))
    (cond
     ((or (featurep 'helm) (featurep 'anything))
      (funcall (if (featurep 'helm) 'helm 'anything)
	       :sources `((name . "group")
			  (candidates . ,candidates-list)
			  (action . ,(lambda (X) (setq room-id X))))))
     (t
      (setq room-id (completing-read "group?(complete with TAB): "
				     candidates-list))))
    (string-match "\\([0-9]+\\):.*" room-id)
    (setq room-id (string-to-number (replace-match "\\1" t nil room-id)))
    (setq room-jid (cdr (assoc 'xmpp_jid (jabber-hipchat-get-room-info room-id))))
    (list room-id room-jid)
    ))

(defun jabber-hipchat-start-room-chat ()
  (interactive)
  (let ((list (jabber-hipchat-get-room-id))
	room-id room-jid)
    (setq room-id (car jabber-hipchat-get-room-id))
    (setq room-jid (cadr jabber-hipchat-get-room-id))
    (jabber-muc-join (car jabber-connections)
		     room-jid (jabber-hipchat-my-name) t)
  ))


(defun jabber-hipchat-get-all-room-json ()
  (with-temp-buffer
    (jabber-hipchat-exec-rest-api (current-buffer) "/v2/room?max-results=1000"
				  jabber-hipchat-token-view-room)
    (cdr (assoc 'items (json-read-from-string (buffer-string))))
    
    ))


(defun jabber-hipchat-get-room-info (room-id)
  (with-temp-buffer
    (jabber-hipchat-exec-rest-api (current-buffer)
				  (format "/v2/room/%d" room-id )
				  jabber-hipchat-token-view-room)
    (json-read-from-string (buffer-string))))


(defun jabber-hipchat-exit-from-room (room-id)
  (with-temp-buffer
    (jabber-hipchat-exec-rest-api (current-buffer)
				  (format "/v2/room/%d/member/%d"
					   room-id (jabber-hipchat-my-user-id))
				  jabber-hipchat-token-adm-room
				  "DELETE")
    (json-read-from-string (buffer-string))
    ))


(defun jabber-hipchat-get-history-json (buffer id target token)
  (jabber-hipchat-exec-rest-api
   buffer (format "/v2/%s/%s/history?max-results=1000" target id) token))

(defun jabber-hipchat-hist-2-jabber-log (item jid)
  (let ((my-jid (jabber-hipchat-my-jid))
	message date who from)
    (if (assoc 'file item)
	(setq message (concat "File uploaded: "
			      (cdr (assoc 'url (cdr (assoc 'file item))))))
      (setq message (cdr (assoc 'message item))))
    (setq date (cdr (assoc 'date item)))
    (string-match "\\..+" date)
    (setq date (replace-match "" t nil date))
    (setq from (cdr (assoc 'from item)))
    (setq who (if (listp from)
		  (cdr (assoc 'id from))
		"JIRA"))
    (vector (concat date "Z")
	    (if (equal who (jabber-hipchat-jid-2-user-id my-jid))
		"out" "in")
	    "me" jid message )
    )
  )
(defun jabber-hipchat-get-history (jid buffer)
  (let ((id (jabber-hipchat-jid-2-user-id jid))
	json item links)
    (with-temp-buffer
      (jabber-hipchat-get-history-json (current-buffer) id "user"
				       jabber-hipchat-token-view-mess)
      (setq json (json-read-from-string (buffer-string))))
    (setq item (cdr (assoc 'items json)))
    (with-current-buffer buffer
      (mapcar (lambda (X) (jabber-hipchat-hist-2-jabber-log X jid)) item))
    ))


(defadvice jabber-history-query (around jabber-hipchat activate)
  (let (id jid)
    (with-temp-buffer
      (string-match ".*/\\([0-9]+_[0-9]+@.*\\)$" history-file)
      (setq jid (replace-match "\\1" t nil history-file))
      (setq ad-return-value
	    (jabber-hipchat-get-history jid (current-buffer)))
      )))

(defun jabber-hipchat-jabber-activity-show-p (JID)
  nil
  )

(defadvice jabber-connect-all (around jabber-hipchat activate)
  ;; hipchat は、 connect 時に ROOM のメッセージ履歴が送られてくる。
  ;; メッセージ履歴は activity として表示しないように jabber-activity-show-p をセットする
  (setq jabber-activity-show-p 'jabber-hipchat-jabber-activity-show-p)
  ad-do-it
  (run-at-time 60 nil
	       (lambda ()
		 (setq jabber-activity-show-p 'jabber-activity-show-p-default)
		 )))

(defun jabber-hipchat-get-history-room ()
  (interactive)
  (let ((jid (jabber-hipchat-my-jid))
	json log-list)
    (with-temp-buffer
      (jabber-hipchat-get-history-json (current-buffer)
				       (car (jabber-hipchat-get-room-id)) "room"
				       jabber-hipchat-token-view-group
				       )
      (setq json (json-read-from-string (buffer-string))))
    (setq log-list (mapcar (lambda (X) (jabber-hipchat-hist-2-jabber-log X jid))
			   (cdr (assoc 'items json))))
    (dolist (log log-list)
      (insert (format "%s\n" log)))
    ))
;;(jabber-hipchat-get-history-room)
;; jabber-chat-insert-backlog-entry


(provide 'jabber-hipchat)
