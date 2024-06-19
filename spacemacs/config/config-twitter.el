;; This file configures twitter for use.

;;;###autoload
(defun my-twitter-setup ()
  (setq twittering-use-master-password t
        twittering-connection-type-order '(wget curl urllib-http native urllib-https)
        twittering-tinyurl-service 'bit.ly
        twittering-bitly-login "login"
        twittering-bitly-api-key "api-key"))

(provide 'config-twitter)
