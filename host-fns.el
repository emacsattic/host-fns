;;; host-fns.el --- functions for querying host name information

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Public domain

;; $Id: host-fns.el,v 1.5 2015/10/20 13:52:14 friedman Exp $

;;; Commentary:
;;; Code:

(require 'file-fns)
(require 'string-fns)
(require 'cl)

(defun host-name ()
  "Returns the name of the current host minus the domain."
  (let ((hostname (downcase (system-name))))
    (save-match-data
      (substring hostname (string-match "^[^.]+" hostname) (match-end 0)))))

(defun dns-domain-name ()
  "Get the domain name of the current host and return it as a string.

First, try to get the domain name from the function `system-name', which
will succeed if `system-name' returns the fully-qualified host name.  If
that fails, several external programs are tried.  If all these attempts
fail to determine the domain name the string \"unknown\" is returned.

Note that if  domain-name  has to call the external program
`domainname' the return value could be incorrect.  `domainname' is actually
supposed to return the NIS domain rather than the domain of the host, but
many system administrators configure systems incorrectly."
  (let ((domain (system-name)))
    (save-match-data
      (cond ((string-match "\\." domain)
             (substring domain (1+ (match-beginning 0))))
            (t
             (setq domain nil)
             (let ((progdata '(("hostname"      . "\\.\\(.*\\)")
                               ("dnsdomainname" . "\\(.*[\\.]+.*\\)")
                               ("domainname"    . "\\(.*[\\.]+.*\\)")))
                   prog regexp output)
               (while (and progdata (null domain))
                 (setq prog     (car (car progdata)))
                 (setq regexp   (cdr (car progdata)))
                 (setq progdata (cdr progdata))
                 (cond ((file-in-pathlist-p prog exec-path)
                        (setq output (with-output-to-string
                                       (call-process prog nil
                                                     standard-output nil)))
                        (and (string-match regexp output)
                             (setq domain (matching-substring 1 output)))))))
             (or domain "unknown"))))))

(defun nis-domain-name ()
  "Get the NIS domain name of the current host and return it as a string."
  (substring (with-command-output-to-string "domainname") 0 -1))

(defun abbreviate-hostnick (hostname)
  "Return a shorter string for hostname.
Any hyphenated host names get converted to using the first char of each
hyphenated part, e.g. \"apple-gunkies\" => \"ag\"."
  (and (string-match "\\." hostname)
       (setq hostname (substring hostname 0 (match-beginning 0))))
  (let* ((hostnick hostname))
    (save-match-data
      (cond ((string-match "-" hostname)
             (let ((pos (if (string-match "^-+" hostname) (match-end 0) 0))
                   (len (length hostname))
                   (abbrev ""))
               (while (< pos len)
                 (setq abbrev
                       (concat abbrev (char-to-string (aref hostname pos))))
                 (if (string-match "-+" hostname pos)
                     (setq pos (match-end 0))
                   (setq pos len)))
               (setq hostnick abbrev)))))
    hostnick))

(defun host-name-to-addr (host &optional family)
  "Return the IP address corresponding to host name HOST.
Optional argument FAMILY specifies the address family to lookup, and defaults to `ipv4'.
Probably the only other useful address family is `ipv6'.

The address returned is a vector of 4 octets or 8 double-octet words, or
nil if the host name cannot be resolved.

Note: this function can only return the first address associated with a host
even if it has multiple addresses."
  (condition-case () ;; Ignore lookup failures
      (let* ((proc (make-network-process :name    "host-name-to-addr"
                                         :family  (or family 'ipv4)
                                         :host    host
                                         :service 9 ;; discard
                                         :type    'datagram
                                         :noquery t))
             (addr (plist-get (process-contact proc t) :remote)))
        (delete-process proc)
        (subseq addr 0 -1))  ;; strip port
    (error nil)))

(provide 'host-fns)

;;; host-fns.el ends here.
