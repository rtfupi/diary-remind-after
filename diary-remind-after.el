;; diary-remind-after.el -- Implement the reminder period after the event.

;; Copyright (C) 2003,2010,2019,2021 Eugene V. Markov

;; Author: Eugene V. Markov
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; Для применения "напоминательных" периодов можно использовать
;; например:
;; %%(diary-remind '(diary-anniversary 1 1 2018) '(29 30 31))
;; но у этого способа нет "напоминаьного" периода после события
;; и, поскольку, ф-ия `diary-remind' рекурсивная, то при
;; использовании больших периодов наблюдаются большие тормоза в
;; работе с календарем.
;;
;; Разработано две ф-ии:
;; em-diary-anniversary - для годовщин с "напоминательным" периодом
;; до и после события.
;; em-diary-date - для конкретной даты с "напоминательным" периодом
;; до и после события.


;; (customize-option 'diary-file) -> ~/.emacs.d/.diary
;;
;; (customize-option 'european-calendar-style) -> t
;;
;; (customize-option 'diary-remind-message)
;; (" # " (if (eq place 'before) "->" "<-") " (Rem.: " (em-diary-print ddate) ")" diary-entry)
;; (" # " (if (eq place (quote before)) "->" "<=") " (Rem.: " (em-diary-print ddate) ")" diary-entry)
;; (setq diary-remind-message
;;       '((cond
;;          ((eq place 'before)
;;           (concat " -> (" (em-diary-print ddate) ")" diary-entry))
;;          ((eq place 'after)
;;           (concat " <= (" (em-diary-print ddate) ")" diary-entry))
;;          (t
;;           (concat " !!             " diary-entry)))))



(defun em-diary-print (ddate)
  "Print date from American style according to `calendar-date-style'."
  (let ((dd (calendar-extract-day ddate))
        (mm (calendar-extract-month ddate))
        (yy (calendar-extract-year ddate)))
    (unless yy (setq yy 0))
    (cond
     ((eq calendar-date-style 'iso)  ; YMD
      (format "%.4d-%.2d-%.2d" yy mm dd))
     ((eq calendar-date-style 'european) ; DMY
      (format "%.2d-%.2d-%.4d" dd mm yy))
     (t
      (format "%.2d/%.2d/%.4d" dd mm yy)))))



(defun em-diary-test-bound (d m y before after exact)
  "Check for a range. D - the day of anniversary. M  - the
mouth of anniversary. Y - the year of anniversary. BEFORE -
the number of days before anniversary. AFTER - the number
of days after anniversary. EXACT - exact date.

Returns a list of the form '(N POSITION RESULT ANNIVERSARY BEFORE AFTER).
N :  0 - anniversary this year;
     1 - anniversary next year;
    -1 - anniversary last year.
POSITION : before - before the anniversary;
           after  - after the anniversary;
           nil    - doesn't matter;
RESULT : t - in the bound;
ANNIVERSARY : absolute date of the current anniversary;
BEFORE      : the absolute date of the bottom of the current anniversary;
AFTER       : the absolute date of the upper bound of the current
              anniversary.
"
  ;;EXACT=t   => текущая дата должна лежать строго в [(d m y)-before, (d m y)+after]
  ;;EXACT=nil => текущая дата должна быть >= (d m y)-before и лежать в любом из
  ;;             интервалов [(d m t)-before, (d m t)+after]

  (let* ((yc (calendar-extract-year date)) ; текущий год
         (s (calendar-absolute-from-gregorian date)) ; текущая дата
         (bfirst (- (calendar-absolute-from-gregorian (list m d y)) before)) ; первая нижняя граница
         (afirst (+ (calendar-absolute-from-gregorian (list m d y)) after))  ; первая верхняя граница
         (A (calendar-absolute-from-gregorian (list m d yc))) ; юбилей в текущем году
         (b (- A before)) ; нижняя граница в этом году
         (a (+ A after)) ; верхняя граница в этом году
         Aa v fl)


      (when (and (>= s bfirst)
                 (or (not exact) (<= s afirst)))

        (setq v (calendar-gregorian-from-absolute b))
        (when (/= (calendar-extract-year v) yc)
          ;; год нижней границы не равен текущему.

          ;;   new year                        new year
          ;; --+---|---+---+-------------------+---|---+---+--
          ;;   b       A   a                   b       A   a
          ;; зациклим
          (setf (nth 2 v) yc)
          (setq b (calendar-absolute-from-gregorian v))
          (setq fl t))

        (unless fl
          (setq v (calendar-gregorian-from-absolute a))
          (when (/= (calendar-extract-year v) yc)
            ;; год верхней границы не равен текущему.

            ;;         new year                          new year
            ;; --+----+---|---+--------------------+----+---|---+-
            ;;   b    A       a                    b   A       a
            ;; зациклим
            (setf (nth 2 v) yc)
            (setq a (calendar-absolute-from-gregorian v))
            (setq fl t)))

        (if fl
            (cond
             ((and (>= s b) (<= A a))
              ;; --+---|---+---+-------------------+-.-|---+---+--
              ;;   b       A   a                   b s     A   a
              (setq Aa (calendar-absolute-from-gregorian (list m d (1+ yc))))
              (list 1 'before t Aa (- Aa before) (+ Aa after)))

             ((and (<= s a) (<= A a))
              (if (< s A)
                  ;; --+---|-.-+---+-------------------+---|---+---+--
                  ;;   b     s A   a                   b       A   a
                  (list 0 'before t A (- A before) (+ A after))
                ;; --+--|---+-.-+-------------------+---|---+---+--
                ;;   b      A s a                   b       A   a
                (list 0 'after t A (- A before) (+ A after))))

             ((and (<= s a) (>= A b))
              ;; --+---+---|-.-+--------------------+---+---|---+-
              ;;   b   A     s a                    b   A       a
              (setq Aa (calendar-absolute-from-gregorian (list m d (1- yc))))
              (list -1 'after t Aa (- Aa before) (+ Aa after)))

             ((and (>= s b) (>= A b))
              (if (< s A)
                  ;; --+---+---|---+--------------------+-.-+---|---+-
                  ;;   b   A       a                    b s A       a
                  (list 0 'before t A (- A before) (+ A after))
                ;; --+---+---|---+---------------------+---+-.-|---+-
                ;;   b   A       a                     b   A s     a
                (list 0 'after t A (- A before) (+ A after))))
             (t
              (list 0 nil nil)))

          ;; new year                      new year
          ;; ---|--------+---+---+------------|-
          ;;             b   A   a
          (if (and (>= s b) (<= s a))
              (if (and (>= s b) (< s A ))
                  (list 0 'before t A b a)
                (list 0 'after t A b a))
            (list 0 nil nil))
          ))))



(defun em-diary-anniversary (month day &optional year before after off mark mark-reminder)
  "Anniversary diary entry.
DAY - the day of anniversary.
MONTH - the mouth of anniversary.
YEAR - the year of anniversary (nil - no (not known) date of birth).

BEFORE - warn for the number of days before the anniversary.
AFTER - warn for the number of days after the anniversary.

OFF - do not display if the date falls in the current period.
      This is a list of '(MONTH DAY YEAR).

An optional parameter MARK and MARK-REMINDER specifies a face or
single-character string to use when highlighting the day in the
calendar.

The order of the input parameters changes according to
`calendar-date-style' (e.g. to DAY MONTH YEAR in the European style)."
;;!!! date - external variable !!!

  (unless before (setq before 0))
  (unless after (setq after 0))

  (let* ((y (calendar-extract-year date))
         (ddate (diary-make-date month day year))
         (dd (calendar-extract-day ddate))
         (mm (calendar-extract-month ddate))
         (yy (calendar-extract-year ddate))
         (diff (if yy (- y yy) 100))
         (yy (if yy yy (1- y)))  ; это для того, что если год указан, то юбилей
                                 ; рассматривается только со следующего от указанного года,
                                 ; а если год не указан, то рассматривается уже в этом году.
         r place)

    ;; високосный год
    (and (= mm 2) (= dd 29) (not (calendar-leap-year-p y))
         (setq mm 3 dd 1))

    ;; временно не отображать (подготовка)
    (when off
      (setq off (calendar-absolute-from-gregorian
                 (diary-make-date (calendar-extract-month off)
                                  (calendar-extract-day off)
                                  (calendar-extract-year off)))))


    (setq r (em-diary-test-bound dd mm (1+ yy) before after nil))

    ;; (message ">>>>>> off: %S" off)
    ;; (message ">>>>>> r: %S" r)
    ;; (message ">>>>>> date: %S" date)

    (when (and (nth 2 r)
               (not (and off
                         (>= off (nth 4 r))
                         (<= off (nth 5 r)))))

      (if (and (calendar-date-equal (list mm dd y) date)
               (> diff 0))
          (let ((diary-entry (format entry diff (diary-ordinal-suffix diff))))
            (cons mark (mapconcat 'eval diary-remind-message "")))
        (when (and (or (/= before 0) (/= after 0))
                   (or (not diary-marking-entries-flag) mark-reminder))
          (setq diff (+ diff (nth 0 r)))
          (setq place (nth 1 r))
          (when (and (nth 2 r) (> diff 0))
            (let ((diary-entry (format entry diff (diary-ordinal-suffix diff))))
              (cons mark-reminder (mapconcat 'eval diary-remind-message ""))))
          )))))
;;(calendar-absolute-from-gregorian '(2 14 2019))



(defun em-diary-correct-last-day (day month year)
  ""
  (when (or
         (and (= month 2)
            (or (and (calendar-leap-year-p year) (> day 29))
                (> day 28)))
         (= day 31))
    (calendar-last-day-of-month mm)))



(defun em-diary-month-test-bound (day month year before after off)
  ""
  ;; Simple case
  ;;
  ;;           year                                              year+1
  ;; Dec        |        Jan       Feb        Nov       Dec        |        Jan
  ;; -o-[=====]-|-[=====]-o-[=====]-o-[=~..~=]-o-[=====]-o-[=====]-|-[=====]-o-
  ;;    b c   a   b c   a   b c   a              b c   a   b c   a   b c   a
  ;;

  ;; Complicated cases
  ;;
  ;;           year                                              year+1
  ;; Dec        |        Jan       Feb        Nov       Dec        |        Jan
  ;; =o==]---[==|==]---[==o==]---[==o==]~..~[==o==]---[==o==]---[==|==]---[==o=
  ;; c   a   b c   a   b c   a   b c        b c   a   b c   a   b c   a   b c
  ;;         \_________________________/
  ;;            0         1         2
  ;;           last     curent     next

  ;;
  ;;           year                                              year+1
  ;; Dec        |        Jan       Feb        Nov       Dec        |        Jan
  ;; -o==]---[==|==]---[==o==]---[==o==]~..~[==o==]---[==o==]---[==|==]---[==o=
  ;;   c a   b   c a   b   c a   b   c a    b   c a   b   c a   b   c a   b   c
  ;;                                        \_________________________/
  ;;                                           0         1         2
  ;;                                          last     curent    next

  ;; c - check date;
  ;; b - before bound;
  ;; a - after bound.

  (let (m ; month list 
        d ; day list
        y ; year list
        da; absolute date list
        b ; before bound list
        a ; after bound list
        ca; absolute curent date
        out l)

    ;;
    (setq m (list (if (<= (- month 1) 0) 12 (- month 1))
                  month
                  (1+ (mod month 12))))

    (setq d (list (em-diary-correct-last-day day (nth 0 m))
                  (em-diary-correct-last-day day (nth 1 m))
                  (em-diary-correct-last-day day (nth 2 m))))

    (setq y (list (if (< (nth 1 m) (nth 0 m)) (1- year))
                  year
                  (if (> (nth 1 m) (nth 2 m)) (1+ year))))

    (setq da)
    (setq l '( 0 1 2))
    (while l
      (setq da (nconc da (list (calendar-absolute-from-gregorian
                                (list (nth (car l) m) (nth (car l) d) (nth (car l) y))))))
      (setq l (cdr l)))

    (setq b)
    (setq l '( 0 1 2))
    (while l
      (setq b (nconc b (list (- (nth (car l) da) before))))
      (setq l (cdr l)))


    (setq a)
    (setq l '( 0 1 2))
    (while l
      (setq a (nconc a (list (+ (nth (car l) da) after))))
      (setq l (cdr l)))

    (setq ca (calendar-absolute-from-gregorian date))
    (setq off (calendar-absolute-from-gregorian off))

    (setq out)
    (setq l '( 1 0 2))
    (while l
      (when
          (and (>= ca (nth (car l) b)) (<= ca (nth (car l) a))
               (or (not off)                                             ; not set
                   (or (< off (nth (car l) b)) (> off (nth (car l) a)))  ; outbound
                   (and (>= off (nth (car l) b)) (<= off (nth (car l) a)); inbound and <
                        (< ca off))))
        (setq out (list t ca (nth (car l) b) (nth (car l) a)))
        (setq l))
      (setq l (cdr l)))

    out))



;; (defun em-diary-date (month day year &optional before after off mark mark-reminder)
;;   "Specific date diary entry.
;; Entry applies if date is MONTH, DAY, YEAR. The order
;; of the input parameters changes according to `calendar-date-style'
;; \(e.g. to DAY MONTH YEAR in the European style).

;; BEFORE - warn for the number of days before specific date.
;; AFTER - warn for the number of days after specific date.

;; If month day year is represented by a list or t, then BEFORE AFTER
;; are set to nil.

;; An optional parameter MARK and MARK-REMINDER specifies a face or
;; single-character string to use when highlighting the day in the
;; calendar."

;;   (let* (
;;          (d (calendar-extract-day date))          ; текущий день
;;          (m (calendar-extract-month date))        ; текущий месц
;;          (y (calendar-extract-year date))         ; текущий год
;;          (ddate (diary-make-date month day year)) ; приведем в соответствие с `calendar-date-style'
;;          dd                                       ; день события
;;          mm                                       ; меяц события
;;          yy                                       ; год события
;;          place r)

;;     (setq day (calendar-extract-day ddate))
;;     (setq month (calendar-extract-month ddate))
;;     (setq year (calendar-extract-year ddate))

;;     (setq dd (if (or (and (listp day) (memq d day)) (eq day t)) d day))
;;     (setq mm (if (or (and (listp month) (memq m month)) (eq month t)) m month))
;;     (setq yy (if (or (and (listp year) (memq y year)) (eq year t)) y year))


;;     (unless (or (listp dd) (listp mm) (listp yy))

;;       ;; диапазоны работают только если день указан явно.
;;       (and (or (listp day) (eq day t)) (setq before 0 after 0))

;;       ;; временно не отображать (подготовка)
;;       (when off
;;         (setq off (diary-make-date (calendar-extract-month off)
;;                                    (calendar-extract-day off)
;;                                    (calendar-extract-year off))))

;;       (if (eq mouth t) ; периодичность в месяц
;;           (setq r (em-diary-month-test-bound dd mm yy before after off))

;;       (when (and (nth 2 r)
;;                  (not (and off
;;                            (>= off (nth 4 r))
;;                            (<= off (nth 5 r)))))

;;         (if (calendar-date-equal (list mm dd yy) date)
;;             (let ((diary-entry entry))
;;               (cons mark (mapconcat 'eval diary-remind-message "")))

;;           (when (and (or before after)
;;                      (or (not diary-marking-entries-flag) mark-reminder))
;;             (unless before (setq before 0))
;;             (unless after (setq after 0))
;;             (setq r (em-diary-test-bound dd mm yy before after t))
;;             (setq place (nth 1 r))
;;             ;; (message ">>>>>> date: %S" date)
;;             ;; (message ">>>>>> r: %S" r)
;;             ;; (message ">>>>>> entry: %S" entry)
;;             ;; (message ">>>>>> place: %S" place)
;;             ;; (message ">>>>>>")
;;             (when (nth 2 r)
;;               (let ((diary-entry entry))
;;                 (cons mark-reminder (mapconcat 'eval diary-remind-message ""))))
;;             )))))



(defun em-diary-date (month day year &optional before after mark mark-reminder)
  "Specific date diary entry.
Entry applies if date is MONTH, DAY, YEAR. The order
of the input parameters changes according to `calendar-date-style'
\(e.g. to DAY MONTH YEAR in the European style).

BEFORE - warn for the number of days before specific date.
AFTER - warn for the number of days after specific date.

If month day year is represented by a list or t, then BEFORE AFTER
are set to nil.

An optional parameter MARK and MARK-REMINDER specifies a face or
single-character string to use when highlighting the day in the
calendar."

  (let* (
         (d (calendar-extract-day date))
         (m (calendar-extract-month date))
         (y (calendar-extract-year date))
         (ddate (diary-make-date month day year)) ; приведем в соответствие с `calendar-date-style'
         (dd (calendar-extract-day ddate))
         (mm (calendar-extract-month ddate))
         (yy (calendar-extract-year ddate))
         (dd (if (or (and (listp dd) (memq d dd)) (eq dd t)) d dd))
         (mm (if (or (and (listp mm) (memq m mm)) (eq mm t)) m mm))
         (yy (if (or (and (listp yy) (memq y yy)) (eq yy t)) y yy))
         place)

    (unless (or (listp dd) (listp mm) (listp yy))

      ;; диапазоны работают только если дата указана явно.
      (and (or (listp day) (eq day t)
               (listp month) (eq month t)
               (listp year) (eq year t))
               (setq before nil after nil))

      (if (calendar-date-equal (list mm dd yy) date)
          (let ((diary-entry entry))
            (cons mark (mapconcat 'eval diary-remind-message "")))
        (when (and (or before after)
                   (or (not diary-marking-entries-flag) mark-reminder))
          (unless before (setq before 0))
          (unless after (setq after 0))
          (setq r (em-diary-test-bound dd mm yy before after t))
          (setq place (nth 1 r))
          ;; (message ">>>>>> date: %S" date)
          ;; (message ">>>>>> r: %S" r)
          ;; (message ">>>>>> entry: %S" entry)
          ;; (message ">>>>>> place: %S" place)
          ;; (message ">>>>>>")
          (when (nth 2 r)
            (let ((diary-entry entry))
              (cons mark-reminder (mapconcat 'eval diary-remind-message ""))))
          )))))


(provide 'diary-remind-after)
