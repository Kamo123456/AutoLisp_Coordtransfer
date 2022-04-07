(defun C:coordtransfer ( / aCen cAng cCen cPl cRad cReg
	fDr it lCnt lLst mSp pCen pT1
	pT2 ptLst R tHt tLst vlaPl vlaTab
	vLst cTxt oldCol nPl clFlg actDoc
	tPt1 tPt2 cAng tiPt oSnp *error* girl xc0 yc0 pt txt html xc yc pt0)

    (vl-load-com)

  (defun findtag (bEnt tag / AttVal)
      (while (not (eq "SEQEND" (cdr (assoc 0 (entget (setq bEnt (entnext bEnt)))))))
        (if (eq tag (cdr (assoc 2 (entget bEnt))))
   (setq AttVal (cdr (assoc 1 (entget bEnt)))))) AttVal)

 (defun Extract_DXF_Values(Ent Code)
   (mapcar 'cdr
    (vl-remove-if-not
     '(lambda(a)(=(car a)Code))
 (entget Ent)))
   ); end of

   (defun *error*(msg)
     (setvar "CMDECHO" 1)
     (if oSnp(setvar "OSMODE" oSnp))
     (if mSp(vla-EndUndoMark actDoc))
   (princ)
   ); end of *error*

    ;;; s = string d = delimiter p = position delimiter (thanx Lee Mac)
  (defun SplitStr ( s d / p )
    (if s (progn
  (if (setq p (vl-string-search d s))
    (cons (substr s 1 p) (SplitStr (substr s (+ p 1 (strlen d))) d)) (list s)))))

  (defun reptag (tag newvalue ent / alist )
(if (and (= (type ent) (read "VLA-OBJECT")) newvalue)
(progn
(setq alist ( vlax-invoke ent 'GetAttributes))
 (foreach a alist
  (if (=  (vla-get-tagstring a) tag)
  (vlax-put-property a 'TextString newvalue)
  );i
 );fe
);p
(if (= 'ename (type ent)) (reptag tag newvalue (vlax-ename->vla-object ent)));i
);i
(princ));d

    ;(princ "\nВыберите девушку: ")
  (if
    ;(setq girl (ssname (ssget "_:S" (list '(0 . "INSERT") '(2 . "girl"))) 0))
    (setq girl (car (entsel "\nВыберите девушку: ")))
    (progn 
   (setq xc0 (atof (findtag girl "X_COORDINATE")))
   (setq yc0 (atof (findtag girl "Y_COORDINATE")))
   (setq pt0 (cdr (assoc 10 (entget girl))))


 (if
   (and
     (setq cPl(entsel "\nSelect LwPoliline > "))
     (= "LWPOLYLINE"(car(Extract_DXF_Values(car cPl)0)))
     ); end and
(progn
  (princ "\nPlease Wait... \n")
  (setq vlaPl(vlax-ename->vla-object(car cPl))
	ptLst(mapcar 'append
		       (setq vLst(Extract_DXF_Values(car cPl)10))
		       (mapcar 'list(Extract_DXF_Values(car cPl)42)))
	r 2 lCnt 0
	tLst '((1 0 "N\nТочки")(1 1 "X")(1 2 "Y")
	       ;(1 3 "Radius")
	       )
	actDoc(vla-get-ActiveDocument
	       (vlax-get-acad-object))
	mSp(vla-get-ModelSpace actDoc)
	tHt 625
	    ); end setq
    (vla-StartUndoMark actDoc)
    (setvar "CMDECHO" 0)
    (setq oSnp(getvar "OSMODE"))
    (foreach vert ptLst
      (setq vert(trans vert 0 1)
	    tLst(append tLst
		  (list(list r 0(strcat "Т" (itoa(1+ lCnt))))
		  (list r 1(rtos(+ yc0 (/ (- (cadr vert) (cadr pt0)) 1000.0))2 4))
		  (list r 2(rtos(+ xc0 (/ (- (car vert) (car pt0)) 1000.0))2 4))
		  ;(list r 3 "")
		       )))
      (setq r(1+ r) lCnt(1+ lCnt))
      ); end foreach
  (setq vlaTab(vla-AddTable mSp (vlax-3D-point '(0 0 0))
		(+ 1(/(length tLst)3)) 3 (* 3 tHt)(* 20 tHt)))
  ;создаем список стилей таблиц и вставляем нужный стиль, если он есть в этом списке
   (setq tablelist (dictsearch (namedobjdict) "Acad_TableStyle"))
   (setq poplist (list "Use Current"))
   (foreach item tablelist
     (if (= 3 (car item))
     (setq poplist (append poplist (list (cdr item)))))) 
  (if (member "Копия 'Ведомость головы забоя свай'" poplist)
    (vla-put-StyleName vlaTab "Копия 'Ведомость головы забоя свай'"))

  (vla-SetRowHeight vlaTab 1 2200)
  (foreach i tLst
    (vl-catch-all-apply 'vla-SetText(cons vlaTab i))  
    (vla-SetCellTextHeight vlaTab(car i)(cadr i)tHt)
    (vla-SetCellAlignment vlaTab(car i)(cadr i)acMiddleCenter)
    (if (/= (car i) 1) (vla-SetRowHeight vlaTab (car i) 1300))
    ); end foreach
  ;(vla-put-VertCellMargin vlaTab (* 0.75 tHt))
  ;(vla-put-Height vlaTab(* 1.75(/(length tLst)3)))
  ;(vla-SetRowHeight vlaTab 0 2140)
  (vla-SetColumnWidth vlaTab 0 5800)
  (vla-SetColumnWidth vlaTab 1 6430)
  (vla-SetColumnWidth vlaTab 2 6430)
  (vla-put-RepeatTopLabels vlaTab :vlax-true)
  (vla-put-BreakSpacing vlaTab (* 3 tHt))
  (vla-DeleteRows  vlaTab 0 1)
  (princ "\n<<< Place Table >>> ")
  (command "_.copybase" (trans '(0 0 0)0 1)(entlast) "")
  (command "_.erase" (entlast) "")
  (command "_.pasteclip" pause)
  (setvar "OSMODE" oSnp)
  (setvar "CMDECHO" 1)
  (vla-EndUndoMark actDoc)
  ); end progn
    (princ "\n<!> Это не полилиния! Quit. <!> ")
   ); end if
   (gc)
   (princ "\nЖми ESC !!!: ")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
) (progn  


;;;    (defun c:test (/ a b c d i)
;;; (if (and (setq a (ssget '((0 . "*TABLE"))))
;;;   (setq b (getdist "\nSpecify Row Height : "))
;;;     )
;;;   (repeat (setq i (sslength a))
;;;     (setq c (vlax-ename->vla-object (ssname a (setq i (1- i)))))
;;;     (setq d 1)
;;;     (while (< 0 d (vla-get-rows c))
;;;(vla-setrowheight c d b)
;;;(setq d (1+ d))
;;;     )
;;;   )
;;; )
;;; (princ)
;;;)

  

  (if (setq pt
	(getpoint "\nУкажи точку снятия координат в буфер обмена, жми ESC и переходи на другой чертеж: "))
    (progn
  (setq txt (strcat (rtos (car pt) 2 5) " " (rtos (cadr pt) 2 5)))
  (vlax-invoke (vlax-get (vlax-get (setq html (vlax-create-object "htmlfile")) 'ParentWindow)
			 'ClipBoardData) 'setData "Text" txt)))



  (princ "\nЕсли координаты уже в буфере и перешел на другой чертеж, то выбери девушку, иначе жми ESC: ")
  (if (setq girl (ssname (ssget "_:S" (list '(0 . "INSERT") '(2 . "girl"))) 0)) (progn
  (setq txt (vlax-Invoke (vlax-Get (vlax-Get (setq htmlFile (vlax-Create-Object "htmlFile"))
		'ParentWindow) 'ClipBoardData) 'GetData "Text"))
  (setq xc (car (SplitStr txt " ")))
  (setq yc (cadr (SplitStr txt " ")))										  
  (reptag "X_COORDINATE" xc girl) (reptag "Y_COORDINATE" yc girl)))
  ))

  );end coordtransfer