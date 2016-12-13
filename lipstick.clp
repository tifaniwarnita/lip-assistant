;;; -*- clips -*-
;;;======================================================
;;;   Lip Assitant
;;;
;;;     Personal assistant for choosing the best
;;;		lipstick for you :3
;;;		(batch "C:/Users/Tifani/Documents/GitHub/lip-assistant/lipstick.clp")
;;;
;;;======================================================
;;;


;;;*************
;;;* TEMPLATES *
;;;*************

(deftemplate number
	(slot value))

(deftemplate
	form (slot name)
	(slot description))

(deftemplate question
	(slot text)
	(slot type)
	(slot ident))

(deftemplate answer
	(slot ident)
	(slot text))

(deftemplate user
	(slot income (default 0))
	(slot dependents (default 0)))

(deftemplate recommendation
	(slot name)
	(slot explanation))
                    
(deftemplate lipstick 
   (slot shape (default 0))
   (slot texture (default 0))
   (slot price (default 0))
   (slot skintone (default 0)))


;;;*************
;;;* FUNCTIONS *
;;;*************

(deffunction is-of-type (?answer ?type)
	"Check that the answer has the right form"
	(if (eq ?type yes-no) then
		(return (or (eq ?answer yes) (eq ?answer no)))
	else (if (eq ?type number) then
		(return (numberp ?answer))
	else (if (eq ?type shape-answer) then
		(return (or (eq ?answer liquid) (eq ?answer stick)))
	else (if (eq ?type texture-liquid-answer) then
		(return (or (eq ?answer matte) (eq ?answer stain) (eq ?answer gloss)))
	else (if (eq ?type texture-stick-answer) then
		(return (or (eq ?answer matte) (eq ?answer sheer) (eq ?answer satin)))
	else (if (eq ?type price-answer) then
		(return (or (eq ?answer a) (eq ?answer b) (eq ?answer c)))
	else (if (eq ?type skintone-answer) then
		(return (or (eq ?answer light) (eq ?answer medium) (eq ?answer dark)))
 	else (return (> (str-length ?answer) 0))))))))))

(deffunction ask-user (?question ?type)
	"Ask a question, and return the answer"
	(bind ?answer "")
	(while (not (is-of-type ?answer ?type)) do
		(printout t ?question " ")
		(if (eq ?type yes-no) then
			(printout t "(yes or no) ")
		else (if (eq ?type shape-answer) then
			(printout t "(liquid/stick) ")
		else (if (eq ?type texture-liquid-answer) then
			(printout t "(matte/stain/gloss) ")
		else (if (eq ?type texture-stick-answer) then
			(printout t "(matte/sheer/satin) ")
		else (if (eq ?type price-answer) then
			(printout t "(a/b/c) " crlf)
			(printout t "a. <Rp100.000" crlf)
			(printout t "b. Rp100.000 - Rp200.000" crlf)
			(printout t "c. >Rp200.000" crlf)
		else (if (eq ?type skintone-answer) then
			(printout t "(light/medium/dark) ")))))))
		(bind ?answer (read)))
	(return ?answer))


;;;*****************
;;;*     FACTS     *
;;;*****************

(deffacts question-data
	"The questions the system can ask."
	(question
		(ident shape)
		(type shape-answer)
		(text "Do you prefer liquid lipstick or stick lipstick?"))
	(question
		(ident texture-liquid)
		(type texture-liquid-answer)
		(text "Which texture do you prefer?"))
	(question
		(ident texture-stick)
		(type texture-stick-answer)
		(text "Which texture do you prefer?"))
	(question
		(ident price)
		(type price-answer)
		(text "How many budget do you have?"))
	(question
		(ident skintone)
		(type skintone-answer)
		(text "What is your skin complexion?")))


;;;**********************
;;;* LIPSTICK RULE: ASK *
;;;**********************

(defmodule ask)

(defrule ask::ask-question-by-id
	"Ask a question and assert the answer"
	(declare (auto-focus TRUE))
	;; If there is a question with ident ?id...
	(MAIN::question (ident ?id) (text ?text) (type ?type))
	;; ... and there is no answer for it
	(not (MAIN::answer (ident ?id)))
	;; ... and the trigger fact for this question exists
	?ask <- (MAIN::ask ?id)
	=>
	;; Ask the question
	(bind ?answer (ask-user ?text ?type))
	;; Assert the answer as a fact
	(assert (MAIN::answer (ident ?id) (text ?answer)))
	;; Remove the trigger
	(retract ?ask)
	;; And finally, exit this module
	(return))


;;;***************************
;;;* LIPSTICK RULE: START UP *
;;;***************************

(defmodule startup)

(defrule print-banner
	=>
	(printout t "Type your name and press Enter> ")
	(bind ?name (read))
	(printout t crlf "*****************************" crlf)
	(printout t "Hello, " ?name "." crlf)
	(printout t "I am Lipzy. Nice to meet you!" crlf)
	(printout t "Please answer the questions and" crlf)
	(printout t "I will tell you what kind of" crlf)
	(printout t "lipstick you may need." crlf)
	(printout t "*****************************" crlf crlf))


;;;****************************
;;;* LIPSTICK RULE: INTERVIEW *
;;;****************************

(defmodule interview)

(defrule request-shape
	=>
	(assert (ask shape)))

(defrule request-texture-liquid
	(answer (ident shape) (text ?d&:(eq ?d liquid)))
	=>
	(assert (MAIN::ask texture-liquid)))

(defrule request-texture-stick
	(answer (ident shape) (text ?d&:(eq ?d stick)))
	=>
	(assert (MAIN::ask texture-stick)))

(defrule request-price
	=>
	(assert (ask price)))

(defrule request-skintone
	=>
	(assert (ask skintone)))

(defrule assert-interview-fact-1
	(answer (ident shape) (text ?sh&:(eq ?sh liquid)))
	(answer (ident texture-liquid) (text ?tx))
	(answer (ident price) (text ?pr))
	(answer (ident skintone) (text ?sk))
	=>
	(assert (lipstick
		(shape ?sh)
		(texture ?tx)
		(price ?pr)
		(skintone ?sk))))

(defrule assert-interview-fact-2
	(answer (ident shape) (text ?sh&:(eq ?sh stick)))
	(answer (ident texture-stick) (text ?tx))
	(answer (ident price) (text ?pr))
	(answer (ident skintone) (text ?sk))
	=>
	(assert (lipstick
		(shape ?sh)
		(texture ?tx)
		(price ?pr)
		(skintone ?sk))))

;;;*********************************
;;;* LIPSTICK RULE: RECOMMENDATION *
;;;*********************************

(defmodule recommend)

(defrule lipstick-liquid-matte-a-light
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "NYX Soft Matte Lip Color Stockholm")
		(explanation "NYX Soft Matte Lip Color Stockholm"))))

(defrule lipstick-liquid-matte-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "NYX Soft Matte Lip Color Cannes")
		(explanation "NYX Soft Matte Lip Color Cannes"))))

(defrule lipstick-liquid-matte-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "NYX Soft Matte Lip Color Rome")
		(explanation "NYX Soft Matte Lip Color Rome"))))

;;;

(defrule lipstick-liquid-matte-b-light
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "The Balm Meet Matte Hughes Commited")
		(explanation "The Balm Meet Matte Hughes Commited"))))

(defrule lipstick-liquid-matte-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "The Balm Meet Matte Hughes Charming")
		(explanation "The Balm Meet Matte Hughes Charming"))))

(defrule lipstick-liquid-matte-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "The Balm Meet Matte Hughes Trustworthy")
		(explanation "The Balm Meet Matte Hughes Trustworthy"))))

;;;

(defrule lipstick-liquid-matte-c-light
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "Girlactik Matte Lip Paint Blushing")
		(explanation "Girlactik Matte Lip Paint Blushing"))))

(defrule lipstick-liquid-matte-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Girlactik Matte Lip Paint Allure")
		(explanation "Girlactik Matte Lip Paint Allure"))))

(defrule lipstick-liquid-matte-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Girlactik Matte Lip Paint Divine")
		(explanation "Girlactik Matte Lip Paint Divine"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule lipstick-liquid-stain-a-light
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "Etude House Fresh Cherry Tint Orange")
		(explanation "Etude House Fresh Cherry Tint Orange"))))

(defrule lipstick-liquid-stain-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Etude House Fresh Cherry Tint Pink")
		(explanation "Etude House Fresh Cherry Tint Pink"))))

(defrule lipstick-liquid-stain-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Etude House Fresh Cherry Tint Red")
		(explanation "Etude House Fresh Cherry Tint Red"))))

;;;

(defrule lipstick-liquid-stain-b-light
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "Body Shop Lip & Cheek Stain Dusty Rose")
		(explanation "Body Shop Lip & Cheek Stain Dusty Rose"))))

(defrule lipstick-liquid-stain-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Body Shop Lip & Cheek Stain Dark Cherry")
		(explanation "Body Shop Lip & Cheek Stain Dark Cherry"))))

(defrule lipstick-liquid-stain-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Body Shop Lip & Cheek Stain Deep Berry")
		(explanation "Body Shop Lip & Cheek Stain Deep Berry"))))

;;;

(defrule lipstick-liquid-stain-c-light
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "Benefit Chachatint Cheek & Lip Stain")
		(explanation "Benefit Chachatint Cheek & Lip Stain"))))

(defrule lipstick-liquid-stain-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Benefit Posietint Cheek & Lip Stain")
		(explanation "Benefit Posietint Cheek & Lip Stain"))))

(defrule lipstick-liquid-stain-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Benefit Benetint Cheek & Lip Stain")
		(explanation "Benefit Benetint Cheek & Lip Stain"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule lipstick-liquid-gloss-a-light
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "NYX Butter Gloss Tiramisu")
		(explanation "NYX Butter Gloss Tiramisu"))))

(defrule lipstick-liquid-gloss-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "NYX Butter Gloss Praline")
		(explanation "NYX Butter Gloss Praline"))))

(defrule lipstick-liquid-gloss-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "NYX Butter Gloss Ginger Snap")
		(explanation "NYX Butter Gloss Ginger Snap"))))

;;;

(defrule lipstick-liquid-gloss-b-light
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "Sephora Ultra Shine Lip Gel Earth & Fire")
		(explanation "Sephora Ultra Shine Lip Gel Earth & Fire"))))

(defrule lipstick-liquid-gloss-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Sephora Ultra Shine Lip Gel Deep Rose")
		(explanation "Sephora Ultra Shine Lip Gel Deep Rose"))))

(defrule lipstick-liquid-gloss-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Sephora Ultra Shine Lip Gel Lady Duck")
		(explanation "Sephora Ultra Shine Lip Gel Lady Duck"))))

;;;

(defrule lipstick-liquid-gloss-c-light
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "Anastasia Beverly Hills Lipgloss Sunset Trip")
		(explanation "Anastasia Beverly Hills Lipgloss Metallic Rose"))))

(defrule lipstick-liquid-gloss-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Anastasia Beverly Hills Lipgloss Metallic Rose")
		(explanation "Anastasia Beverly Hills Lipgloss Metallic Rose"))))

(defrule lipstick-liquid-gloss-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Anastasia Beverly Hills Lipgloss Sunset Tara")
		(explanation "Anastasia Beverly Hills Lipgloss Tara"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule lipstick-stick-matte-a-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "e.l.f Matte Lip Colour Natural")
		(explanation "e.l.f Matte Lip Colour Natural"))))

(defrule lipstick-stick-matte-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "e.l.f Matte Lip Colour Praline")
		(explanation "e.l.f Matte Lip Colour Praline"))))

(defrule lipstick-stick-matte-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "e.l.f Matte Lip Colour Cranberry")
		(explanation "e.l.f Matte Lip Colour Cranberry"))))

;;;

(defrule lipstick-stick-matte-b-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "Colourpop Lippie Stix Hype Girl")
		(explanation "Colourpop Lippie Stix Hype Girl"))))

(defrule lipstick-stick-matte-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Colourpop Lippie Stix Brink")
		(explanation "Colourpop Lippie Stix Brink"))))

(defrule lipstick-stick-matte-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Colourpop Lippie Stix Hype Daydream")
		(explanation "Colourpop Lippie Stix Daydream"))))

;;;

(defrule lipstick-stick-matte-c-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

(defrule lipstick-stick-matte-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

(defrule lipstick-stick-matte-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule lipstick-stick-satin-a-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "Revlon Super Lustrous Creme Lipstick Ginger Rose")
		(explanation "Revlon Super Lustrous Creme Lipstick Ginger Rose"))))

(defrule lipstick-stick-satin-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Revlon Super Lustrous Creme Lipstick Rose Velvet")
		(explanation "Revlon Super Lustrous Creme Lipstick Rose Velvet"))))

(defrule lipstick-stick-satin-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Revlon Super Lustrous Creme Lipstick Rum Raisin")
		(explanation "Revlon Super Lustrous Creme Lipstick Rum Raisin"))))

;;;

(defrule lipstick-stick-satin-b-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "L'Oreal Paris Color Riche Lipstick Beige A Nu")
		(explanation "L'Oreal Paris Color Riche Lipstick Beige A Nu"))))

(defrule lipstick-stick-satin-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "L'Oreal Paris Color Riche Lipstick Blushing Berry")
		(explanation "L'Oreal Paris Color Riche Lipstick Blushing Berry"))))

(defrule lipstick-stick-satin-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "L'Oreal Paris Color Riche Lipstick Spiced Cider")
		(explanation "L'Oreal Paris Color Riche Lipstick Spiced Cider"))))

;;;

(defrule lipstick-stick-satin-c-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "MAC Satin Lipstick Faux")
		(explanation "MAC Satin Lipstick Faux"))))

(defrule lipstick-stick-satin-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "MAC Satin Lipstick Twig")
		(explanation "MAC Satin Lipstick Twig"))))

(defrule lipstick-stick-satin-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "MAC Satin Lipstick Del Rio")
		(explanation "MAC Satin Lipstick Del Rio"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule lipstick-stick-sheer-a-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

(defrule lipstick-stick-sheer-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

(defrule lipstick-stick-sheer-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

;;;

(defrule lipstick-stick-sheer-b-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

(defrule lipstick-stick-sheer-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

(defrule lipstick-stick-sheer-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

;;;

(defrule lipstick-stick-sheer-c-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

(defrule lipstick-stick-sheer-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

(defrule lipstick-stick-sheer-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name lipstick-a)
		(explanation "Cocok kerja di air"))))

;;;*************************
;;;* LIPSTICK RULE: REPORT *
;;;*************************

(defmodule report)

(defrule sort-and-print
	?r1 <- (recommendation
		(name ?f1)
		(explanation ?e))
	(not (recommendation
		(name ?f2&
	:(< (str-compare ?f2 ?f1) 0))))
	=>
	(printout t "*** The most suitable lipstick for you is " ?f1 crlf)
	(printout t "Explanation: " ?e crlf crlf)
	(retract ?r1))

(deffunction run-system ()
	(reset)
	(focus startup interview recommend report)
	(run))

(while TRUE
	(run-system))