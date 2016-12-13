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
		(printout t crlf ?question " ")
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
		(explanation
			"Neither lipstick nor lip gloss, this matte lip cream is a new kind of lip color that goes on silky smooth and sets to a matte finish. Soft Matte Lip Cream is surprisingly durable and unlike some matte lipstick formulas, also moisturizing."
		))))

(defrule lipstick-liquid-matte-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "NYX Soft Matte Lip Color Cannes")
		(explanation 
			"Neither lipstick nor lip gloss, this matte lip cream is a new kind of lip color that goes on silky smooth and sets to a matte finish. Soft Matte Lip Cream is surprisingly durable and unlike some matte lipstick formulas, also moisturizing."
		))))

(defrule lipstick-liquid-matte-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "NYX Soft Matte Lip Color Rome")
		(explanation 
			"Neither lipstick nor lip gloss, this matte lip cream is a new kind of lip color that goes on silky smooth and sets to a matte finish. Soft Matte Lip Cream is surprisingly durable and unlike some matte lipstick formulas, also moisturizing."
		))))

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
		(explanation
			"Say hello to a truly loyal long-lasting liquid lipstick—Meet Matt(e) Hughes! Take your pick from eight dreamy shades—each as long-wearing and comfortable as the last. These matte, vanilla-mint liquid lipsticks are stuck on you without feeling clingy."
		))))

(defrule lipstick-liquid-matte-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "The Balm Meet Matte Hughes Charming")
		(explanation 
			"Say hello to a truly loyal long-lasting liquid lipstick—Meet Matt(e) Hughes! Take your pick from eight dreamy shades—each as long-wearing and comfortable as the last. These matte, vanilla-mint liquid lipsticks are stuck on you without feeling clingy."
		))))

(defrule lipstick-liquid-matte-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "The Balm Meet Matte Hughes Trustworthy")
		(explanation 
			"Say hello to a truly loyal long-lasting liquid lipstick—Meet Matt(e) Hughes! Take your pick from eight dreamy shades—each as long-wearing and comfortable as the last. These matte, vanilla-mint liquid lipsticks are stuck on you without feeling clingy."
		))))

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
		(explanation
			"You are the artist to your lips with Matte Lip Paint. Apply this richly pigmented lip paint and your lips are sure to look pout-full!! Did you know Girlactik gives the most product in a vial for matte lippies? You get 7.5ML…that’s more than double of most brands!"
		))))

(defrule lipstick-liquid-matte-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Girlactik Matte Lip Paint Allure")
		(explanation 
			"You are the artist to your lips with Matte Lip Paint. Apply this richly pigmented lip paint and your lips are sure to look pout-full!! Did you know Girlactik gives the most product in a vial for matte lippies? You get 7.5ML…that’s more than double of most brands!"
		))))

(defrule lipstick-liquid-matte-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Girlactik Matte Lip Paint Divine")
		(explanation 
			"You are the artist to your lips with Matte Lip Paint. Apply this richly pigmented lip paint and your lips are sure to look pout-full!! Did you know Girlactik gives the most product in a vial for matte lippies? You get 7.5ML…that’s more than double of most brands!"
		))))

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
		(explanation "Tint promotes moist & shiny cherry lips with a creamy, smooth texture."))))

(defrule lipstick-liquid-stain-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Etude House Fresh Cherry Tint Pink")
		(explanation "Tint promotes moist & shiny cherry lips with a creamy, smooth texture."))))

(defrule lipstick-liquid-stain-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Etude House Fresh Cherry Tint Red")
		(explanation "Tint promotes moist & shiny cherry lips with a creamy, smooth texture."))))

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
		(explanation "This dual-purpose liquid stain gives lips and cheeks a natural pop of buildable colour. Use under lip gloss for a show-stopping-pout and dab onto cheeks for a natural-looking rosy glow."))))

(defrule lipstick-liquid-stain-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Body Shop Lip & Cheek Stain Dark Cherry")
		(explanation "This dual-purpose liquid stain gives lips and cheeks a natural pop of buildable colour. Use under lip gloss for a show-stopping-pout and dab onto cheeks for a natural-looking rosy glow."))))

(defrule lipstick-liquid-stain-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Body Shop Lip & Cheek Stain Deep Berry")
		(explanation "This dual-purpose liquid stain gives lips and cheeks a natural pop of buildable colour. Use under lip gloss for a show-stopping-pout and dab onto cheeks for a natural-looking rosy glow."))))

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
		(explanation 
			"This mango-tinted stain beautifully blushes lips & cheeks with a tropical coral hue. Our sheer stain has a natural finish that will leave you looking deliciously vibrant for hours."
		))))

(defrule lipstick-liquid-stain-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Benefit Posietint Cheek & Lip Stain")
		(explanation 
			"This poppy-pink tinted stain brightens cheeks & lips with a cheery flush. The liquid-gel formula beautifully blends on all complexions and lasts for hours."
		))))

(defrule lipstick-liquid-stain-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx stain))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Benefit Benetint Cheek & Lip Stain")
		(explanation 
			"Our original rose-tinted stain is kiss-proof, see-through color for lips & cheeks that lasts for hours. Originally created for an exotic dancer in the 1970s, benetint has soared to cult fave status."
		))))

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
		(explanation "Buttery soft and silky smooth, our decadent Butter Gloss is now available in 12 more sumptuous shades! Each glossy color delivers sheer to medium coverage that melts onto your lips and is never sticky, leaving your lips soft, supple and kissable."))))

(defrule lipstick-liquid-gloss-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "NYX Butter Gloss Praline")
		(explanation "Buttery soft and silky smooth, our decadent Butter Gloss is now available in 12 more sumptuous shades! Each glossy color delivers sheer to medium coverage that melts onto your lips and is never sticky, leaving your lips soft, supple and kissable."))))

(defrule lipstick-liquid-gloss-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "NYX Butter Gloss Ginger Snap")
		(explanation "Buttery soft and silky smooth, our decadent Butter Gloss is now available in 12 more sumptuous shades! Each glossy color delivers sheer to medium coverage that melts onto your lips and is never sticky, leaving your lips soft, supple and kissable."))))

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
		(explanation "A lip gloss with a light-reflecting plumped shine inspired by gel nail polishes. Ultra Shine Lip Gel transforms lips with a gel-effect formula for a light-reflecting, plumped shine. Its ultra-light, sensorial texture is also hydrating."))))

(defrule lipstick-liquid-gloss-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Sephora Ultra Shine Lip Gel Deep Rose")
		(explanation "A lip gloss with a light-reflecting plumped shine inspired by gel nail polishes. Ultra Shine Lip Gel transforms lips with a gel-effect formula for a light-reflecting, plumped shine. Its ultra-light, sensorial texture is also hydrating."))))

(defrule lipstick-liquid-gloss-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Sephora Ultra Shine Lip Gel Lady Luck")
		(explanation "A lip gloss with a light-reflecting plumped shine inspired by gel nail polishes. Ultra Shine Lip Gel transforms lips with a gel-effect formula for a light-reflecting, plumped shine. Its ultra-light, sensorial texture is also hydrating."))))

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
		(explanation "An opaque, high-shine lip color. Use Anastasia Beverly Hills Lip Gloss’ flat sponge-tip applicator to deliver intense pigment with precision for an expert finish in a single swipe. Can be worn alone or layered over lipstick, non-drying formula, vanilla scent, and available in 43 shades."))))

(defrule lipstick-liquid-gloss-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Anastasia Beverly Hills Lipgloss Metallic Rose")
		(explanation "An opaque, high-shine lip color. Use Anastasia Beverly Hills Lip Gloss’ flat sponge-tip applicator to deliver intense pigment with precision for an expert finish in a single swipe. Can be worn alone or layered over lipstick, non-drying formula, vanilla scent, and available in 43 shades."))))

(defrule lipstick-liquid-gloss-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh liquid))
		(texture ?tx&:(eq ?tx gloss))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Anastasia Beverly Hills Lipgloss Tara")
		(explanation "An opaque, high-shine lip color. Use Anastasia Beverly Hills Lip Gloss’ flat sponge-tip applicator to deliver intense pigment with precision for an expert finish in a single swipe. Can be worn alone or layered over lipstick, non-drying formula, vanilla scent, and available in 43 shades."))))

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
		(explanation "This convenient jumbo sized lip pencil creates exact color application with twist up ease so you never have to sharpen. The pigment rich color glides on effortlessly and easily to provide long lasting matte color. The enriched Vitamin A, C & E formula moisturizes and hydrates lips for beautiful healthy looking lips."))))

(defrule lipstick-stick-matte-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "e.l.f Matte Lip Colour Praline")
		(explanation "This convenient jumbo sized lip pencil creates exact color application with twist up ease so you never have to sharpen. The pigment rich color glides on effortlessly and easily to provide long lasting matte color. The enriched Vitamin A, C & E formula moisturizes and hydrates lips for beautiful healthy looking lips."))))

(defrule lipstick-stick-matte-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "e.l.f Matte Lip Colour Cranberry")
		(explanation "This convenient jumbo sized lip pencil creates exact color application with twist up ease so you never have to sharpen. The pigment rich color glides on effortlessly and easily to provide long lasting matte color. The enriched Vitamin A, C & E formula moisturizes and hydrates lips for beautiful healthy looking lips."))))

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
		(explanation "You’ll be the ultimate turn up girl in this soft coral nude in a matte finish."))))

(defrule lipstick-stick-matte-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Colourpop Lippie Stix Brink")
		(explanation "A warm dusty taupe in a matte finish. Curated by @brittanysuleiman. Inspired by the insta-famous Kylie lip"))))

(defrule lipstick-stick-matte-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Colourpop Lippie Stix Hype Daydream")
		(explanation "Matte X finish toned plummy brown"))))

;;;

(defrule lipstick-stick-matte-c-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "Stila Matte'ificent Lipstick Jolie")
		(explanation "Master the matte lip look with this must-have lipstick. Formulated with the perfect balance of concentrated matte pigments and oils, this lasting lip color creates a smooth, weightless, creamy matte finish that’s never heavy or dry. Infused with nourishing argon oil, castor oil, and vitamin E, it offers non-drying and vibrant, cushion-comfort coverage and one-swipe color."))))

(defrule lipstick-stick-matte-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Stila Matte'ificent Lipstick Mon Ami")
		(explanation "Master the matte lip look with this must-have lipstick. Formulated with the perfect balance of concentrated matte pigments and oils, this lasting lip color creates a smooth, weightless, creamy matte finish that’s never heavy or dry. Infused with nourishing argon oil, castor oil, and vitamin E, it offers non-drying and vibrant, cushion-comfort coverage and one-swipe color."))))

(defrule lipstick-stick-matte-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx matte))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Stila Matte'ificent Lipstick Brulee")
		(explanation "Master the matte lip look with this must-have lipstick. Formulated with the perfect balance of concentrated matte pigments and oils, this lasting lip color creates a smooth, weightless, creamy matte finish that’s never heavy or dry. Infused with nourishing argon oil, castor oil, and vitamin E, it offers non-drying and vibrant, cushion-comfort coverage and one-swipe color."))))

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
		(explanation "For seven decades, women everywhere have indulged in the bold, luxurious shades of our most iconic lipstick. Super Lustrous™ creates unforgettably seductive lip color in four finishes — crème, matte, pearl, and sheer — and is available in 82 vibrant shades. The legacy lives on. "))))

(defrule lipstick-stick-satin-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Revlon Super Lustrous Creme Lipstick Rose Velvet")
		(explanation "For seven decades, women everywhere have indulged in the bold, luxurious shades of our most iconic lipstick. Super Lustrous™ creates unforgettably seductive lip color in four finishes — crème, matte, pearl, and sheer — and is available in 82 vibrant shades. The legacy lives on. "))))

(defrule lipstick-stick-satin-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Revlon Super Lustrous Creme Lipstick Rum Raisin")
		(explanation "For seven decades, women everywhere have indulged in the bold, luxurious shades of our most iconic lipstick. Super Lustrous™ creates unforgettably seductive lip color in four finishes — crème, matte, pearl, and sheer — and is available in 82 vibrant shades. The legacy lives on. "))))

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
		(explanation "Dress your lips in sumptuous colour with Color Riche lipstick. Indulge in either a creamy matte or a lacquer finish."))))

(defrule lipstick-stick-satin-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "L'Oreal Paris Color Riche Lipstick Blushing Berry")
		(explanation "Dress your lips in sumptuous colour with Color Riche lipstick. Indulge in either a creamy matte or a lacquer finish."))))

(defrule lipstick-stick-satin-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "L'Oreal Paris Color Riche Lipstick Spiced Cider")
		(explanation "Dress your lips in sumptuous colour with Color Riche lipstick. Indulge in either a creamy matte or a lacquer finish."))))

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
		(explanation "A lipstick with hundreds of hues. The iconic product that made M·A·C famous."))))

(defrule lipstick-stick-satin-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "MAC Satin Lipstick Twig")
		(explanation "A lipstick with hundreds of hues. The iconic product that made M·A·C famous."))))

(defrule lipstick-stick-satin-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx satin))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "MAC Satin Lipstick Del Rio")
		(explanation "A lipstick with hundreds of hues. The iconic product that made M·A·C famous."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule lipstick-stick-sheer-a-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
			(name "Revlon Colorburst Lip Butter Peach Parfait")
			(explanation "Buttery balm with beautiful shiny colour to give you baby soft, healthy glowing lips - 94% of women felt lips were softer, smoother, and instantly hydrated. Hydrating mango, shea and coconut butter formula boosts lip moisture. Pampering gel formula provides melts onto the lips like butter. Sheer to medium colour"))))

(defrule lipstick-stick-sheer-a-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Revlon Colorburst Lip Butter Pink Truffle")
		(explanation "Buttery balm with beautiful shiny colour to give you baby soft, healthy glowing lips - 94% of women felt lips were softer, smoother, and instantly hydrated. Hydrating mango, shea and coconut butter formula boosts lip moisture. Pampering gel formula provides melts onto the lips like butter. Sheer to medium colour"))))

(defrule lipstick-stick-sheer-a-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr a))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Revlon Colorburst Lip Butter Pink Truffle")
		(explanation "Buttery balm with beautiful shiny colour to give you baby soft, healthy glowing lips - 94% of women felt lips were softer, smoother, and instantly hydrated. Hydrating mango, shea and coconut butter formula boosts lip moisture. Pampering gel formula provides melts onto the lips like butter. Sheer to medium colour"))))

;;;

(defrule lipstick-stick-sheer-b-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "Burt's Bees Tinted Balm Pink Blossom")
		(explanation "For just a hint of color and 8 hours of moisture. The Botanical Waxes in these softly tinted balms will take your lips to lovely in one pretty swipe. They come in a range of 6 naturally flattering shades. 8-hour moisture, adds a hint of natural color, nourishes with Shea Butter and Botanical Waxes, and available in 6 sheer, wearable every day shades"))))

(defrule lipstick-stick-sheer-b-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "Burt's Bees Tinted Balm Hibiscus")
		(explanation "For just a hint of color and 8 hours of moisture. The Botanical Waxes in these softly tinted balms will take your lips to lovely in one pretty swipe. They come in a range of 6 naturally flattering shades. 8-hour moisture, adds a hint of natural color, nourishes with Shea Butter and Botanical Waxes, and available in 6 sheer, wearable every day shades"))))

(defrule lipstick-stick-sheer-b-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr b))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "Burt's Bees Tinted Balm Red Dahlia")
		(explanation "For just a hint of color and 8 hours of moisture. The Botanical Waxes in these softly tinted balms will take your lips to lovely in one pretty swipe. They come in a range of 6 naturally flattering shades. 8-hour moisture, adds a hint of natural color, nourishes with Shea Butter and Botanical Waxes, and available in 6 sheer, wearable every day shades"))))

;;;

(defrule lipstick-stick-sheer-c-light
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk light)))
	=>
	(assert (recommendation
		(name "NARS Sheer Lipstick Cruisin")
		(explanation "A beautifully sheer formula infused with ingredients to improve the condition of the lips. Sheer, long lasting color pigments have been suspended in this translucent formula to provide a sophisticated shine. Lightweight formula glides on smoothly, lush shade range, patented blend of conditioners and antioxidants hydrate, nourish and protect lips"))))

(defrule lipstick-stick-sheer-c-medium
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk medium)))
	=>
	(assert (recommendation
		(name "NARS Sheer Lipstick Dolce Vita")
		(explanation "A beautifully sheer formula infused with ingredients to improve the condition of the lips. Sheer, long lasting color pigments have been suspended in this translucent formula to provide a sophisticated shine. Lightweight formula glides on smoothly, lush shade range, patented blend of conditioners and antioxidants hydrate, nourish and protect lips"))))

(defrule lipstick-stick-sheer-c-dark
	(lipstick
		(shape ?sh&:(eq ?sh stick))
		(texture ?tx&:(eq ?tx sheer))
		(price ?pr&:(eq ?pr c))
		(skintone ?sk&:(eq ?sk dark)))
	=>
	(assert (recommendation
		(name "NARS Sheer Lipstick Outsider")
		(explanation "A beautifully sheer formula infused with ingredients to improve the condition of the lips. Sheer, long lasting color pigments have been suspended in this translucent formula to provide a sophisticated shine. Lightweight formula glides on smoothly, lush shade range, patented blend of conditioners and antioxidants hydrate, nourish and protect lips"))))

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