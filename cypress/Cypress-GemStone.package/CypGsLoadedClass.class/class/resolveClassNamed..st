"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
resolveClassNamed: aSymbol
	"If the given symbol resolves in this session, *and* if what it resolves to is a class, answer that class.
	Otherwise, answer nil."

	| possibleClass |
	possibleClass := GsCurrentSession currentSession objectNamed: aSymbol.
	^(possibleClass isBehavior and: [possibleClass isMeta not])
		ifTrue: [possibleClass]
		ifFalse: [nil]