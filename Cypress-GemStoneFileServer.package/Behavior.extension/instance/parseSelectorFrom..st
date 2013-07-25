*Cypress-GemStoneFileServer
parseSelectorFrom: methodString

	| meth |
	meth := self
				_parseMethod: methodString
				category: 'xyzzy'
				using: GsSession currentSession symbolList
				environmentId: 0.
	meth class ~~ GsNMethod
		ifTrue: 
			["if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil ifFalse: [^nil].
			meth := meth at: 1].
	^meth selector asString