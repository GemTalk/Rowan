"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
parseSelectorFrom: methodString

	| meth |
	^[meth := self
				_parseMethod: methodString
				category: #'xyzzy'
				using: GsSession currentSession symbolList
				environmentId: 0.
	meth class ~~ GsNMethod
		ifTrue: 
			["if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil ifFalse: [^nil].
			meth := meth at: 1].
	meth selector asString]
		on: CompileError
		do: [:ex | ex return: '_____could_not_parse_selector_from_method_source_____'].
