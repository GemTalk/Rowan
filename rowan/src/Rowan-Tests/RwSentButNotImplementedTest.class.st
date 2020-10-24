Class {
	#name : 'RwSentButNotImplementedTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwSentButNotImplementedTest >> _sentButNotImplemented [
	"return a map so that references to unimplemented selectors can be found"

	| sent implemented map notImplemented notImplementedMap |
	sent := IdentitySet new.
	implemented := IdentitySet new.
	map := IdentityDictionary new.
	ClassOrganizer new classes
		do: [ :class | 
			| metaClass |
			class selectors
				do: [ :sel | 
					| meth |
					implemented add: sel.
					meth := class compiledMethodAt: sel.
					map at: meth put: meth _selectorPool.
					sent addAll: meth _selectorPool ].
			metaClass := class class.
			metaClass selectors
				do: [ :sel | 
					| meth |
					implemented add: sel.
					meth := metaClass compiledMethodAt: sel.
					map at: meth put: meth _selectorPool.
					sent addAll: meth _selectorPool ] ].
	notImplemented := sent - implemented.
	notImplemented removeAllPresent: self _specialCases.
	notImplementedMap := SymbolKeyValueDictionary new.
	notImplemented
		do: [ :sel | 
			map
				keysAndValuesDo: [ :meth :selectors | 
					(selectors includes: sel)
						ifTrue: [ 
							| methName |
							methName := (meth homeMethod inClass printString , '>>'
								, meth homeMethod selector asString) asSymbol.
							(notImplementedMap
								at: methName
								ifAbsent: [ notImplementedMap at: methName put: IdentitySet new ])
								add: sel ] ] ].
	^ notImplementedMap
]

{ #category : 'private' }
RwSentButNotImplementedTest >> _specialCases [
	| nonImplemented |
	nonImplemented := Set new.
	(self class allSelectors
		select: [ :ea | ea beginsWith: 'nonImplementedSelectors' ])
		do: [ :selector | nonImplemented addAll: (self perform: selector) ].
	^ nonImplemented
]

{ #category : 'special cases' }
RwSentButNotImplementedTest >> nonImplementedSelectorsGsKernel360 [
	#(#'_announceMethodMoved:oldCategory:' #'_unsafeSetOop:' #'notify:' #'modifyingObject:atOffset:to:' #'changingSizeOfObject:to:' #'deletingIn:startingAt:count:' #'inserting:into:at:insertSize:' #'modifyingByteObject:startingAt:withNewValue:' #'invokingBecomeOn:to:' #'transformIntoDateTime:' #'zork' #'someMessageThatIsntUnderstood' #'adding:to:' #'removing:from:' #'installDocumentation')
]

{ #category : 'test' }
RwSentButNotImplementedTest >> testSentButNotImplemented [
	| sentNotImplemented |
	sentNotImplemented := self _sentButNotImplemented.
	sentNotImplemented
		keysAndValuesDo: [ :meth :selectors | 
			Transcript
				cr;
				show: 'Sent but not implemented from ' , meth printString.
			selectors
				do: [ :sel | 
					Transcript
						cr;
						tab;
						show: sel printString ] ].
	self assert: sentNotImplemented isEmpty description: sentNotImplemented size asString, ' sent but not implemented selectors'
]
