private
_sentButNotImplemented
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