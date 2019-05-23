method writing
_methodFileNameFor: aMethodDefinition
	| selector |
	selector := aMethodDefinition selector.
	^ (selector includes: $:)
	ifTrue: [ 
	  selector
		collect: [ :each | 
		  each = $:
			ifTrue: [ $. ]
			ifFalse: [ each ] ] ]
	ifFalse: [ 
	  (self class selectorSpecials includes: selector first)
		ifFalse: [ selector ]
		ifTrue: [ 
		  | output specials |
		  specials := self class selectorSpecials.
		  output := String new writeStreamPortable.
		  output nextPut: $^.
		  selector
			do: [ :each | 
			  output
				nextPutAll:
				  ((specials includes: each)
					ifTrue: [ specials at: each ]
					ifFalse: [ each asString ]) ]
			separatedBy: [ output nextPut: $. ].
		  output contents ] ]