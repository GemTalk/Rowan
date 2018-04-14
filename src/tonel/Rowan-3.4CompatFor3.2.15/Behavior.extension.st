Extension { #name : 'Behavior' }

{ #category : '*rowan-3.4compatfor3.2.15' }
Behavior >> moveMethod: aSelector toCategory: categoryName environmentId: envId [

	"Moves the method aSelector (a String) from its current category to the
 specified category (also a String).  If either aSelector or categoryName is
 not in the receiver's method dictionary, or if aSelector is already in
 categoryName, generates an error..
 This method does not account for selectors inherited from superclasses."

	| selSym catSym oldCat |
	self _validatePrivilege
		ifFalse: [ ^ nil ].
	(self includesCategory: categoryName environmentId: envId)
		ifFalse: [ ^ self _categoryNotFound: categoryName ].
	oldCat := self categoryOfSelector: aSelector environmentId: envId.

"Backport of a change in behavior that was made in 3.4 and is needed for Rowan" 
"
	oldCat ifNil: [ ^ self _error: #'classErrSelectorNotFound' args: {aSelector} ].
"

	catSym := categoryName asSymbol.
	selSym := aSelector asSymbol.

	(envId ~~ 0
		or: [ self canWriteMethods or: [ GsPackagePolicy current enabled not ] ])
		ifTrue: [ 
			| catDict |
			catDict := self _baseCategorysForStore: envId.
			oldCat ifNotNil: [(catDict at: oldCat) remove: selSym].
			(catDict at: catSym) add: selSym ]
		ifFalse: [ GsPackagePolicy current moveSelector: selSym toCategory: catSym for: self ].
	self _needsAnnouncement
		ifTrue: [ self _announceMethodMoved: (self compiledMethodAt: selSym) oldCategory: oldCat ]

]
