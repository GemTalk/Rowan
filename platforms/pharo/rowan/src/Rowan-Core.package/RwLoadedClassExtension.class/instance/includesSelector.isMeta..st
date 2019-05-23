testing
includesSelector: aSelector isMeta: isMeta

	^ isMeta
		ifTrue: [ loadedClassMethods includesKey: aSelector ]
		ifFalse: [ loadedInstanceMethods includesKey: aSelector ]