"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: patch access
"
createdClass: aClass

	| className association |
	className := aClass name.
	(createdClasses includesKey: className)
		ifTrue: [self error: 'Duplicate class creation'].
	association := SymbolAssociation newWithKey: className value: aClass.
	createdClasses add: association.
	^association