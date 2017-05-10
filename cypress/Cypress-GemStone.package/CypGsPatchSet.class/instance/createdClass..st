"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: patch access
"
createdClass: aClass

	| className |
	className := aClass name.
	(createdClasses includesKey: className)
		ifTrue: [self error: 'Duplicate class creation'].
	createdClasses
		add: (SymbolAssociation newWithKey: className value: aClass)