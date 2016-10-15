"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: snapshotting
"
addExtensionMethodsFromClass: aClass toMap: classMap

	| defs map |
	defs := classMap at: aClass theNonMetaClass
				ifAbsent: [OrderedCollection new].
	map := Dictionary new.
	aClass categorysDo: 
			[:category :selectors |
			category asLowercase = ('*' , self basePackageName asLowercase)
				ifTrue: [map at: category put: selectors asSortedCollection]].
	map keys asSortedCollection do: 
			[:category |
			(map at: category)
				do: [:selector | defs add: (aClass compiledMethodAt: selector) asCypressMethodDefinition]].
	defs notEmpty ifTrue: [classMap at: aClass theNonMetaClass put: defs]