"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
forClass: aClass

	| superclassname |
	superclassname := aClass superclass
				ifNil: ['nil']
				ifNotNil: [:sClass | sClass name].
	^self
		name: aClass name
		superclassName: superclassname
		category: aClass category
		instVarNames: aClass instVarNames
		classInstVarNames: aClass class instVarNames
		classVarNames: aClass classVarNames
		poolDictionaryNames: aClass sharedPools
		comment: aClass comment
		subclassType: (self subclassTypeOf: aClass)
