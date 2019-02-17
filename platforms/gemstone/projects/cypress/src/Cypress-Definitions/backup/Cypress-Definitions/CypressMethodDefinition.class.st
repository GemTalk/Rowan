"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressMethodDefinition',
	#superclass : 'CypressDefinition',
	#instVars : [
		'category',
		'classIsMeta',
		'className',
		'selector',
		'source'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressMethodDefinition class >> className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource [

	^self new
		className: aName asString
		classIsMeta: isMetaclass
		selector: aSelector asString
		category: aCategory asString
		source: (self normalizeLineEndingsOf: aSource)

]

{ #category : 'instance creation' }
CypressMethodDefinition class >> forMethod: aGsNMethod [

	| behavior selector |
	behavior := aGsNMethod inClass.
	selector := aGsNMethod selector.
	^self new
		className: behavior theNonMetaClass name asString
		classIsMeta: behavior isMeta
		selector: selector asString
		category: (behavior categoryOfSelector: selector) asString
		source: (self normalizeLineEndingsOf: aGsNMethod sourceString asString)

]

{ #category : 'comparing' }
CypressMethodDefinition >> = aDefinition [
    ^ super = aDefinition
        and: [ aDefinition source = self source
                and: [ aDefinition category = self category ] ]

]

{ #category : 'loading' }
CypressMethodDefinition >> actualClass [

  ^ self theNonMetaClass
    ifNotNil: [:cls |
      self classIsMeta
        ifTrue: [ cls class ]
        ifFalse: [ cls  ] ].

]

{ #category : 'converting' }
CypressMethodDefinition >> asCypressMethodDefinition [

	^self

]

{ #category : 'accessing' }
CypressMethodDefinition >> category [

	^category

]

{ #category : 'visiting' }
CypressMethodDefinition >> classDefinition: classBlock methodDefinition: methodBlock [

	methodBlock value: self

]

{ #category : 'accessing' }
CypressMethodDefinition >> classIsMeta [

	^classIsMeta

]

{ #category : 'accessing' }
CypressMethodDefinition >> className [

	^className

]

{ #category : 'initialization' }
CypressMethodDefinition >> className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource [

	className := aName.
	classIsMeta := isMetaclass.
	selector := aSelector.
	category := aCategory.
	source := self normalizeLineEndingsOf: aSource

]

{ #category : 'accessing' }
CypressMethodDefinition >> description [
	^ Array	
		with: className
		with: selector
		with: classIsMeta

]

{ #category : 'comparing' }
CypressMethodDefinition >> hash [

	| hash |
	hash := classIsMeta asString hash.
	hash := source hash bitOr: hash.
	hash := category hash bitOr: hash.
	hash := className hash bitOr: hash.
	^hash

]

{ #category : 'visiting' }
CypressMethodDefinition >> instanceMethod: instanceBlock classMethod: classBlock [

	^(self classIsMeta
		ifTrue: [ classBlock ]
		ifFalse: [ instanceBlock ]) value: self

]

{ #category : 'testing' }
CypressMethodDefinition >> isInitializer [
	^ self selector = 'initialize' and: [self classIsMeta]

]

{ #category : 'loading' }
CypressMethodDefinition >> loadMethodDefinition [

	self actualClass
		compileMethod: self source
		dictionaries: System myUserProfile symbolList
		category: self category
		environmentId: 0

]

{ #category : 'loading' }
CypressMethodDefinition >> postLoadOver: aDefinition [

	super postLoadOver: aDefinition.
	(self isInitializer
		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 
			ifTrue: [ self theNonMetaClass initialize ].

]

{ #category : 'printing' }
CypressMethodDefinition >> printDetailsOn: aStream [

	aStream
		nextPutAll: self className;
		nextPutAll: (self classIsMeta ifTrue: [' class'] ifFalse: ['']);
		nextPutAll: '>>';
		nextPutAll: self selector.

]

{ #category : 'dependency' }
CypressMethodDefinition >> requirements [
	"Answer list of global names required by this definition"

	^{self className}

]

{ #category : 'accessing' }
CypressMethodDefinition >> selector [

	^selector

]

{ #category : 'accessing' }
CypressMethodDefinition >> source [

	^source

]

{ #category : 'loading' }
CypressMethodDefinition >> theNonMetaClass [

	^self resolveGlobalNamed: self className or: []

]

{ #category : 'loading' }
CypressMethodDefinition >> unloadDefinition [

  self actualClass ifNotNil: [:cl | cl removeSelector: self selector asSymbol ].

]
