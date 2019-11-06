"
Defines a class.
Properties should include 'name' and 'super'.
Other typical properties are 'instvars' 'classinstvars' 'classvars'.
Methods are their own definitions: classMethodDefinitions instanceMethodDefinitions

"
Class {
	#name : 'RwClassDefinition',
	#superclass : 'RwAbstractClassDefinition',
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwClassDefinition class >> newForClassNamed: className super: superClassName category: category [

	^ self 
		newForClassNamed: className 
		super: superClassName 
		instvars: #() 
		classinstvars: #() 
		classvars: #() 
		category: category 
		comment: nil 
		pools: #() 
		type: 'normal'
]

{ #category : 'instance creation' }
RwClassDefinition class >> newForClassNamed: className super: superClassName category: category comment: comment [

	^ self 
		newForClassNamed: className 
		super: superClassName 
		instvars: #() 
		classinstvars: #() 
		classvars: #() 
		category: category 
		comment: comment 
		pools: #() 
		type: 'normal'
]

{ #category : 'instance creation' }
RwClassDefinition class >> newForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment pools: pools type: type [

	| propertiesDictionary |
	propertiesDictionary := Dictionary new.
	category
		ifNotNil: [ 
			propertiesDictionary
				at: 'category' put: category;
				yourself ].
	propertiesDictionary
		at: 'classinstvars' put: classinstvars;
		at: 'classvars' put: classvars;
		at: 'instvars' put: instvars;
		at: 'name' put: className;
		at: 'pools' put: pools;
		at: 'superclass' put: superClassName;
		at: 'type' put: type asString.
	comment ifNotNil: [ propertiesDictionary at: 'comment' put: comment ].
	^ self
		withProperties: propertiesDictionary
		classMethodDefinitions: Dictionary new
		instanceMethodDefinitions: Dictionary new
]

{ #category : 'utility' }
RwClassDefinition >> _updateCategoryFromClass: aClass [

	| propertyName newValue |
	propertyName := 'category'.
	newValue := aClass _classCategory ifNil: [ ^ self removeProperty: propertyName ].
	newValue isEmpty
		ifTrue: [ ^ self removeProperty: propertyName ].

	self propertyAt: propertyName put: newValue
]

{ #category : 'utility' }
RwClassDefinition >> _updateClassInstvarNamesFromClass: aClass [

	self classInstVarNames: (aClass class instVarNames collect: [:each | each asString ])
]

{ #category : 'utility' }
RwClassDefinition >> _updateClassNameFromClass: aClass [

	self name: aClass name asString
]

{ #category : 'utility' }
RwClassDefinition >> _updateClassTypeFromClass: aClass [

	"For GemStone, the valid values are:
	byteSubclass
	variable
	normal for non-indexable pointer object"

	"Must be in-synch with RwGsLoadedSymbolDictClass>>updateClassTypeFromClass"

	| propertyName oldValue newValue |
	propertyName := 'type'.
	oldValue := self propertyAt: propertyName.
	newValue := aClass isBytes
						ifTrue: [
							aClass superClass isBytes not
								ifTrue: [ 'byteSubclass' ]
								ifFalse: [ 'normal' ]]
						ifFalse: [ 
							aClass isSpecial
								ifTrue: [ 'immediate' ]
								ifFalse: 
									[ aClass isNsc
										ifTrue: [ 'normal' ]
										ifFalse:  [
											(aClass isVariable and: [aClass superClass isVariable not])
												ifTrue: [ 'variable' ]
												ifFalse: [ 'normal' ] ] ] ].

	oldValue = newValue
		ifFalse: 
			[ self propertyAt: propertyName put: newValue ]
]

{ #category : 'utility' }
RwClassDefinition >> _updateClassVariableNamesFromClass: aClass [

	self classVarNames: ((aClass classVarNames collect: [:each | each asString]) asSortedCollection asArray)
]

{ #category : 'utility' }
RwClassDefinition >> _updateCommentFromClass: aClass [
	| propertyName newValue |
	propertyName := 'comment'.
	newValue := (aClass _extraDictAt: #comment) ifNil: [ ^ self removeProperty: propertyName ].
	newValue isEmpty
		ifTrue: [ ^ self removeProperty: propertyName ].

	self propertyAt: propertyName put: newValue
]

{ #category : 'utility' }
RwClassDefinition >> _updateConstraintsFromClass: aClass [

	| propertyName newValue sortedConstraints |
	propertyName := 'gs_constraints'.
	sortedConstraints := aClass _rwSortedConstraints.
	(sortedConstraints isKindOf: Array ) 
		ifTrue: [
			newValue := (sortedConstraints collect: [:ar | 
					(ar isKindOf: Array )
						ifTrue: [ {(ar at: 1) asString . (ar at: 2) name asString } ]
						ifFalse: [ 
							"varyingConstraint"
							ar name asString ] ]) asArray.
			newValue isEmpty ifTrue: [ ^ self removeProperty: propertyName ] ]
		ifFalse: [
			newValue :=  sortedConstraints 
				ifNil: [ ^ self removeProperty: propertyName ]
				ifNotNil: [ sortedConstraints ] ].

	self propertyAt: propertyName put: newValue
]

{ #category : 'utility' }
RwClassDefinition >> _updateInstvarNamesFromClass: aClass [

	self instVarNames: (aClass instVarNames collect: [:each | each asString ])
]

{ #category : 'utility' }
RwClassDefinition >> _updateOptionsFromClass: aClass [
	"Options are GemStone-specific. If the class has no options, the property is absent.
	Otherwise, the property is a sorted array of strings. The valid values are:

	dbTransient
	disallowGciStore
	instancesInvariant
	instancesNonPersistent
	modifiable
	subclassesDisallowed
	traverseByCallback

	Not all combinations are legal, but we don't have to worry about that here, since we're getting the values
	from the image, and the image is trusted to only have valid values."

	| propertyName newValue |
	propertyName := 'gs_options'.
	newValue := (aClass _rwOptionsArray collect: [:option | option asString])
				asSortedCollection asArray.
	newValue isEmpty 
		ifTrue: [ self removeProperty: propertyName ]
		ifFalse: [ self propertyAt: propertyName put: newValue ].
  aClass _rwReservedOop ifNotNil:[:resOop | self propertyAt:'gs_reservedoop' put: resOop asString ]
                          ifNil:[ self removeProperty:'gs_reservedoop' ].
]

{ #category : 'utility' }
RwClassDefinition >> _updatePoolDictionaryNamesFromClass: aClass [

	"Order of pool dictionaries *does* matter in GemStone, so we keep the names in the order given in the image."

	"self instVarNames: (aClass sharedPools collect: [ :each | each name asString ]) asArray"

	"NOT YET IMPLEMENTED"
]

{ #category : 'utility' }
RwClassDefinition >> _updateSuperclassNameFromClass: aClass [
	self superclassName: (aClass superclass ifNotNil:[:sup | sup name asString] ifNil:['nil'])
]

{ #category : 'utility' }
RwClassDefinition >> _updateSymbolDictionaryFromClass: aClass [

	| ar symbolDictName |
	ar := System myUserProfile dictionariesAndSymbolsOf: aClass.
	symbolDictName := ((ar at: 1) at: 1) name.
	self gs_symbolDictionary: symbolDictName
]

{ #category : 'accessing' }
RwClassDefinition >> classType [
	"Legal types are normal, variable, and bytes."

	^self propertyAt: 'type' ifAbsent: ['normal']
]

{ #category : 'accessing' }
RwClassDefinition >> classVarNames [
  ^ properties at: 'classvars'
]

{ #category : 'accessing' }
RwClassDefinition >> classVarNames: anArrayOfClassVarNames [

	^ properties at: 'classvars' put: anArrayOfClassVarNames
]

{ #category : 'accessing' }
RwClassDefinition >> comment: aString [
  properties at: 'comment' put: aString

]

{ #category : 'accessing' }
RwClassDefinition >> instVarNames [

	^ properties at: 'instvars'
]

{ #category : 'accessing' }
RwClassDefinition >> instVarNames: arrayOfInstanceVariableNames [

	^ properties at: 'instvars' put: arrayOfInstanceVariableNames
]

{ #category : 'accessing' }
RwClassDefinition >> shebang [

	^ self propertyAt: 'shebang' ifAbsent: []
]

{ #category : 'accessing' }
RwClassDefinition >> shebang: aStringOrNil [

	aStringOrNil ifNil: [ ^ self properties removeKey: 'shebang' ifAbsent: [] ].
	^ self propertyAt: 'shebang' put: aStringOrNil
]

{ #category : 'accessing' }
RwClassDefinition >> superclassName [

	^self propertyAt: 'superclass' ifAbsent: [self propertyAt: 'super']
]

{ #category : 'accessing' }
RwClassDefinition >> superclassName: aString [

	^ self propertyAt: 'superclass' put: aString
]

{ #category : 'utility' }
RwClassDefinition >> updatePropertiesFromClass: aClass [

	"Update my properties from the class. Leave unknown properties alone, they may apply to another platform."

	self
		_updateSymbolDictionaryFromClass: aClass;
		_updateClassNameFromClass: aClass;
		_updateSuperclassNameFromClass: aClass;
		_updateInstvarNamesFromClass: aClass;
		_updateClassInstvarNamesFromClass: aClass;
		_updateClassVariableNamesFromClass: aClass;
		_updatePoolDictionaryNamesFromClass: aClass;
		_updateClassTypeFromClass: aClass;
		_updateOptionsFromClass: aClass;
		_updateConstraintsFromClass: aClass;
		_updateCommentFromClass: aClass;
		_updateCategoryFromClass: aClass
]
