Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel-36x' }
Class >> _rwOptionsArray [
  ^ self _optionsArrayForDefinition
]

{ #category : '*rowan-gemstone-kernel-36x' }
Class >> _rwReservedOop [
 "returns nil or the SmallInteger specifying a reserved oop"
  ^ self asOopNumber <= System _lastReservedOopNumber 
    ifTrue:[ self asOop ] 
    ifFalse:[ nil ].
]

{ #category : '*rowan-gemstone-kernel-36x' }
Class >> rwClassDefinitionInSymbolDictionaryNamed: symDictName [
	"create an RwClassDefinition for the reciever suitable for recreating the class. Ignore methods"

	| loadedClass superclassName |
superclassName := self superClass ifNil: [ 'nil' ] ifNotNil: [:sc| sc name asString ].
	loadedClass := Rowan image
		loadedClassForClass: self
		ifAbsent: [ 
			^ (RwClassDefinition
				newForClassNamed: self name asString
				super: superclassName
				instvars: (self instVarNames collect: [ :each | each asString ])
				classinstvars: (self class instVarNames collect: [ :each | each asString ])
				classvars: (self classVarNames collect: [ :each | each asString ])
				category: self _classCategory
				comment: self commentForFileout
				pools: (self sharedPools collect: [ :each | each name asString ]) asArray
				type: self rwClassType)
				gs_symbolDictionary: symDictName;
				yourself ].
	^ loadedClass asDefinition
]

{ #category : '*rowan-gemstone-kernel-36x' }
Class >> rwClassType [
	"Answer the string the desribes the class type"

	^ self isBytes
		ifTrue: [ 
			self superClass isBytes not
				ifTrue: [ 'byteSubclass' ]
				ifFalse: [ 'normal' ] ]
		ifFalse: [ 
			self areInstancesSpecial
				ifTrue: [ 'immediate' ]
				ifFalse: [ 
					self isNsc
						ifTrue: [ 'normal' ]
						ifFalse: [ 
							(self isVariable and: [ self superClass isVariable not ])
								ifTrue: [ 'variable' ]
								ifFalse: [ 'normal' ] ] ] ]
]
