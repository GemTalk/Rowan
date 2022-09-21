Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel' }
Class >> _rwDefinitionOfConstraints [
"This is the part of the definition that describes constraints.  Constraints are
 deprecated and not enforced, but may still be useful for some purposes.
 Returns a string of the form
     constraints: { <Array of instance-variable-symbol/class-name pairs> }
 "

| result firstElement constraintArray |
result := String new.

result add: 'constraints: '.
( constraints isKindOf: Array ) ifTrue: [
	constraintArray := self _rwSortedConstraints.
    result addAll: '{ '.
    firstElement := true.
    constraintArray do: [ :ar |
        " if not the first constraint, prefix with a period to separate
          from the last constraint "
        firstElement ifFalse: [
          result add: ' . '; lf; add: '                '
        ]
        ifTrue: [
          firstElement := false
        ].
		(ar  isKindOf: Array ) 
			ifTrue: [
				result add: '{ #'; add: (ar at: 1) ;
					add: ' . '; add: (ar at: 2) name; addLast: $} ]
			ifFalse: [ 
				"varyingConstraint"
				result add: ar name ].
      ].
    result add: ' }'.
  ]
  ifFalse: [
    constraints class class == Metaclass3 ifTrue: [
      result add: constraints name.
    ]
    ifFalse: [
      result add: ' nil'
    ].
  ].

^result

]

{ #category : '*rowan-gemstone-kernel' }
Class >> _rwOptionsForDefinition [ 
  "copy of _optionsForDefinition"

  | result arr |
  result :=  'options: #(' copy .
  arr := self _rwOptionsArray .
  1 to: arr size do:[:j | result add: $ ; add: (arr at: j) ].
  result add: $)  .
  ^ result

]

{ #category : '*rowan-gemstone-kernel' }
Class >> _rwSortedConstraints [
"as of https://github.com/dalehenrich/Rowan/issues/293, no longer sorting in alphabetical order ... instance variable order is the right answer"

|   aConstraint constraintArray |
( constraints isKindOf: Array ) ifTrue: [
	constraintArray := {}.
    1 to: self instSize do: [ :x |
      aConstraint := constraints atOrNil: x .
      ((aConstraint ~~ nil _and: [aConstraint ~~ Object])
          _and:[ superClass == nil
            _or:[ ((superClass _namedIvConstraintAt: x) isVersionOf: aConstraint) not ]] )
      ifTrue: [ constraintArray add: {(instVarNames at: x) . aConstraint } ] ].

    aConstraint:= self _varyingConstraint.
    ( (aConstraint ~~ Object) _and:
        [(superClass _varyingConstraint) ~~ aConstraint] )
		ifTrue: [ constraintArray add: aConstraint ]
  ]
  ifFalse: [
    constraints class class == Metaclass3 ifTrue: [
      ^ constraints
    ]
    ifFalse: [
      ^nil
    ].
  ].

^constraintArray
]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'byteSubclass'
		superclass: self name asString
		instVarNames: #()
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		options: optionsArray
]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName  options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'byteSubclass'
		superclass: self name asString
		instVarNames: #()
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aPackageName
		constraints: #()
		options: optionsArray
]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwCategory [
	"Provide direct access to category of class."

	^ self category
]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwCategory: aString [
	^ Rowan projectTools browser
		updateClassCategory: aString
		forClassNamed: self thisClass name asString
]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwComment [

	"Provide direct access to comment of class, bypassing default comeent string."
  
  ^ (self _extraDictAt: #comment) ifNil: [ '' ]
]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwComment: aString [

	^ Rowan projectTools browser
		updateClassComment: aString
		forClassNamed: self thisClass name asString

]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName constraints: constraintArray options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'variable'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aCategoryName
		constraints: constraintArray
		options: optionsArray

]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'variable'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		options: optionsArray
]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName constraints: constraintArray options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'variable'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aPackageName
		constraints: constraintArray
		options: optionsArray

]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'variable'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aPackageName
		constraints: #()
		options: optionsArray

]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName constraints: constraintArray options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'normal'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aCategoryName
		constraints: constraintArray
		options: optionsArray

]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'normal'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		options: optionsArray
]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName constraints: constraintArray options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'normal'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aPackageName
		constraints: constraintArray
		options: optionsArray

]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'normal'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aPackageName
		constraints: #()
		options: optionsArray

]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary newVersionOf: oldClass category: aCategoryName packageName: aPackageName options: optionsArray [
	| loadedPackage |
	loadedPackage := Rowan image
		loadedPackageNamed: aPackageName
		ifAbsent: [ self error: 'No loaded package found for ' , aPackageName printString ].

	aDictionary
		ifNotNil: [ 
			| expectedSymDictName specifiedSymDictName |
			(expectedSymDictName := loadedPackage loadedProject
				gemstoneSymbolDictNameForPackageNamed: aPackageName)
				~= (specifiedSymDictName := aDictionary name asString)
				ifTrue: [ 
					self
						error:
							'Attempt to move a packaged class ' , aString printString
								, ' from the symbol dictionary ' , expectedSymDictName printString
								, ' to the symbol dictionary ' , specifiedSymDictName printString
								, '. Please use the Rowan api to achieve the move' ] ].

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'normal'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aPackageName
		constraints: #()
		options: optionsArray
]
