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
Class >> _rwSortedConstraints [

|   aConstraint constraintArray |
( constraints isKindOf: Array ) ifTrue: [
	constraintArray := {}.
    1 to: self instSize do: [ :x |
      aConstraint := constraints atOrNil: x .
      ((aConstraint ~~ nil _and: [aConstraint ~~ Object])
          _and:[ superClass == nil
            _or:[ (superClass _namedIvConstraintAt: x) ~~ aConstraint ]] )
      ifTrue: [ constraintArray add: {(instVarNames at: x) . aConstraint } ] ].
	constraintArray := constraintArray sort: [:ar1 :ar2 | (ar1 at: 1) <= (ar2 at: 1) ].

    aConstraint:= self varyingConstraint .
    ( (aConstraint ~~ Object) _and:
        [(superClass varyingConstraint) ~~ aConstraint] )
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
Class >> indexableSubclass: aString
instVarNames: anArrayOfInstvarNames
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
newVersionOf: oldClass
description: aDescription
constraints: constraintsArray
options: optionsArray [
	"Creates and returns a new indexable subclass of the receiver.  Instances of the
 new class are represented as pointer objects.

 This method generates an error if instances of the receiver are of special
 storage format or if they are NSCs.

 optionsArray is an Array of Symbols containing zero or more of  
   #noInheritOptions,  #subclassesDisallowed, #disallowGciStore, #modifiable , 
   #traverseByCallback 
 and at most one of 
   #dbTransient, #instancesNonPersistent, #instancesInvariant
 If present, #noInheritOptions must be the first element and it causes
 none of subclassesDisallowed, disallowGciStore, traverseByCallback,
         dbTransient, instancesNonPersistent, instancesInvariant 
 to be inherited from the superclass, nor copied from the
 current version of the class.

 Returns oldClass if it would be equivalent to the requested new class.
 (See Class(C)>>comment). "

	| hist fmt descr |
	self isBytes
		ifTrue: 
			[^aString _error: #classErrBadFormat
				with: 'cannot create indexable subclass of byte class'].
	self isNsc
		ifTrue: 
			[^aString _error: #classErrBadFormat
				with: 'cannot create indexable subclass of Nsc class'].
	fmt := format bitOr: 16r4.	"add indexable bit"
	descr := aDescription.
	oldClass
		ifNotNil: 
			[(self
				_equivalentSubclass: oldClass
				superCls: self
				name: aString
				newOpts: optionsArray
				newFormat: fmt
				newInstVars: anArrayOfInstvarNames
				newClassInstVars: anArrayOfClassInstVars
				newPools: anArrayOfPoolDicts
				newClassVars: anArrayOfClassVars
				inDict: aDictionary
				constraints: constraintsArray isKernel: false )
					ifTrue: 
						[oldClass _commentOrDescription: aDescription.
						^oldClass	"avoid creation of a new version"].
			hist := oldClass classHistory.
			descr ifNil: [descr := oldClass comment]].
	^self
		_subclass: aString
		instVarNames: anArrayOfInstvarNames
		format: fmt
		constraints: constraintsArray
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionary
		inClassHistory: hist
		description: descr
		options: optionsArray

]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'bytes'
		superclass: self name asString
		instVarNames: #()
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aCategoryName
		options: optionsArray
]

{ #category : '*rowan-gemstone-kernel' }
Class >> rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName  options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'bytes'
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
		packageName: aCategoryName
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
		packageName: aCategoryName
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
Class >> subclass: aString
instVarNames: anArrayOfInstvarNames
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
newVersionOf: oldClass
description: aDescription
constraints: constraintsArray
options: optionsArray [

	"Creates and returns a new subclass of the receiver.

 optionsArray is an Array of Symbols containing zero or more of  
   #noInheritOptions,  #subclassesDisallowed, #disallowGciStore, #modifiable , 
   #traverseByCallback 
 and at most one of 
   #dbTransient, #instancesNonPersistent, #instancesInvariant
 If present, #noInheritOptions must be the first element and it causes
 none of subclassesDisallowed, disallowGciStore, traverseByCallback,
         dbTransient, instancesNonPersistent, instancesInvariant 
 to be inherited from the superclass, nor copied from the
 current version of the class.

 Returns oldClass if it would be equivalent to the requested new class.
 (See Class(C)>>comment). "

  | hist descr |
  descr := aDescription.
  oldClass ifNotNil:[
    (self _equivalentSubclass: oldClass superCls: self name: aString
         newOpts: optionsArray newFormat: oldClass format 
         newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars
         newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars
         inDict: aDictionary constraints: constraintsArray isKernel: false ) ifTrue:[
      oldClass _commentOrDescription: aDescription.
      ^oldClass	"avoid creation of a new version"
    ].
    hist := oldClass classHistory.
    descr ifNil: [descr := oldClass comment]
  ].
  ^self _subclass: aString instVarNames: anArrayOfInstvarNames
	format: format constraints: constraintsArray classVars: anArrayOfClassVars
	classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts
	inDictionary: aDictionary inClassHistory: hist
	description: descr options: optionsArray

]
