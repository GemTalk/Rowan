Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel-extensions' }
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

{ #category : '*rowan-gemstone-kernel-extensions' }
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