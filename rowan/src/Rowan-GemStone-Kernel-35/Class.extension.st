Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel-35' }
Class >> _subclass: aString
instVarNames: anArrayOfInstvarNames
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
newVersionOf: oldClass
description: aDescription
options: optionsArray [

 "optionsArray is an Array of Symbols containing zero or more of
   #noInheritOptions,  #subclassesDisallowed, #disallowGciStore, #modifiable ,
   #traverseByCallback #selfCanBeSpecial #logCreation
 and at most one of
   #dbTransient, #instancesNonPersistent, #instancesInvariant
 If present, #noInheritOptions must be the first element and it causes
 none of subclassesDisallowed, disallowGciStore, traverseByCallback,
         dbTransient, instancesNonPersistent, instancesInvariant
 to be inherited from the superclass, nor copied from the
 current version of the class.
 #selfCanBeSpecial is never inherited and is needed only when modifying
 superclass hierarchy above classes with special format.
 The option #logCreation, if present in the options array, causes logging
 with GsFile(C)>>gciLogSever:  of class creation / equivalence.

 Returns oldClass if it would be equivalent to the requested new class.
 (See the class comment for Class). "

	| hist descr |
	descr := aDescription.
	oldClass
		ifNotNil:
			[ (self
				_equivalentSubclass: oldClass
				superCls: self
				name: aString
				newOpts: optionsArray
				newFormat: oldClass format
				newInstVars: anArrayOfInstvarNames
				newClassInstVars: anArrayOfClassInstVars
				newPools: anArrayOfPoolDicts
				newClassVars: anArrayOfClassVars
				inDict: aDictionary
				isKernel: false)
					ifTrue:
						[oldClass _commentOrDescription: aDescription.
						^oldClass	"avoid creation of a new version"].
			hist := oldClass classHistory.
			descr
				ifNil:
					[descr := [oldClass comment] on: Error
								do: [:ex | 'old comment not available']]].
	^self
		_subclass: aString
		instVarNames: anArrayOfInstvarNames
		format: format
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionary
		inClassHistory: hist
		description: descr
		options: optionsArray
]
