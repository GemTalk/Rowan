Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
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

  | hist descr newClass |
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
  newClass := self _subclass: aString instVarNames: anArrayOfInstvarNames
	format: format classVars: anArrayOfClassVars
	classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts
	inDictionary: aDictionary inClassHistory: hist
	description: descr options: optionsArray.
	newClass _installConstraints: theConstraints oldClass: oldClass.
	^ newClass

]
