Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >> _constraintsEqual: anArray [
  "Result true if receiver's constraints equal to anArray or 
   if anArray is empty and receiver's constraints are all Object ."
^ [ | myConstr superInstSiz ofs arySiz |
    anArray _isArray ifTrue:[
      myConstr := constraints .
      superInstSiz := superClass ifNil:[ 0 ] ifNotNil:[:sc | sc instSize] .
      (arySiz := anArray size) == 0 ifTrue:[
	superInstSiz + 1 to: myConstr size do:[:j | 
	  (myConstr at:j ) == Object ifFalse:[ 
	     (j == (self instSize + 1) and:[ superClass ~~ nil]) ifTrue:[
	       ^ self _varyingConstraint isVersionOf: superClass _varyingConstraint 
	     ].
	     ^ false 
	  ].
	].
      ] ifFalse:[ | varConstr instSiz myConstrSiz ivNams |
	instSiz := self instSize .
	varConstr := (myConstr atOrNil: instSiz + 1) ifNil:[ Object]. 
	ofs := 1 .
	myConstr := myConstr copyFrom: superInstSiz + 1 to: instSiz .
	"elements of myConstr, and varConstr, set to nil when finding a matching
	 element in anArray."
	myConstrSiz := myConstr size .
	ivNams := instVarNames .
	1 to: arySiz do:[:j | | elem |
	  elem := anArray at: j .
	  elem _isArray ifTrue:[ | ivNam |
	    ivNam := elem atOrNil: 1 .
	    1 to: myConstrSiz do:[:m |
	      (ivNams at: (superInstSiz + m)) == ivNam ifTrue:[ 
		 ((elem atOrNil: 2) isVersionOf: (myConstr at: m))   ifTrue:[
		   myConstr at: m put: nil .
		 ] ifFalse:[
		   ^ false 
		 ].
	      ].
	    ].
	  ] ifFalse:[
	    j == arySiz ifTrue:[ 
	      (elem isVersionOf: varConstr) ifTrue:[ varConstr := nil ] ifFalse:[ ^ false ]
	    ] ifFalse:[ 
	      ^ false 
	    ].
	  ].
	].
	"items neither nil nor Object were missing from anArray"
	(varConstr == nil or:[ varConstr == Object ]) ifFalse:[ ^ false ].
	1 to: myConstrSiz do:[:j| | cx |
	  ((cx := myConstr at: j ) == nil or:[ cx == Object]) ifFalse:[ ^ false ]
	].
      ]
    ] ifFalse:[
      (self _varyingConstraint isVersionOf: anArray) ifFalse:[ ^ false ].
    ].
    true
  ] onSynchronous: Error do:[:ex| false ].

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >> _equivalentSubclass: oldClass superCls: actualSelf name: aString newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars inDict: aDictionary constraints: aConstraint isKernel: isKernelBool [
	^ self
		_equivalentSubclass: oldClass
		superCls: actualSelf
		name: aString
		newOpts: optionsArray
		newFormat: theFormat
		newInstVars: anArrayOfInstvarNames
		newClassInstVars: anArrayOfClassInstVars
		newPools: anArrayOfPoolDicts
		newClassVars: anArrayOfClassVars
		inDict: aDictionary
		isKernel: isKernelBool

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >> _installConstraints: theConstraints oldClass: oldClass [

	oldClass ifNotNil: [ [ self _installOldConstraints: oldClass _constraints ] on: Deprecated do: [:ex | ex resume ] ].
	theConstraints 
		ifNil: [ constraints := nil ]
		ifNotNil: [ self _installConstraints: theConstraints ]
]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >>  _installConstraints: theConstraints [

	| existingConstraintsMap existingVaryingConstraint theConstraintsMap theVaryingConstraint keys 
		existingConstraints myInstVarNames |
	existingConstraintsMap := Dictionary new.
	existingVaryingConstraint := self _varyingConstraint.
	myInstVarNames := self allInstVarNames.
	existingConstraints := [ self _constraints ifNil: [ {} ] ] on: Deprecated do: [:ex | ex resume ].
	1 to: existingConstraints size do: [:index |
		existingConstraintsMap at: (myInstVarNames at: index) put: (existingConstraints at: index ) ].
	theConstraintsMap := Dictionary new.
	theVaryingConstraint := Object.
	theConstraints do: [:arrayOrVaryingConstraintClass |
		arrayOrVaryingConstraintClass _isArray
			ifTrue: [ theConstraintsMap at: (arrayOrVaryingConstraintClass at: 1) put: (arrayOrVaryingConstraintClass at: 2) ]
			ifFalse: [ theVaryingConstraint := arrayOrVaryingConstraintClass ] ].
	keys := existingConstraintsMap keys copy.
	keys addAll: theConstraintsMap keys.
	keys do: [:key | 
		| existingConstraint theConstraint |
		existingConstraint := existingConstraintsMap at: key ifAbsent: [].
		theConstraint := theConstraintsMap at: key ifAbsent: [].
		existingConstraint == theConstraint
			ifFalse: [ 
				| instVarString |
				instVarString := key asString.
				existingConstraint == nil
					ifTrue: [ 
						"add theConstraint" 
						self _rwInstVar: instVarString constrainTo: theConstraint ]
					ifFalse: [ 
						theConstraint == nil
							ifTrue: [ 
								"remove the constraint" 
								self _rwInstVar: instVarString constrainTo: Object ]
							ifFalse: [
								"change the value of the constraint"
                                self _rwInstVar: instVarString constrainTo: theConstraint ] ] ] ].
	existingVaryingConstraint == theVaryingConstraint
		ifFalse: [
			"change the varying constraint"
			[ self _setVaryingConstraint: theVaryingConstraint] on: Deprecated do: [:ex | ex resume ] ].

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >> _installOldConstraints: theConstraints [

	constraints := theConstraints copy

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
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

	| hist fmt descr newClass |
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
	newClass := self
		_subclass: aString
		instVarNames: anArrayOfInstvarNames
		format: fmt
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionary
		inClassHistory: hist
		description: descr
		options: optionsArray.
	newClass _installConstraints: constraintsArray oldClass: oldClass.
	^ newClass


]

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
	newClass _installConstraints: constraintsArray oldClass: oldClass.
	^ newClass

]
