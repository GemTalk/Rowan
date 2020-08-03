Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-35x' }
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

{ #category : '*rowan-gemstone-35x' }
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

{ #category : '*rowan-gemstone-35x' }
Class >> _installConstraints: theConstraints [

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

{ #category : '*rowan-gemstone-35x' }
Class >> _installConstraints: theConstraints oldClass: oldClass [

	oldClass ifNotNil: [ [ self _installOldConstraints: oldClass _constraints ] on: Deprecated do: [:ex | ex resume ] ].
	theConstraints 
		ifNil: [ constraints := nil ]
		ifNotNil: [ self _installConstraints: theConstraints ]
]

{ #category : '*rowan-gemstone-35x' }
Class >> _installOldConstraints: theConstraints [

	constraints := theConstraints copy
]

{ #category : '*rowan-gemstone-35x' }
Class >> _subclass: className instVarNames: anArrayOfInstvarNames format: theFormat constraints: theConstraints classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary inClassHistory: aClassHistory description: aDescription options: optionsArray [
  ^ self _subclass: className instVarNames: anArrayOfInstvarNames format: theFormat classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary inClassHistory: aClassHistory description: aDescription options: optionsArray
]

{ #category : '*rowan-gemstone-35x' }
Class >> indexableSubclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDict inDictionary: aDictionary newVersionOf: oldClass description: aDescription constraints: constraintsArray options: optionsArray [

	| newClass |
	newClass := self indexableSubclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDict inDictionary: aDictionary newVersionOf: oldClass description: aDescription options: optionsArray.
	^ newClass
]

{ #category : '*rowan-gemstone-35x' }
Class >> subclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary newVersionOf: oldClass description: aDescription constraints: theConstraints options: optionsArray [
	"class creation creates a class with no constraints, so if constraints _are_ specified, we need to add them separately"

	| newClass |
	newClass := self 
		_subclass: aString 
		instVarNames: anArrayOfInstvarNames 
		classVars: anArrayOfClassVars 
		classInstVars: anArrayOfClassInstVars 
		poolDictionaries: anArrayOfPoolDicts 
		inDictionary: aDictionary 
		newVersionOf: oldClass 
		description: aDescription 
		options: optionsArray.
	newClass _installConstraints: theConstraints oldClass: oldClass.
	^ newClass
]
