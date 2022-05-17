Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >> _checkConstraints: aConstraint instVarNames: anArrayOfInstvarNames [
  "Returns aConstraint or { aConstraint } "
  | theConstraints |
  theConstraints := aConstraint .
  (self isPointers) ifTrue: [
     (aConstraint _isArray) ifFalse: [ aConstraint _error: #classErrConstraintNotClass ].
  ] ifFalse:[
   (self isBytes) ifTrue:[
     (self instSize ~~ 0) ifTrue: [ self _error: #classErrByteObjInstVars].
     (anArrayOfInstvarNames size ~~ 0) ifTrue: [  self _error: #classErrByteObjInstVars].
     (aConstraint _isArray) ifFalse: [  aConstraint _error: #classErrConstraintNotClass ].
     (aConstraint size ~~ 0) ifTrue: [  self _error: #classErrBadConstraint ] .
   ] ifFalse:[
     (self isNsc) ifTrue:[
	(aConstraint _isArray) ifFalse:[ "for compatibility with 3.0, construct an Array"
		   "specifying inherited constraints on named instance variables" 
		 "plus the specified constraint on unnamed instance variables."
	   theConstraints := { aConstraint } .
	].
     ].
   ].
  ].
  ^ theConstraints

]

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
Class >> _installConstraints: theConstraints [

	| existingConstraintsMap existingVaryingConstraint theConstraintsMap theVaryingConstraint keys existingConstraints myInstVarNames superConstraints superC superInstVarNames superConstraintsMap |
	superConstraintsMap := Dictionary new.
	existingConstraintsMap := Dictionary new.
	existingVaryingConstraint := self _varyingConstraint.
	myInstVarNames := self allInstVarNames.
	superC := self superClass.
	superInstVarNames := superC ifNil: [ {} ] ifNotNil: [ superC allInstVarNames ].
	[ 
	superConstraints := superC
		ifNil: [ {} ]
		ifNotNil: [ :superCl | superCl _constraints ifNil: [ {} ] ].
	existingConstraints := self _constraints ifNil: [ {} ] ]
		on: Deprecated
		do: [ :ex | ex resume ].
	1 to: superConstraints size do: [ :index | 
		superConstraintsMap
			at: (superInstVarNames at: index)
			put: (superConstraints at: index) ].
	1 to: existingConstraints size do: [ :index | 
		existingConstraintsMap
			at: (myInstVarNames at: index)
			put: (existingConstraints at: index) ].
	theConstraintsMap := Dictionary new.
	theVaryingConstraint := Object.
	theConstraints
		do: [ :arrayOrVaryingConstraintClass | 
			arrayOrVaryingConstraintClass _isArray
				ifTrue: [ 
					theConstraintsMap
						at: (arrayOrVaryingConstraintClass at: 1)
						put: (arrayOrVaryingConstraintClass at: 2) ]
				ifFalse: [ theVaryingConstraint := arrayOrVaryingConstraintClass ] ].
	keys := existingConstraintsMap keys copy.
	keys addAll: theConstraintsMap keys.
	keys addAll: superConstraintsMap keys.
	keys
		do: [ :key | 
			| existingConstraint theConstraint superConstraint instVarString |
			existingConstraint := existingConstraintsMap at: key ifAbsent: [  ].
			superConstraint := superConstraintsMap at: key ifAbsent: [  ].
			theConstraint := theConstraintsMap at: key ifAbsent: [  ].
			instVarString := key asString.
			existingConstraint == theConstraint
				ifTrue: [ 
					(theConstraint isNil and: [ superConstraint notNil ])
						ifTrue: [ 
							"inherit constraint from superclass"
							self _rwInstVar: instVarString constrainTo: superConstraint ] ]
				ifFalse: [ 
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
			[ self _setVaryingConstraint: theVaryingConstraint ]
				on: Deprecated
				do: [ :ex | ex resume ] ]

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >> _installConstraints: theConstraints oldClass: oldClass [

	| initialConstraints |
	initialConstraints := theConstraints.
	oldClass ifNotNil: [ [ 
		| oldSuperclass theSuperclass oldSuperConstraints theSuperConstraints theConstraintsAr |
		theConstraintsAr := theConstraints ifNil: [ #() ].
		oldSuperclass := oldClass superclass.
		theSuperclass :=  self superclass.
		(oldSuperclass == theSuperclass or: [ oldSuperclass allInstVarNames = theSuperclass allInstVarNames ])
			ifTrue: [ self _installOldConstraints: oldClass _constraints ]
			ifFalse: [
				| theConstraintMap |
				theConstraintMap := Dictionary new.
				theConstraintsAr do: [:ar |
					theConstraintMap at: (ar at: 1) put: (ar at: 2) ].
				oldSuperConstraints := oldSuperclass _constraints ifNil: [ #() ].
				theSuperConstraints := theSuperclass _constraints ifNil: [ #() ].
				oldSuperConstraints = theSuperConstraints
					ifFalse: [ 
						| superInstVars theInstVars |
						superInstVars := theSuperclass allInstVarNames.
						theInstVars := self allInstVarNames.
						initialConstraints := {}.
						theConstraintsAr do: [:ar | 
							((superInstVars includes: (ar at: 1)) or: [ theInstVars includes: (ar at: 1) ])
								ifFalse: [ theConstraintMap removeKey: (ar at: 1) ] ].
						initialConstraints := theConstraintsAr select: [:ar | theConstraintMap includesKey: (ar at: 1) ] ] ] ] 
		on: Deprecated do: [:ex | ex resume ] ].
	initialConstraints 
		ifNil: [ constraints := nil ]
		ifNotNil: [ self _installConstraints: initialConstraints ]

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >> _installOldConstraints: theConstraints [

	constraints := theConstraints copy

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >> _primSubclass: nameSym
instVarNames: arrayOfIvNames
format: anInteger
constraints: aConstraint
classVars: aSymbolDictionary
poolDictionaries: anArrayOfPoolDicts
classInstanceVars: arrayOfCivNames [

	aConstraint isEmpty ifFalse: [
"need to add constraint creation in this method"
"self halt"].
	^self
		_primSubclass: nameSym
		instVarNames: arrayOfIvNames
		format: anInteger
		classVars: aSymbolDictionary
		poolDictionaries: anArrayOfPoolDicts
		classInstanceVars: arrayOfCivNames

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >> _subclass: className instVarNames: anArrayOfInstvarNames format: theFormat constraints: theConstraints classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary inClassHistory: aClassHistory description: aDescription options: optionsArray [

	"Backport of bugfix for Bug47433"

	"The preferred private subclass creation method.
 optionsArray is an Array of Symbols containing zero or more of  
   #noInheritOptions,  #subclassesDisallowed, #disallowGciStore, #modifiable , 
   #traverseByCallback 
 and at most one of 
   #dbTransient, #instancesNonPersistent, #instancesInvariant .
 If present, #noInheritOptions must be the first element and it causes
 none of subclassesDisallowed, disallowGciStore, traverseByCallback,
         dbTransient, instancesNonPersistent, instancesInvariant 
 to be inherited from the superclass, nor copied from the
 current version of the class.
"

	| cvDict result theName ivNames classCon conEle conEleEle theHist poolDicts modifiableBool fmtArr fmt nCivs sza szb civNames |
	self _validatePrivilege
		ifFalse: [ ^ nil ].
	className _isOneByteString
		ifFalse: [ 
			(className _validateClass: CharacterCollection)
				ifFalse: [ ^ nil ] ].
	self subclassesDisallowed
		ifTrue: [ ^ self _error: #'classErrSubclassDisallowed' ].
	anArrayOfClassInstVars
		ifNotNil: [ 
			anArrayOfClassInstVars _isArray
				ifFalse: [ 
					(anArrayOfClassInstVars _validateClass: Array)
						ifFalse: [ ^ nil ] ] ].
	aDictionary
		ifNotNil: [ 
			(aDictionary _validateClass: SymbolDictionary)
				ifFalse: [ ^ nil ] ].
	fmtArr := self _validateOptions: optionsArray withFormat: theFormat newClassName: className .
	fmt := fmtArr at: 1.
	modifiableBool := fmtArr at: 2.
	(self instancesInvariant and: [ (fmt bitAnd: 16r8) == 0 ])
		ifTrue: [ ^ self _error: #'classErrInvariantSuperClass' ].
	anArrayOfInstvarNames _isArray
		ifFalse: [ 
			(anArrayOfInstvarNames _validateClass: Array)
				ifFalse: [ ^ nil ] ].
	ivNames := {}.
	1 to: anArrayOfInstvarNames size do: [ :j | ivNames add: (anArrayOfInstvarNames at: j) ].
	theConstraints _isArray
		ifFalse: [ classCon := theConstraints ]
		ifTrue: [ 
			classCon := theConstraints class new.
			1 to: theConstraints size do: [ :j | 
				conEle := theConstraints at: j.
				conEle _isArray
					ifFalse: [ classCon add: conEle ]
					ifTrue: [ 
						| temp |
						temp := conEle class new.
						1 to: conEle size do: [ :k | 
							conEleEle := conEle at: k.
							(conEleEle isKindOf: CharacterCollection)
								ifTrue: [ temp add: conEleEle asSymbol ]
								ifFalse: [ temp add: conEleEle ] ].
						classCon add: temp ] ] ].
	nCivs := anArrayOfClassInstVars size.
	civNames := anArrayOfClassInstVars.
	nCivs ~~ 0
		ifTrue: [ 
			| aSet |
			civNames := Array new: nCivs.
			aSet := IdentitySet new.
			1 to: nCivs do: [ :k | 
				| aName |
				aName := (anArrayOfClassInstVars at: k) asSymbol.
				self class _validateNewClassInstVar: aName.
				civNames at: k put: aName.
				aSet add: aName.
				aSet size < k
					ifTrue: [ 
						ImproperOperation
							signal:
								'array of new class instanceVariables contains a duplicate ' , aName printString ] ] ].	"Gs64 v3.0 , cvDict and poolDicts maybe nil from caller,
    and will be converted to nil if caller passed an empty Array."
	cvDict := self _makeClassVarDict: anArrayOfClassVars.	"undo the compiler's canonicalization of empty arrays (fix bug 14103) "
	poolDicts := anArrayOfPoolDicts.
	(poolDicts _isArray and: [ poolDicts size == 0 ])
		ifTrue: [ poolDicts := nil ].
	theName := className asSymbol.
	result := self
		_subclass: theName
		instVarNames: ivNames
		format: fmt
		constraints: classCon
		classVars: cvDict
		poolDictionaries: poolDicts
		classInstanceVars: civNames.
	result _installConstraints: classCon.	
	modifiableBool
		ifTrue: [ result _subclasses: IdentitySet new ].
	subclasses ifNotNil: [ subclasses add: result ].
	aDictionary ifNotNil: [ aDictionary at: theName put: result ].

	result extraDict: SymbolDictionary new.
	result _commentOrDescription: aDescription.
	theHist := aClassHistory.
	theHist ifNil: [ theHist := ClassHistory new name: className ].
	theHist notEmpty
		ifTrue: [ result category: theHist current _classCategory ].
	theHist add: result.
	result classHistory: theHist.
	result timeStamp: DateTime now.
	result userId: System myUserProfile userId.

	sza := self class instSize + anArrayOfClassInstVars size.
	szb := result class instSize.
	sza == szb
		ifFalse: [ 
			InternalError
				signal:
					'prim 233: inconsistent class instance variables, superClass+args=>'
						, sza asString , '  newClass=>' , szb asString ].

	modifiableBool
		ifFalse: [ result immediateInvariant ].
	result copyVariables.
	self _clearCachedOrganizer.
	(fmtArr at: 3)
		ifTrue: [ GsFile gciLogServer: 'created class ' , className ].
	^ result

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Class >> _subclass: nameSym
instVarNames: arrayOfIvNames
format: anInteger
constraints: aConstraint
classVars: aSymbolDictionary
poolDictionaries: anArrayOfPoolDicts
classInstanceVars: arrayOfCivNames [

self _validateNewClassName: nameSym .
^ self _primSubclass: nameSym
  instVarNames: arrayOfIvNames
  format: anInteger
  constraints: aConstraint
  classVars: aSymbolDictionary
  poolDictionaries: anArrayOfPoolDicts
  classInstanceVars: arrayOfCivNames

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
constraints: aConstraint
options: optionsArr [

| hist theConstraints descr oldClass |
self deprecated: 'Obsolete in GemStone/S 64.  The preferred methods are in the Subclass Creation category (' , aString , ').'.
theConstraints := self _checkConstraints: aConstraint instVarNames: anArrayOfInstvarNames .
oldClass := self _classNamed: aString inDictionary: aDictionary .
oldClass ifNotNil:[  
  (self _equivalentSubclass: oldClass superCls: self name: aString newOpts: optionsArr 
	newFormat: self format 
    newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars 
    newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars 
    inDict: aDictionary constraints: aConstraint  isKernel: false ) ifTrue:[
       ^ oldClass  "avoid creation of a new version"
  ].
  hist := oldClass classHistory .
  descr := oldClass comment.
].
^ self _subclass: aString
          instVarNames: anArrayOfInstvarNames
          format: format
          constraints: theConstraints
          classVars: anArrayOfClassVars
          classInstVars: anArrayOfClassInstVars
          poolDictionaries: anArrayOfPoolDicts
          inDictionary: aDictionary
          inClassHistory: hist 
          description: descr
          options: optionsArr

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
