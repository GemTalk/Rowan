Extension { #name : 'Behavior' }

{ #category : '*rowan-gemstone-kernel' }
Behavior >> _rowanCopyMethodsAndVariablesFrom: sourceClass dictionaries: dicts [

"Copies all instance and class methods, pool dictionaries, and values of
 class variables from the given class to ourselves. 

 Returns an Array of methods in the source class which failed to compile.
 Some of them might be class methods.  The Array will be empty if none failed.

 Method environmentIds are copied from the source methods.
"

| failed srccls targcls sel doit otherCvs |

self _validatePrivilege ifFalse:[ ^ nil ].
self == sourceClass ifTrue:[ 
  "because iteration is directly over the source's categories dictionaries"
  ^ self error: 'source of copy must not be self' 
].
failed := { } .

"Copy pool dictionaries"
sourceClass _poolDictionaries do: [ :dict | | poolDicts |
  poolDicts := poolDictionaries .
  (poolDicts ~~ nil and:[ poolDicts includesIdentical: dict]) ifFalse: [
      poolDicts ifNil:[ poolDicts := { } . poolDictionaries := poolDicts ]
          ifNotNil:[ poolDicts isInvariant ifTrue:[
                       poolDicts := Array withAll: poolDicts . poolDictionaries := poolDicts
                     ]].
      poolDicts add: dict
  ].
].

"Copy class variable values"
otherCvs := sourceClass _classVars .
otherCvs ifNotNil:[ | destCvs |
   destCvs := classVars . 
   otherCvs == destCvs ifTrue: [ self halt ].
   otherCvs associationsDo: [ :assn | | other |
    destCvs ifNotNil:[ other := destCvs associationAt: assn key otherwise: nil ].
	(other == assn)
		ifTrue: [
			"avoid sharing associations between the reciever and sourceClass"
			destCvs removeKey: other key.	"remove the association"
			destCvs at: assn key put: assn value
		].
    (other == nil or: [other value == nil and: [assn value ~~ nil]]) ifTrue: [
        destCvs ifNil:[ destCvs := self _createClassVarsDict ].
        destCvs at: assn key put: assn value
    ].
  ].
].

"Copy class and instance methods"
1 to: 2 do: [ :j | | envId |
  j == 1 ifTrue:[ srccls := sourceClass.  targcls := self ] 
        ifFalse:[ srccls := sourceClass class.  targcls := self class ].
  envId := 0 .
  srccls categorysDo:[ :cat :sels |
      1 to: sels size do: [ :s | | oldMeth |
	sel := sels at: s.
	doit := true.
	doit ifTrue: [ | methEnvId |
          oldMeth := srccls compiledMethodAt: sel environmentId: envId .
	  methEnvId := oldMeth environmentId .
          methEnvId == envId ifFalse:[ self error:'environmentId mismatch']. 
	  ( targcls 
	     _compileMethodTrappingErrors: oldMeth sourceString 
	     dictionaries: dicts category: cat environmentId: envId ) ifNotNil:[
	    failed add: oldMeth 
          ].
        ].
      ].
  ].
].

^failed.

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> _rwInstVar: aString constrainTo: aClass [
"Copy of instVar:constratinTo: without requiring that class be mutable"

"Changes the receiver's constraint on the instance variable named
 aString to aClass.

 The argument aString must be the name of an instance variable defined in the
 receiver or inherited from a superclass.  aClass must be a kind of Class.
 The receiver, and any subclasses for which a constraint change will result,
 must be modifiable; otherwise, an error will be generated.

 If the superclass of the receiver has a constraint on the same instance
 variable, then aClass must be identical to, or a subclass of, that inherited
 constraint.

 For each of the receiver's subclasses, if the constraint on the specified
 instance variable is aClass or is a subclass of aClass, then that constraint
 will be unchanged.  Otherwise, the subclass's constraint will be changed to
 aClass."

| ivInfo offset mySubclasses superConstraintClass |
self _validatePrivilege ifFalse:[ ^ nil ].
aClass _validateIsClass ifFalse:[ ^ nil ].
(Symbol _existingWithAll: aString) ifNotNil:[ :aSymbol |
  ivInfo := self _ivOffsetAndConstraint: aSymbol .
].
ivInfo ifNil:[ ^ self _error: #classErrNotAVar args:{ aString } ] .
offset := ivInfo at: 1 .
superConstraintClass := self superClass _namedIvConstraintAtOffset: offset.
(aClass ~~ Object and: [superConstraintClass ~~ Object])
ifTrue: [ 
	(aClass validateSubclassOf: superConstraintClass) ifFalse:[
	  ^ nil
	] ].
mySubclasses := self subclasses .
self _rwNewConstraint: aClass atOffset: offset .
mySubclasses do:[:x| x _rwNewInheritedConstraint: aClass atOffset: offset ] .

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> _rwNewConstraint: aClass atOffset: offset [

	| oldConstraintsArray newConstraintsArray constraintsIndex instanceVariableNames |
	oldConstraintsArray := constraints.
	newConstraintsArray := oldConstraintsArray copy.
	instanceVariableNames := self class allInstVarNames.
	constraintsIndex := instanceVariableNames indexOf: #constraints.
	self _unsafeAt: constraintsIndex put: newConstraintsArray.

	[ self _newConstraint: aClass atOffset: offset ] on: Deprecated do: [:ex | ex resume ].

	constraints immediateInvariant
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> _rwNewInheritedConstraint: aClass atOffset: offset [

"Change the constraint for offset to aClass if that offset is not
 already constrained to be a subclass of aClass.  Assume all
 variance and error checks have been done."

self _validatePrivilege ifTrue:[ 
  ((self _constraintAt: offset ) isSubclassOf: aClass ) ifFalse:[
    self _rwNewConstraint: aClass atOffset: offset .
    self subclasses do:[:aSubcls|
       aSubcls _rwNewInheritedConstraint: aClass atOffset: offset ].
  ] .
]
 
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rowanPackageName [

	"answer the name of the package that the receiver is a member of. Answer 'Rowan unpackagedName` if 
		the receiver is not a member of any package"

	| loadedClass |
	loadedClass := Rowan image
		loadedClassForClass: self thisClass
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedClass loadedPackage name

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rowanProjectName [

	"answer the name of the project that the receiver is a member of. Answer `Rowan unpackagedName` if 
		the receiver is not a member of any project"

	| loadedClass |
	loadedClass := Rowan image
		loadedClassForClass: self thisClass
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedClass loadedProject name

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwCompileExtensionMethod: sourceString category: categoryName packageName: packageName [
	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: categoryName
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		inPackageNamed: packageName
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwCompileExtensionMethod: sourceString package: aPackageName [

	| aCategory |
	aCategory := '*', aPackageName asLowercase .
	^Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategory asString asLowercase
		forClassNamed: self thisClass name asString
		isMeta: self isMeta

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwCompileMethod: sourceString category: aCategoryString [

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString asString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwCompileMethod: sourceString category: aCategoryString packageName: packageName [

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		inPackageNamed: packageName
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwCompileMethod: sourceString dictionaries: aSymbolList category: aCategoryString packageName: packageName [
	"Rowan version of Behavior >> #compileMethod:dictionaries:category:environmentId:"

	"This compiles some source code for the receiver.  The first argument,
 sourceString, is the string of source code to be compiled.  The second
 argument is a SymbolList to be used in parsing, along with the list of all
 class variables and pool dictionaries for the receiver and all of its
 superclasses.  The third argument (a String) indicates the method's category.

 sourceString must be a kind of String or DoubleByteString.  Instances of
 JapaneseString are not supported as source strings.  String literals
 ('abc') are generated as instances of the class of sourceString,
 unless sourceString is a Symbol, in which case 'abc' produces a String.
 If sourceString is a DoubleByteSymbol, 'abc' produces a DoubleByteString.

 anEnvironmentId must be a SmallInteger >= 0 and <= 16rFFFF.
 0 denotes the base Smalltalk image.  1 was reserved for use by Ruby .

 If there are no errors, this adds the resulting compiled method to the
 receiver's method dictionary and returns that method,
 otherwise signals a CompileError .
 A CompileWarning may be signaled, after adding the new method
 to a receiver's method dictionary."

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		dictionaries: aSymbolList
		inProtocol: aCategoryString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		inPackageNamed: packageName
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwMoveClassToPackage: packageName [
	"Move class to <packageName>, whether or not it has been packaged. The methods in the class that are in the
		original package of the class are also moved to the new package. If the class was originally unpackaged,
		then only unpackaged methods (class and instance side) are moved to the new package."

	^ Rowan projectTools browser
		moveClassNamed: self thisClass name asString
		toPackage: packageName
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwMoveMethod: methodSelector toCategory: categoryName [

	^ Rowan projectTools browser
		moveMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		toProtocol: categoryName

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwMoveMethod: methodSelector toPackage: packageName [
	"Move the method into <packageName>, whether or not it has been packaged"

	^ Rowan projectTools browser
		moveMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		toPackage: packageName
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwRemoveCategory: categoryName [

	^ Rowan projectTools browser
		removeProtocol: categoryName 
		fromClassNamed:  self thisClass name asString
		isMeta: self isMeta

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwRemoveSelector: methodSelector [

	^ Rowan projectTools browser
		removeMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwUnpackageClass [
	"Unpackage the receiver and all methods in the class that in the same package, 
		while leaving the class installed in the image"

	^ Rowan projectTools browser unpackageClassNamed: self thisClass name asString
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwUnpackageMethod: methodSelector [
	"Move the method into <packageName>, whether or not it has been packaged"

	^ Rowan projectTools browser
		unpackageMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
]
