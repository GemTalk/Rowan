! Copyright (C) GemTalk Systems 1986-2024.  All Rights Reserved.
! Class extensions for 'AbstractDictionary'

!		Instance methods for 'AbstractDictionary'

category: '*rowan-gemstone-kernel-36x'
method: AbstractDictionary
at: key ifPresent: aBlock
   "Lookup the given key in the receiver. If it is present, answer the value of 
    evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [^ nil].
	^ aBlock cull: v
%

! Class extensions for 'Behavior'

!		Instance methods for 'Behavior'

category: '*rowan-gemstone-kernel'
method: Behavior
rowanPackageName

	"answer the name of the package that the receiver is a member of. Answer 'Rowan unpackagedName` if 
		the receiver is not a member of any package"

	| loadedClass |
	loadedClass := Rowan image
		loadedClassForClass: self thisClass
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedClass loadedPackage name
%

category: '*rowan-gemstone-kernel'
method: Behavior
rowanProjectName

	"answer the name of the project that the receiver is a member of. Answer `Rowan unpackagedName` if 
		the receiver is not a member of any project"

	| loadedClass |
	loadedClass := Rowan image
		loadedClassForClass: self thisClass
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedClass loadedProject name
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwCompileExtensionMethod: sourceString category: categoryName packageName: packageName
	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: categoryName
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		inPackageNamed: packageName
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwCompileExtensionMethod: sourceString package: aPackageName

	| aCategory |
	aCategory := '*', aPackageName asLowercase .
	^Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategory asString asLowercase
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwCompileMethod: sourceString category: aCategoryString

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString asString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwCompileMethod: sourceString category: aCategoryString packageName: packageName

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		inPackageNamed: packageName
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwCompileMethod: sourceString dictionaries: aSymbolList category: aCategoryString packageName: packageName
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
%

category: '*rowan-gemstone-35x'
method: Behavior
rwGuaranteePersistentMethodDictForEnv: envId
	"in 3.5, the method persistentMethodDictForEnv: DOES NOT always return a GsMethodDictionary,
		as classes are created without a GsMethodDictionary for envId 0."

	<primitive: 2001>
	| prot |
	prot := System _protectedMode .
	[ 
		| newDict |
		(self persistentMethodDictForEnv: envId) ifNotNil: [:oldDict | ^ oldDict ].
		newDict := GsMethodDictionary new.
		self persistentMethodDictForEnv: envId put: newDict.
		^ newDict ] 
		ensure:[ prot _leaveProtectedMode ].
%

category: '*rowan-gemstone-kernel-36x'
method: Behavior
rwMethodCategories
	^ self _unifiedCategorys: 0
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwMoveClassToPackage: packageName
	"Move class to <packageName>, whether or not it has been packaged. The methods in the class that are in the
		original package of the class are also moved to the new package. If the class was originally unpackaged,
		then only unpackaged methods (class and instance side) are moved to the new package."

	^ Rowan projectTools browser
		moveClassNamed: self thisClass name asString
		toPackage: packageName
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwMoveMethod: methodSelector toCategory: categoryName

	^ Rowan projectTools browser
		moveMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		toProtocol: categoryName
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwMoveMethod: methodSelector toPackage: packageName
	"Move the method into <packageName>, whether or not it has been packaged"

	^ Rowan projectTools browser
		moveMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		toPackage: packageName
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwRemoveCategory: categoryName

	^ Rowan projectTools browser
		removeProtocol: categoryName 
		fromClassNamed:  self thisClass name asString
		isMeta: self isMeta
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwRemoveSelector: methodSelector

	^ Rowan projectTools browser
		removeMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwUnpackageClass
	"Unpackage the receiver and all methods in the class that in the same package, 
		while leaving the class installed in the image"

	^ Rowan projectTools browser unpackageClassNamed: self thisClass name asString
%

category: '*rowan-gemstone-kernel'
method: Behavior
rwUnpackageMethod: methodSelector
	"Move the method into <packageName>, whether or not it has been packaged"

	^ Rowan projectTools browser
		unpackageMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
%

category: '*rowan-gemstone-35x'
method: Behavior
_constraintOn: aSymbol

"Returns the class kind constraint for the instance variable represented by
 aSymbol.  If aSymbol does not represent an instance variable of objects whose
 behavior is defined by the receiver, returns nil.
 If the instance variable aSymbol is not constrained, returns Object ."

| ivNams constrs |

ivNams := instVarNames .
constrs := constraints .
1 to: self instSize do: [ :i |
  aSymbol == (ivNams  at: i) ifTrue:[ ^ self _constraintAt: i ].
].
^ nil
%

category: '*rowan-gemstone-35x'
method: Behavior
_ivOffsetAndConstraint: aSymbol

"Searches the instVarNames instance variable of the receiver for an instance
 variable named aSymbol, and returns an Array containing the offset and the
 constraint for that instance variable.  Returns nil if no instance variable
 exists with the name aSymbol."

| idx |
idx := instVarNames indexOfIdentical: aSymbol .
idx == 0 ifTrue:[ ^ nil ].
^ { idx .  self _constraintAt: idx } 
%

category: '*rowan-gemstone-35x'
method: Behavior
_namedIvConstraintAtOffset: offset

"Returns the constraint, if any, on the named instance variable at the
 specified offset.  Returns Object if there is no such named instance variable,
 or if the instance variable at that offset is not constrained."

(offset > self instSize ) ifTrue:[ ^ Object ] .
^ self _constraintAt: offset 
%

category: '*rowan-gemstone-35x'
method: Behavior
_newConstraint: aClass atOffset: offset

"Execute the constraint change for Behavior | instvar:ConstraintTo:
 assuming all error and variance checks have been done."
| constrs |
self deprecated: 'Behavior>>_newConstraint:atOffset: deprecated, Constraints are no longer supported'.
self _validatePrivilege ifTrue:[ 
  (constrs := constraints) size == 0 ifTrue:[ | sz |
    aClass == Object ifTrue:[ ^ self "do nothing"].
    sz := self instSize .
    (constrs := Array new: sz ) replaceFrom: 1 to: sz withObject: Object.
    constraints := constrs .
  ].
  constrs at: offset put: aClass .
  (aClass == Object) ifFalse:[ self _setConstraintBit ].
  self _refreshClassCache: false .
]
%

category: '*rowan-gemstone-kernel'
method: Behavior
_rowanCopyMethodsAndVariablesFrom: sourceClass dictionaries: dicts

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
%

category: '*rowan-gemstone-kernel'
method: Behavior
_rwInstVar: aString constrainTo: aClass
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
%

category: '*rowan-gemstone-kernel'
method: Behavior
_rwNewConstraint: aClass atOffset: offset

	| oldConstraintsArray newConstraintsArray constraintsIndex instanceVariableNames |
	oldConstraintsArray := constraints.
	newConstraintsArray := oldConstraintsArray copy.
	instanceVariableNames := self class allInstVarNames.
	constraintsIndex := instanceVariableNames indexOf: #constraints.
	self _unsafeAt: constraintsIndex put: newConstraintsArray.

	[ self _newConstraint: aClass atOffset: offset ] on: Deprecated do: [:ex | ex resume ].

	constraints immediateInvariant
%

category: '*rowan-gemstone-kernel'
method: Behavior
_rwNewInheritedConstraint: aClass atOffset: offset

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
 
%

category: '*rowan-gemstone-35x'
method: Behavior
_setConstraintBit

"Sets the constraint bit in the 'format' instance variable of the receiver."

self deprecated: 'Behavior>>_setConstraintBit deprecated, Constraints are no longer supported'.
self _validatePrivilege ifTrue:[
  format := format bitOr: 16#10 .
]
%

category: '*rowan-gemstone-35x'
method: Behavior
_setVaryingConstraint: aClass

"Assign a new value to the constraint on unnamed variables of the receiver,
 assuming all checks have been made."

| constrs ofs |

self deprecated: 'Behavior>>_setVaryingConstraint: deprecated, Constraints are no longer supported'.
self _validatePrivilege ifTrue:[
  constrs := constraints .
  ofs := self instSize + 1 .
  constrs size == 0 ifTrue:[ 
    aClass == Object ifTrue:[ ^ self "nothing to do"].
    (constrs := Array new: ofs) replaceFrom: 1 to: ofs withObject: Object .
    constraints := constrs .
  ].
  constrs at: ofs put: aClass .
  (aClass == Object) ifFalse:[ self _setConstraintBit ].
  self _refreshClassCache: false .
]
%

! Class extensions for 'ByteArray'

!		Instance methods for 'ByteArray'

category: '*rowan-gemstone-kernel'
method: ByteArray
byteArrayMap

	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.  Intended for use by primitives only"

	| map |
	map := ByteArray new: 256 withAll: 0.
	self do: [ :ascii | map at: ascii + 1 put: 1 ].
	^ map
%

! Class extensions for 'CharacterCollection'

!		Instance methods for 'CharacterCollection'

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
asRwGemStoneVersionNumber

	^ RwGemStoneVersionNumber fromString: self
%

category: '*rowan-gemstone-kernel'
method: CharacterCollection
asRwRepository

	"return a platform appropriate repository for the repository identified in the receiver"

	^ self asRwUrl asRwRepository
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
asRwSemanticVersionNumber

	^ RwSemanticVersionNumber fromString: self
%

category: '*rowan-gemstone-url'
method: CharacterCollection
asRwUrl

	""

	^ RwUrl fromString: self
%

category: '*rowan-gemstone-url'
method: CharacterCollection
indexOfAnyOf: specialChars startingAt: oldPos

	oldPos to: self size do: [ :i | 
		(specialChars includes: (self at: i))
			ifTrue: [ ^ i ] ].
	^ 0
%

category: '*rowan-gemstone-kernel'
method: CharacterCollection
indexOfAnyOf: aByteArray startingAt: start ifAbsent: aBlock

	"returns the index of the first character in the given set, starting from start"

	| ans |
	ans := self class
		findFirstInString: self
		inSet: aByteArray asByteArray byteArrayMap
		startingAt: start.
	ans = 0
		ifTrue: [ ^ aBlock value ]
		ifFalse: [ ^ ans ]
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
rwPlatformAttributeMatchForGemStoneVersion: anRwGemStoneVersionConfigurationPlatformAttributeMatcher

	^ anRwGemStoneVersionConfigurationPlatformAttributeMatcher matchString: self
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
rwPlatformAttributeMatchForString: anRwStringConfigurationPlatformAttributeMatcher

	^ anRwStringConfigurationPlatformAttributeMatcher matchString: self
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
rwSemanticIntegerLessThanSelf: anInteger

	"integers have greater precedence than strings"
	"anInteger < aString-> true"

  ^ true
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
rwSemanticStringLessThanSelf: aString

	^ aString < self
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent

	^ aRwSemanticVersonComponent rwSemanticStringLessThanSelf: self
%

category: '*rowan-gemstone-url'
method: CharacterCollection
unescapePercents

	"change each %XY substring to the character with ASCII value XY in hex.  This is the opposite of #encodeForHTTP"

	| ans c asciiVal pos oldPos specialChars |
	ans _ WriteStream on: String new.
	oldPos _ 1.
	specialChars _ '+%'.

	[ 
	pos _ self indexOfAnyOf: specialChars startingAt: oldPos.
	pos > 0 ]
		whileTrue: [ 
			ans nextPutAll: (self copyFrom: oldPos to: pos - 1).
			c _ self at: pos.
			c = $+
				ifTrue: [ ans nextPut: $  ]
				ifFalse: [ 
					(c = $% and: [ pos + 2 <= self size ])
						ifTrue: [ 
							asciiVal _ ((self at: pos + 1) asUppercase digitValueInRadix: 16) * 16
								+ ((self at: pos + 2) asUppercase digitValueInRadix: 16).
							pos _ pos + 2.
							asciiVal > 255
								ifTrue: [ ^ self ].	"not really an escaped string"
							ans nextPut: (Character value: asciiVal) ]
						ifFalse: [ ans nextPut: c ] ].
			oldPos _ pos + 1 ].
	ans nextPutAll: (self copyFrom: oldPos to: self size).
	^ ans contents
%

category: '*rowan-gemstone-kernel'
method: CharacterCollection
withGemstoneLineEndings

	"assume the string is textual, and that CR, LF, and CRLF are all 
  valid line endings.  Replace each occurence with a single LF"

	| cr lf crlf inPos outPos outString lineEndPos newOutPos |
	cr := Character cr.
	lf := Character lf.
	crlf := ByteArray new.
	crlf
		add: cr asciiValue;
		add: lf asciiValue.

	inPos := 1.
	outPos := 1.
	outString := self class _newString: self size.

	[ 
	lineEndPos := self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [ 0 ].
	lineEndPos ~= 0 ]
		whileTrue: [ 
			newOutPos := outPos + (lineEndPos - inPos + 1).
			outString
				replaceFrom: outPos
				to: newOutPos - 2
				with: self
				startingAt: inPos.
			outString at: newOutPos - 1 put: lf.
			outPos := newOutPos.

			((self at: lineEndPos) = cr
				and: [ lineEndPos < self size and: [ (self at: lineEndPos + 1) = lf ] ])
				ifTrue: [ 
					"CRLF ending"
					inPos := lineEndPos + 2 ]
				ifFalse: [ 
					"CR or LF ending"
					inPos := lineEndPos + 1 ] ].	"no more line endings.  copy the rest"
	newOutPos := outPos + (self size - inPos + 1).
	outString
		replaceFrom: outPos
		to: newOutPos - 1
		with: self
		startingAt: inPos.

	^ outString copyFrom: 1 to: newOutPos - 1
%

category: '*rowan-gemstone-kernel'
method: CharacterCollection
withoutGemstoneLineEndings

	"assume the string is textual, and that CR, LF, and CRLF are all 
	valid line endings.  Remove each occurence. "

	| cr lf crlf inPos outPos outString lineEndPos newOutPos |
	cr := Character cr.
	lf := Character lf.
	crlf := ByteArray new.
	crlf
		add: cr asciiValue;
		add: lf asciiValue.

	inPos := 1.
	outPos := 1.
	outString := self class _newString: self size.

	[ 
	lineEndPos := self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [ 0 ].
	lineEndPos ~= 0 ]
		whileTrue: [ 
			newOutPos := outPos + (lineEndPos - inPos + 1).
			outString
				replaceFrom: outPos
				to: newOutPos - 2
				with: self
				startingAt: inPos.
			outPos := newOutPos - 1.

			((self at: lineEndPos) = cr
				and: [ lineEndPos < self size and: [ (self at: lineEndPos + 1) = lf ] ])
				ifTrue: [ 
					"CRLF ending"
					inPos := lineEndPos + 2 ]
				ifFalse: [ 
					"CR or LF ending"
					inPos := lineEndPos + 1 ] ].	"no more line endings.  copy the rest"
	newOutPos := outPos + (self size - inPos + 1).
	outString
		replaceFrom: outPos
		to: newOutPos - 1
		with: self
		startingAt: inPos.

	^ outString copyFrom: 1 to: newOutPos - 1
%

! Class extensions for 'Class'

!		Instance methods for 'Class'

category: '*rowan-gemstone-35x'
method: Class
indexableSubclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDict inDictionary: aDictionary newVersionOf: oldClass description: aDescription constraints: constraintsArray options: optionsArray

	| newClass |
	newClass := self indexableSubclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDict inDictionary: aDictionary newVersionOf: oldClass description: aDescription options: optionsArray.
	^ newClass
%

category: '*rowan-gemstone-kernel'
method: Class
rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName options: optionsArray

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'byteSubclass'
		superclass: self name asString
		instVarNames: #()
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aCategoryName
		options: optionsArray
%

category: '*rowan-gemstone-kernel'
method: Class
rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName  options: optionsArray

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
%

category: '*rowan-gemstone-kernel'
method: Class
rwCategory
	"Provide direct access to category of class."

	^ self category
%

category: '*rowan-gemstone-kernel'
method: Class
rwCategory: aString
	^ Rowan projectTools browser
		updateClassCategory: aString
		forClassNamed: self thisClass name asString
%

category: '*rowan-gemstone-kernel-36x'
method: Class
rwClassDefinitionInSymbolDictionaryNamed: symDictName
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
%

category: '*rowan-gemstone-kernel-36x'
method: Class
rwClassType
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
%

category: '*rowan-gemstone-kernel'
method: Class
rwComment

	"Provide direct access to comment of class, bypassing default comeent string."
  
  ^ (self _extraDictAt: #comment) ifNil: [ '' ]
%

category: '*rowan-gemstone-kernel'
method: Class
rwComment: aString

	^ Rowan projectTools browser
		updateClassComment: aString
		forClassNamed: self thisClass name asString
%

category: '*rowan-gemstone-kernel'
method: Class
rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName constraints: constraintArray options: optionsArray

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
%

category: '*rowan-gemstone-kernel'
method: Class
rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName options: optionsArray

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
%

category: '*rowan-gemstone-kernel'
method: Class
rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName constraints: constraintArray options: optionsArray

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
%

category: '*rowan-gemstone-kernel'
method: Class
rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName options: optionsArray

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
%

category: '*rowan-gemstone-kernel'
method: Class
rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName constraints: constraintArray options: optionsArray

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
%

category: '*rowan-gemstone-kernel'
method: Class
rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName options: optionsArray

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
%

category: '*rowan-gemstone-kernel'
method: Class
rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName constraints: constraintArray options: optionsArray

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
%

category: '*rowan-gemstone-kernel'
method: Class
rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName options: optionsArray

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
%

category: '*rowan-gemstone-kernel'
method: Class
rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary newVersionOf: oldClass category: aCategoryName packageName: aPackageName options: optionsArray
	| loadedPackage |
	loadedPackage := Rowan image
		loadedPackageNamed: aPackageName
		ifAbsent: [ self error: 'No loaded package found for ' , aPackageName printString ].

	aDictionary
		ifNotNil: [ 
			| expectedSymDictName specifiedSymDictName |
			(expectedSymDictName := loadedPackage loadedProject
				symbolDictNameForPackageNamed: aPackageName)
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
%

category: '*rowan-gemstone-35x'
method: Class
subclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary newVersionOf: oldClass description: aDescription constraints: theConstraints options: optionsArray
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
%

category: '*rowan-gemstone-35x'
method: Class
_constraintsEqual: anArray
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
%

category: '*rowan-gemstone-35x'
method: Class
_equivalentSubclass: oldClass superCls: actualSelf name: aString newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars inDict: aDictionary constraints: aConstraint isKernel: isKernelBool
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
%

category: '*rowan-gemstone-35x'
method: Class
_installConstraints: theConstraints

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
%

category: '*rowan-gemstone-35x'
method: Class
_installConstraints: theConstraints oldClass: oldClass

	oldClass ifNotNil: [ [ self _installOldConstraints: oldClass _constraints ] on: Deprecated do: [:ex | ex resume ] ].
	theConstraints 
		ifNil: [ constraints := nil ]
		ifNotNil: [ self _installConstraints: theConstraints ]
%

category: '*rowan-gemstone-35x'
method: Class
_installOldConstraints: theConstraints

	constraints := theConstraints copy
%

category: '*rowan-gemstone-kernel'
method: Class
_rwDefinitionOfConstraints
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
%

category: '*rowan-gemstone-kernel-36x'
method: Class
_rwOptionsArray
  ^ self _optionsArrayForDefinition
%

category: '*rowan-gemstone-kernel'
method: Class
_rwOptionsForDefinition 
  "copy of _optionsForDefinition"

  | result arr |
  result :=  'options: #(' copy .
  arr := self _rwOptionsArray .
  1 to: arr size do:[:j | result add: $ ; add: (arr at: j) ].
  result add: $)  .
  ^ result
%

category: '*rowan-gemstone-kernel-36x'
method: Class
_rwReservedOop
 "returns nil or the SmallInteger specifying a reserved oop"
  ^ self asOopNumber <= System _lastReservedOopNumber 
    ifTrue:[ self asOop ] 
    ifFalse:[ nil ].
%

category: '*rowan-gemstone-kernel'
method: Class
_rwSortedConstraints
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
%

! Class extensions for 'GsFileIn'

!		Instance methods for 'GsFileIn'

category: '*rowan-gemstone-kernel-36x'
method: GsFileIn
currentPackage

	^ Rowan gemstoneTools topaz currentTopazPackageName
%

category: '*rowan-gemstone-kernel-36x'
method: GsFileIn
currentPackage: aStringOrNil

	Rowan gemstoneTools topaz currentTopazPackageName: aStringOrNil
%

category: '*rowan-gemstone-kernel-36x'
method: GsFileIn
currentProject

	^ Rowan gemstoneTools topaz currentTopazProjectName
%

category: '*rowan-gemstone-kernel-36x'
method: GsFileIn
currentProject: aStringOrNil

	Rowan gemstoneTools topaz currentTopazProjectName: aStringOrNil
%

! Class extensions for 'GsNMethod'

!		Instance methods for 'GsNMethod'

category: '*rowan-gemstone-kernel'
method: GsNMethod
rowanPackageName

	"answer the name of the package that the receiver is a member of. Answer `Rowan unpackagedName` if 
		the receiver is not a member of any package"

	| loadedMethod |
	loadedMethod := Rowan image
		loadedMethodForMethod: self
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedMethod loadedPackage name
%

category: '*rowan-gemstone-kernel'
method: GsNMethod
rowanProjectName

	"answer the name of the project that the receiver is a member of. Answer `Rowan unpackagedName` if 
		the receiver is not a member of any project"

	| loadedMethod |
	loadedMethod := Rowan image
		loadedMethodForMethod: self
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedMethod loadedProject name
%

! Class extensions for 'Integer'

!		Instance methods for 'Integer'

category: '*rowan-gemstone-components-kernel'
method: Integer
rwSemanticIntegerLessThanSelf: anInteger

	^ anInteger < self
%

category: '*rowan-gemstone-components-kernel'
method: Integer
rwSemanticStringLessThanSelf:  aString
  "integers have greater precedence than strings"
	" aString < anInteger -> false"

  ^ false
%

category: '*rowan-gemstone-components-kernel'
method: Integer
rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent

	^ aRwSemanticVersonComponent rwSemanticIntegerLessThanSelf: self
%

! Class extensions for 'Interval'

!		Class methods for 'Interval'

category: '*Rowan-GemStone-Kernel'
classmethod: Interval
fromSton: stonReader
	| result values |
	values := OrderedCollection new.
	stonReader parseMapDo: [:key :value | values add: value].
	values size = 2 ifTrue: [result := self from: values first to: values second].
	values size = 3 ifTrue: [result := self from: values first to: values second by: values third].
	^result
%

!		Instance methods for 'Interval'

category: '*Rowan-GemStone-Kernel'
method: Interval
stonOn: stonWriter
  stonWriter
    writeObject: self
    streamMap: [ :dictionary | 
      dictionary
        at: #'start' put: from;
        at: #'stop' put: to;
        at: #'step' put: by ]
%

! Class extensions for 'Object'

!		Instance methods for 'Object'

category: '*rowan-gemstone-components-kernel'
method: Object
rwPlatformAttributeMatchForGemStoneVersion: anRwGemStoneVersionConfigurationPlatformAttributeMatcher
  ^ self
    error: 'Expected a String or a RwGemStoneVersion'
%

category: '*rowan-gemstone-components-kernel'
method: Object
rwPlatformAttributeMatchForString: anRwStringConfigurationPlatformAttributeMatcher
  ^ self
    error: 'Expected a String or a RwGemStoneVersion'
%

category: '*rowan-gemstone-components-kernel'
method: Object
rwSemanticIntegerLessThanSelf: anInteger
  ^ self
    error: 'Invalid semantic verson component - should be an Integer.'
%

category: '*rowan-gemstone-components-kernel'
method: Object
rwSemanticStringLessThanSelf: aString
  ^ self
    error: 'Invalid semantic verson component - should be String.'
%

category: '*rowan-gemstone-components-kernel'
method: Object
rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent
  ^ self
    error: 'Invalid semantic verson component - should be String or Integer.'
%

! Class extensions for 'SequenceableCollection'

!		Class methods for 'SequenceableCollection'

category: '*rowan-gemstone-kernel'
classmethod: SequenceableCollection
new: size withAll: value

	"Answer an instance of me, with number of elements equal to size, each 
	of which refers to the argument, value."

	^ (self new: size)
		atAllPut: value;
		yourself
%

!		Instance methods for 'SequenceableCollection'

category: '*rowan-gemstone-kernel'
method: SequenceableCollection
copyUpTo: anObject

	"Answer all elements up to but not including anObject. If there
  is no such object, answer a copy of the receiver."

	| idx |
	idx := self indexOf: anObject startingAt: 1.
	idx == 0
		ifTrue: [ ^ self copy ]
		ifFalse: [ ^ self copyFrom: 1 to: idx - 1 ]
%

! Class extensions for 'SymbolDictionary'

!		Instance methods for 'SymbolDictionary'

category: '*rowan-gemstone-kernel'
method: SymbolDictionary
rowanSymbolDictionaryRegistry

	"answer the RwGsSymbolDictionaryRegistry instance installed in the receiver, otherwise answer nil"

	^ self at: #'RwSymbolDictionaryRegistry' ifAbsent: [  ]
%

category: '*rowan-gemstone-kernel-extensions-36x'
method: SymbolDictionary
_rowanCloneSymbolDictionaryNamed: aSymbol symbolList: symbolList

	"return a new symbol dictionary containing copies of all of the classes in the receiver ... the state of each class should be the same with 
		respect to class intance variables and class variables ... similar to what happens when a new version of a class is created, except the
		copied classes are not in the class history of the original class"

	"it is expected that the receiver is in the <symbolList>"

	| toBeOrdered order clonedSymDict processed aClass clonedClasses |
    "order the list of classes in the receiver in superclass order, so that superclasses are cloned before the subclasses are cloned"
	toBeOrdered := (self values select: [:each | each isBehavior ]) asIdentitySet.
	order := OrderedCollection new.
	processed := IdentitySet new.
	[ (aClass := RwGsPatchSet_V2 _anyElementOf: toBeOrdered ifEmpty: [ nil ]) isNil ]
		whileFalse: [ 
			RwGsPatchSet_V2
				_orderBySuperclass: aClass
				from: toBeOrdered
				into: order
				ignoring: processed ].
	"create clone and clone all of the classes before compiling methods"
	clonedSymDict := self class new.
	clonedSymDict name: aSymbol.
	clonedClasses := OrderedCollection new.
	[
		symbolList add: clonedSymDict before: self. "install clone after receiver, so that methods will be compiled and reference the cloned classes"
		"clone the body of classes before compiling methods"
		order do: [:oldClass |
			| oldClassName clonedClass hist superclass |
			oldClassName := oldClass name asSymbol.
			hist := oldClass classHistory copy.	"make copy ... leverage the new class version behavior, which preserves a lot of state, without 
																adding the new class to old class' class hitory"
			superclass := symbolList objectNamed: oldClass superclass name. "look up superclass, in case superclass has been cloned"
			clonedClass :=  superclass
				_subclass: oldClassName asString 
				instVarNames: oldClass instVarNames
				format: oldClass format  
				classVars: oldClass classVarNames
				classInstVars: oldClass class instVarNames
				poolDictionaries: #()
				inDictionary: nil
				inClassHistory: hist
				description: nil
				options: oldClass _optionsArrayForDefinition .
			clonedSymDict at: oldClassName put: clonedClass.
			clonedClasses add: {clonedClass. oldClass} ].
			"compile methods in cloned class"
			clonedClasses do: [:ar | | x clonedClass oldClass |
				clonedClass := ar at: 1.
				oldClass := ar at: 2.
				(x := clonedClass _rowanCopyMethodsAndVariablesFrom: oldClass dictionaries: symbolList)
					isEmpty 
						ifFalse: [ self error: 'failed to compile methods in the cloned class ', oldClass name asString printString ] ] ] 
		ensure: [ 
			"do not leave clone in the symbol list"
			symbolList remove: clonedSymDict ifAbsent: [] ].
	^ clonedSymDict
%

! Class extensions for 'UndefinedObject'

!		Instance methods for 'UndefinedObject'

category: '*rowan-gemstone-kernel'
method: UndefinedObject
rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName constraints: constraintArray options: optionsArray

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'normal'
		superclass: 'nil'
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		category: aCategoryName
		packageName: aPackageName
		constraints: constraintArray
		options: optionsArray
%

category: '*rowan-gemstone-kernel'
method: UndefinedObject
subclass: aString 
instVarNames: anArrayOfInstvarNames 
classVars: anArrayOfClassVars 
classInstVars: anArrayOfClassInstVars 
poolDictionaries: anArrayOfPoolDicts 
inDictionary: aDictionary 
newVersionOf: oldClass 
description: aDescription 
constraints: constraintsArray
options: optionsArray

  | descr theClass superClassOffset |
  descr := aDescription.
  oldClass ifNotNil: [ 
     (Object _equivalentSubclass: oldClass
        superCls: self
        name: aString
        newOpts: optionsArray
        newFormat: oldClass format
        newInstVars: anArrayOfInstvarNames
        newClassInstVars: anArrayOfClassInstVars
        newPools: anArrayOfPoolDicts
        newClassVars: anArrayOfClassVars
        inDict: aDictionary
        constraints: constraintsArray isKernel: false ) ifTrue: [
            oldClass _commentOrDescription: aDescription.
            ^oldClass "avoid creation of a new version"
        ].
      descr ifNil: [descr := oldClass commentForFileout]
  ].
  theClass := Object
        subclass: aString
        instVarNames: anArrayOfInstvarNames
        classVars: anArrayOfClassVars
        classInstVars: anArrayOfClassInstVars
        poolDictionaries: anArrayOfPoolDicts
        inDictionary: aDictionary
        newVersionOf: oldClass
        description: descr
		constraints: constraintsArray
        options: optionsArray.
  theClass == oldClass
    ifFalse:
      [superClassOffset := Behavior _ivOffsetOf: #superClass.
      theClass _unsafeAt: superClassOffset put: nil.
      theClass class _unsafeAt: superClassOffset put: Object class superClass].
  ^theClass
%


commit
