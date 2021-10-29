! Class Declarations
! Generated file, do not Edit

doit
(Object
	subclass: 'GsTonelOrderedDictionary'
	instVarNames: #( size keys values )
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #( #logCreation )
)
		category: 'Tonel-GemStoneCommon-Core';
		comment: 'I am an implementation of a dictionary. Compared to other dictionaries I am very efficient for small sizes, speed- and space-wise. I also mantain the order in which elements are added when iterating. My implementation features some ideas from the RefactoringBrowser.';
		immediateInvariant.
true.
%

removeallmethods GsTonelOrderedDictionary
removeallclassmethods GsTonelOrderedDictionary

! Class implementation for 'GsTonelOrderedDictionary'

!		Class methods for 'GsTonelOrderedDictionary'

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
new
	^ self new: 3
%

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
new: anInteger
	^ self basicNew initialize: anInteger; yourself
%

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
withAll: aDictionary
	^ (self new: aDictionary size)
		addAll: aDictionary;
		yourself
%

!		Instance methods for 'GsTonelOrderedDictionary'

category: 'accessing'
method: GsTonelOrderedDictionary
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation
%

category: 'adding'
method: GsTonelOrderedDictionary
addAll: aDictionary
	aDictionary keysAndValuesDo: [ :key :value | self at: key put: value ].
	^ aDictionary
%

category: 'accessing'
method: GsTonelOrderedDictionary
associations
	"Answer a Collection containing the receiver's associations."

	| result |
	result := WriteStream on: (Array new: self size).
	self associationsDo: [ :assoc | result nextPut: assoc ].
	^ result contents
%

category: 'enumerating'
method: GsTonelOrderedDictionary
associationsDo: aBlock
	self keysAndValuesDo: [ :key :value | aBlock value: key -> value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey
	"Answer the value associated with aKey. Raise an exception, if no such key is defined."

	^ self at: aKey ifAbsent: [ self errorKeyNotFound ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifAbsent: aBlock
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ aBlock value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifAbsentPut: aBlock
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined and store the return value."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ self privateAt: aKey put: aBlock value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifPresent: aBlock
	"Lookup aKey in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0 ifFalse: [ aBlock value: (values at: index) ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey put: aValue
	"Set the value of aKey to be aValue."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index put: aValue ]
		ifTrue: [ self privateAt: aKey put: aValue ]
%

category: 'enumerating'
method: GsTonelOrderedDictionary
do: aBlock
	1 to: size do: [ :index | aBlock value: (values at: index) ]
%

category: 'private'
method: GsTonelOrderedDictionary
errorKeyNotFound
	self error: 'Key not found'
%

category: 'private'
method: GsTonelOrderedDictionary
findIndexFor: aKey
	1 to: size do: [ :index |
		(keys at: index) = aKey
			ifTrue: [ ^ index ] ].
	^ 0
%

category: 'private'
method: GsTonelOrderedDictionary
grow
	| newKeys newValues |
	newKeys := Array new: 2 * size.
	newValues := Array new: 2 * size.
	1 to: size do: [ :index |
		newKeys at: index put: (keys at: index).
		newValues at: index put: (values at: index) ].
	keys := newKeys.
	values := newValues
%

category: 'testing'
method: GsTonelOrderedDictionary
includesKey: aKey
	"Answer whether the receiver has a key equal to aKey."

	^ (self findIndexFor: aKey) ~= 0
%

category: 'initialization'
method: GsTonelOrderedDictionary
initialize: anInteger
  size := 0.
  keys := Array new: anInteger.
  values := Array new: anInteger
%

category: 'testing'
method: GsTonelOrderedDictionary
isCollection
	^ true
%

category: 'testing'
method: GsTonelOrderedDictionary
isEmpty
	^ size = 0
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keys
	^ keys copyFrom: 1 to: size
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keysAndValuesDo: aBlock
	1 to: size do: [ :index | aBlock value: (keys at: index) value: (values at: index) ]
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keysDo: aBlock
	1 to: size do: [ :each | aBlock value: (keys at: each) ]
%

category: 'copying'
method: GsTonelOrderedDictionary
postCopy
	super postCopy.
	keys := keys copy.
	values := values copy
%

category: 'printing'
method: GsTonelOrderedDictionary
printOn: aStream
	super printOn: aStream.
	
	aStream nextPut: $(.
	self size <= 100
		ifTrue: [
			| first |
			first := true.
			self keysAndValuesDo: [ :key :value |
				"keysAndValuesDo:separatedBy: would be nice"
				first
					ifTrue: [ first := false ]
					ifFalse: [ aStream space ].
				aStream
					print: key;
					nextPutAll: '->';				
					print: value ] ]
		ifFalse: [
			aStream
				nextPutAll: 'size ';
				print: self size ].
	aStream nextPut: $)	
%

category: 'private'
method: GsTonelOrderedDictionary
privateAt: aKey put: aValue
	size = keys size ifTrue: [ self grow ].
	keys at: (size := size + 1) put: aKey.
	^ values at: size put: aValue
%

category: 'private'
method: GsTonelOrderedDictionary
removeIndex: index
	| value |
	value := values at: index.
	index to: size - 1 do:
			[ :i | 
			keys at: i put: (keys at: i + 1).
			values at: i put: (values at: i + 1) ].
	keys at: size put: nil.
	values at: size put: nil.
	size := size - 1.
	^ value
%

category: 'accessing'
method: GsTonelOrderedDictionary
removeKey: aKey
	"Remove aKey from the receiver, raise an exception if the element is missing."

	^ self removeKey: aKey ifAbsent: [ self errorKeyNotFound ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
removeKey: aKey ifAbsent: aBlock
	"Remove aKey from the receiver, evaluate aBlock if the element is missing."

	| index |
	index := self findIndexFor: aKey.
	index = 0 ifTrue: [ ^ aBlock value ].
	^ self removeIndex: index
%

category: 'accessing'
method: GsTonelOrderedDictionary
size
	^ size
%

category: 'ston'
method: GsTonelOrderedDictionary
stonOn: stonWriter
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
    stonWriter encodeMap: self
%

category: 'enumerating'
method: GsTonelOrderedDictionary
values
	^ values copyFrom: 1 to: size
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

	| aCategory |
	
	categoryName first == $* ifFalse: [self error: 'Extension category must  contain * as first character'].
	(aCategory isEquivalent: '*', packageName asLowercase) ifFalse: [self error: 'Extension category name must match lowercased name of Rowan package'].
	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategory asString asLowercase
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

category: '*rowan-gemstone-kernel-extensions-36x'
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

category: '*rowan-gemstone-kernel-extensions-36x'
method: Behavior
rwMethodCategories
	^ self _unifiedCategorys: 0
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

category: '*rowan-gemstone-kernel-extensions-36x'
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

category: '*rowan-gemstone-kernel-extensions-36x'
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

category: '*rowan-gemstone-kernel-extensions-36x'
method: Behavior
_namedIvConstraintAtOffset: offset

"Returns the constraint, if any, on the named instance variable at the
 specified offset.  Returns Object if there is no such named instance variable,
 or if the instance variable at that offset is not constrained."

(offset > self instSize ) ifTrue:[ ^ Object ] .
^ self _constraintAt: offset 
%

category: '*rowan-gemstone-kernel-extensions-36x'
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

category: '*rowan-gemstone-kernel-extensions-36x'
method: Behavior
_setConstraintBit

"Sets the constraint bit in the 'format' instance variable of the receiver."

self deprecated: 'Behavior>>_setConstraintBit deprecated, Constraints are no longer supported'.
self _validatePrivilege ifTrue:[
  format := format bitOr: 16#10 .
]
%

category: '*rowan-gemstone-kernel-extensions-36x'
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

category: '*rowan-gemstone-kernel'
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

category: '*rowan-gemstone-kernel'
method: CharacterCollection
asRwSemanticVersionNumber

	^ RwSemanticVersionNumber fromString: self
%

category: '*rowan-gemstone-kernel'
method: CharacterCollection
asRwUrl

	""

	^ RwUrl fromString: self
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
endsWith: suffix

	"Answer whether the tail end of the receiver is the same as suffix.
	 The comparison is case-sensitive."

	| ofs |
	suffix size == 0
		ifTrue: [ ^ false ].
	(ofs := self size - suffix size) < 0
		ifTrue: [ ^ false ].
	^ self at: ofs + 1 equals: suffix	"
  'Elvis' endsWith: 'vis'
  'Elvis' endsWith: ''
"
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
findString: subString startingAt: startIndex caseSensitive: aBoolean

	^ self _findString: subString startingAt: startIndex ignoreCase: aBoolean not
%

category: '*rowan-gemstone-kernel'
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

category: '*tonel-gemstonecommon-core'
method: CharacterCollection
join: aCollection 
	"'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "
	^ self class new: (aCollection size * self size) streamContents: [:stream | 
			aCollection
				do: [:each | stream nextPutAll: each asString] 
				separatedBy: [stream nextPutAll: self]]
%

category: '*tonel-gemstonecommon-core'
method: CharacterCollection
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| cr lf start sz nextLF nextCR |
	start := 1.
	sz := self size.
	cr := Character cr.
	nextCR := self indexOf: cr startingAt: 1.
	lf := Character lf.
	nextLF := self indexOf: lf startingAt: 1.
	[ start <= sz ] whileTrue: [
		(nextLF = 0 and: [ nextCR = 0 ])
			ifTrue: [ "No more CR, nor LF, the string is over"
					aBlock value: start value: sz value: sz.
					^self ].
		(nextCR = 0 or: [ 0 < nextLF and: [ nextLF < nextCR ] ])
			ifTrue: [ "Found a LF"
					aBlock value: start value: nextLF - 1 value: nextLF.
					start := 1 + nextLF.
					nextLF := self indexOf: lf startingAt: start ]
			ifFalse: [ 1 + nextCR = nextLF
				ifTrue: [ "Found a CR-LF pair"
					aBlock value: start value: nextCR - 1 value: nextLF.
					start := 1 + nextLF.
					nextCR := self indexOf: cr startingAt: start.
					nextLF := self indexOf: lf startingAt: start ]
				ifFalse: [ "Found a CR"
					aBlock value: start value: nextCR - 1 value: nextCR.
					start := 1 + nextCR.
					nextCR := self indexOf: cr startingAt: start ]]]
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
putOn: aStream

	^ aStream nextPutAll: self
%

category: '*rowan-gemstone-kernel'
method: CharacterCollection
rwPlatformAttributeMatchForGemStoneVersion: anRwGemStoneVersionConfigurationPlatformAttributeMatcher

	^ anRwGemStoneVersionConfigurationPlatformAttributeMatcher matchString: self
%

category: '*rowan-gemstone-kernel'
method: CharacterCollection
rwPlatformAttributeMatchForString: anRwStringConfigurationPlatformAttributeMatcher

	^ anRwStringConfigurationPlatformAttributeMatcher matchString: self
%

category: '*rowan-gemstone-kernel'
method: CharacterCollection
rwSemanticIntegerLessThanSelf: anInteger

	"integers have greater precedence than strings"
	"anInteger < aString-> true"

  ^ true
%

category: '*rowan-gemstone-kernel'
method: CharacterCollection
rwSemanticStringLessThanSelf: aString

	^ aString < self
%

category: '*rowan-gemstone-kernel'
method: CharacterCollection
rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent

	^ aRwSemanticVersonComponent rwSemanticStringLessThanSelf: self
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
substrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| result sourceStream subStringStream |
	
	(separators isString or: [ separators allSatisfy: [ :element | element isCharacter ] ])
		ifFalse: [ ^ self error: 'separators must be Characters.' ].
	sourceStream := self readStream.
	result := OrderedCollection new.
	subStringStream := String new writeStreamPortable.
	[ sourceStream atEnd ] whileFalse: [
		| char |
		char := sourceStream next.
		(separators includes: char)
			ifTrue: [
				subStringStream isEmpty ifFalse: [
					result add: subStringStream contents.
					subStringStream := String new writeStreamPortable ] ]
			ifFalse: [
				subStringStream nextPut: char ] ].
	subStringStream isEmpty ifFalse: [
		result add: subStringStream contents ].
	^ result asArray
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimBoth

	"Trim separators from both sides of the receiving string."

	^ self trimBoth: [ :char | char isSeparator ]
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimBoth: aBlock

	"Trim characters satisfying the condition given in aBlock from both sides of the receiving string."

	^ self trimLeft: aBlock right: aBlock
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimLeft

	"Trim separators from the left side of the receiving string."

	^ self trimLeft: [ :char | char isSeparator ]
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimLeft: aBlock

	"Trim characters satisfying the condition given in aBlock from the left side of the receiving string."

	^ self trimLeft: aBlock right: [ :char | false ]
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimLeft: aLeftBlock right: aRightBlock

	"Trim characters satisfying the condition given in aLeftBlock from the left side and aRightBlock from the right sides of the receiving string."

	| left right |
	left := 1.
	right := self size.
	[ left <= right and: [ aLeftBlock value: (self at: left) ] ]
		whileTrue: [ left := left + 1 ].
	[ left <= right and: [ aRightBlock value: (self at: right) ] ]
		whileTrue: [ right := right - 1 ].
	^ self copyFrom: left to: right
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimRight

	"Trim separators from the right side of the receiving string."

	^ self trimRight: [ :char | char isSeparator ]
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimRight: aBlock

	"Trim characters satisfying the condition given in aBlock from the right side of the receiving string."

	^ self trimLeft: [ :char | false ] right: aBlock
%

category: '*rowan-gemstone-kernel'
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

category: '*tonel-gemstonecommon-core'
method: CharacterCollection
withLineEndings: lineEndingString

	| stream |
	
	stream := nil.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		(stream isNil and: [ endWithoutDelimiters ~= end ]) ifTrue: [
			(self copyFrom: endWithoutDelimiters + 1 to: end) = lineEndingString ifFalse: [
				stream := WriteStreamPortable with: self copy.
				stream position: start - 1 ]].
		stream ifNotNil: [
			stream next: endWithoutDelimiters - start + 1 putAll: self startingAt: start.
			endWithoutDelimiters = end ifFalse: [
				stream nextPutAll: lineEndingString ]]].
	^stream
		ifNil: [ self ]
		ifNotNil: [ 
			stream position = self size
				ifTrue: [ stream originalContents ]
				ifFalse: [ stream contents ]]
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

category: '*rowan-gemstone-kernel-extensions-36x'
method: Class
indexableSubclass: aString
instVarNames: anArrayOfInstvarNames
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
newVersionOf: oldClass
description: aDescription
constraints: constraintsArray
options: optionsArray
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

%

category: '*rowan-gemstone-kernel'
method: Class
rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName options: optionsArray

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
%

category: '*rowan-gemstone-kernel'
method: Class
rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: aCategoryName packageName: aPackageName  options: optionsArray

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

category: '*rowan-gemstone-kernel-extensions-36x'
method: Class
subclass: aString
instVarNames: anArrayOfInstvarNames
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
constraints: aConstraint
options: optionsArr

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
%

category: '*rowan-gemstone-kernel-extensions-36x'
method: Class
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
%

category: '*rowan-gemstone-kernel-extensions-36x'
method: Class
_checkConstraints: aConstraint instVarNames: anArrayOfInstvarNames
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
%

category: '*rowan-gemstone-kernel-extensions-36x'
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

category: '*rowan-gemstone-kernel-extensions-36x'
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

category: '*rowan-gemstone-kernel-extensions-36x'
method: Class
_installConstraints: theConstraints

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
%

category: '*rowan-gemstone-kernel-extensions-36x'
method: Class
_installConstraints: theConstraints oldClass: oldClass

	oldClass ifNotNil: [ [ self _installOldConstraints: oldClass _constraints ] on: Deprecated do: [:ex | ex resume ] ].
	theConstraints 
		ifNil: [ constraints := nil ]
		ifNotNil: [ self _installConstraints: theConstraints ]
%

category: '*rowan-gemstone-kernel-extensions-36x'
method: Class
_installOldConstraints: theConstraints

	constraints := theConstraints copy
%

category: '*rowan-gemstone-kernel-extensions-36x'
method: Class
_primSubclass: nameSym
instVarNames: arrayOfIvNames
format: anInteger
constraints: aConstraint
classVars: aSymbolDictionary
poolDictionaries: anArrayOfPoolDicts
classInstanceVars: arrayOfCivNames

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

category: '*rowan-gemstone-kernel'
method: Class
_rwOptionsArray
  "copy of _optionsArray"

  | result optCount | 
  result := { } .
  optCount := 0 .
  self instancesDbTransient ifTrue:[ result add: #dbTransient . optCount := optCount + 1 ].
  self instancesNonPersistent ifTrue:[ result add:  #instancesNonPersistent  . optCount := optCount + 1 ].
  self instancesInvariant ifTrue:[ result add:  #instancesInvariant  . optCount := optCount + 1 ].
  optCount > 1 ifTrue:[
    self _error: #classErrBadFormat
        with:'only one of #dbTransient #instancesNonPersistent  #instancesInvariant allowed' .
  ].
  "self _structuralUpdatesDisallowed ifTrue:[ result add: #disallowGciStore  ]." "commented out variant of _optionsArray (https://github.com/dalehenrich/Rowan/issues/292)"
  self isModifiable ifTrue:[ result add: #modifiable  ].
  self subclassesDisallowed ifTrue:[ result add: #subclassesDisallowed  ].
  "self _traversalByCallback ifTrue:[ result add: #traverseByCallback  ]." "commented out variant of _optionsArray (https://github.com/dalehenrich/Rowan/issues/292)"
  ^ result
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

category: '*rowan-gemstone-kernel-extensions-36x'
method: Class
_subclass: className instVarNames: anArrayOfInstvarNames format: theFormat constraints: theConstraints classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary inClassHistory: aClassHistory description: aDescription options: optionsArray

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
%

category: '*rowan-gemstone-kernel-extensions-36x'
method: Class
_subclass: nameSym
instVarNames: arrayOfIvNames
format: anInteger
constraints: aConstraint
classVars: aSymbolDictionary
poolDictionaries: anArrayOfPoolDicts
classInstanceVars: arrayOfCivNames

self _validateNewClassName: nameSym .
^ self _primSubclass: nameSym
  instVarNames: arrayOfIvNames
  format: anInteger
  constraints: aConstraint
  classVars: aSymbolDictionary
  poolDictionaries: anArrayOfPoolDicts
  classInstanceVars: arrayOfCivNames
%

! Class extensions for 'Collection'

!		Instance methods for 'Collection'

category: '*tonel-gemstonecommon-core'
method: Collection
asDictionary

  | dict |
  dict := Dictionary new.
  self do: [:assoc |
    dict add: assoc].
  ^ dict
%

category: '*tonel-gemstonecommon-core'
method: Collection
flattened
	
	"Flattens a collection of collections (no matter how many levels of collections exist).
	Strings are considered atoms and, as such, won't be flattened
	
	Examples:
	#(1 #(2 3) #(4 (#5))) flattened returns #(1 2 3 4 5) 
	#('string1' #('string2' 'string3')) flattened returns #('string1' 'string2' 'string3')"
	
	^ Array streamContents: [ :stream | self flattenOn: stream].
%

category: '*tonel-gemstonecommon-core'
method: Collection
flattenOn: aStream

	self do: [ :each | (each isCollection and: [each isString not]) 
						ifTrue: [each flattenOn: aStream]
						ifFalse: [aStream nextPut: each]].
%

category: '*tonel-gemstone-kernel'
method: Collection
ifNotEmpty: aBlock

	^ self size == 0
		ifFalse: [ aBlock cull: self ]
%

category: '*tonel-gemstone-kernel'
method: Collection
isCollection

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ true
%

category: '*tonel-gemstone-kernel'
method: Collection
select: selectBlock thenDo: doBlock
  "Utility method to improve readability."

  ^ (self select: selectBlock) do: doBlock
%

category: '*tonel-gemstone-kernel'
method: Collection
sort: aSortBlock

	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	^ self sortWithBlock: aSortBlock
%

! Class extensions for 'GsFile'

!		Instance methods for 'GsFile'

category: '*tonel-gemstonecommon-core'
method: GsFile
<< items

 	items putOn: self.
	
	^ self
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

category: '*rowan-gemstone-kernel'
method: Integer
rwSemanticIntegerLessThanSelf: anInteger

	^ anInteger < self
%

category: '*rowan-gemstone-kernel'
method: Integer
rwSemanticStringLessThanSelf:  aString
  "integers have greater precedence than strings"
	" aString < anInteger -> false"

  ^ false
%

category: '*rowan-gemstone-kernel'
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

category: '*tonel-gemstone-kernel'
method: Object
isCollection

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ false
%

category: '*tonel-gemstone-kernel'
method: Object
putOn: aStream

	^ aStream nextPut: self
%

category: '*rowan-gemstone-kernel'
method: Object
rwPlatformAttributeMatchForGemStoneVersion: anRwGemStoneVersionConfigurationPlatformAttributeMatcher
  ^ self
    error: 'Expected a String or a RwGemStoneVersion'
%

category: '*rowan-gemstone-kernel'
method: Object
rwPlatformAttributeMatchForString: anRwStringConfigurationPlatformAttributeMatcher
  ^ self
    error: 'Expected a String or a RwGemStoneVersion'
%

category: '*rowan-gemstone-kernel'
method: Object
rwSemanticIntegerLessThanSelf: anInteger
  ^ self
    error: 'Invalid semantic verson component - should be an Integer.'
%

category: '*rowan-gemstone-kernel'
method: Object
rwSemanticStringLessThanSelf: aString
  ^ self
    error: 'Invalid semantic verson component - should be String.'
%

category: '*rowan-gemstone-kernel'
method: Object
rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent
  ^ self
    error: 'Invalid semantic verson component - should be String or Integer.'
%

! Class extensions for 'PositionableStreamPortable'

!		Instance methods for 'PositionableStreamPortable'

category: '*tonel-gemstonecommon-core'
method: PositionableStreamPortable
match: subCollection
  "Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."

  | pattern startMatch |
  pattern := ReadStreamPortable on: subCollection.
  startMatch := nil.
  [ pattern atEnd ]
    whileFalse: [ 
      self atEnd
        ifTrue: [ ^ false ].
      self next = pattern next
        ifTrue: [ 
          pattern position = 1
            ifTrue: [ startMatch := self position ] ]
        ifFalse: [ 
          pattern position: 0.
          startMatch
            ifNotNil: [ 
              self position: startMatch.
              startMatch := nil ] ] ].
  ^ true
%

category: '*tonel-gemstonecommon-core'
method: PositionableStreamPortable
originalContents
	"Answer the receiver's actual contents collection, NOT a copy.  1/29/96 sw"

	^ collection
%

! Class extensions for 'RwAbstractClassDefinition'

!		Instance methods for 'RwAbstractClassDefinition'

category: '*rowan-gemstone-definitions'
method: RwAbstractClassDefinition
modificationForcingNewVersion

	^ self _modificationForcingNewClassVersion before: self after: self
%

! Class extensions for 'RwClassDefinition'

!		Instance methods for 'RwClassDefinition'

category: '*rowan-gemstone-definitions'
method: RwClassDefinition
compareAgainstBaseForcingNewClassVersion: aDefinition

	"all unchanged and unremoved methods need to be applied to the patch"

	| modification instanceMethodsModification classMethodsModification |
	modification := self _modificationForcingNewClassVersion
		before: aDefinition
		after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	instanceMethodsModification := self _methodsModificationClass
		extendedClassName: self key.
	classMethodsModification := self _methodsModificationClass
		extendedClassName: self key.
	self
		compareDictionary: instanceMethodDefinitions
		againstBaseDictionaryForNewClassVersion: aDefinition instanceMethodDefinitions
		into: instanceMethodsModification
		elementClass: RwMethodDefinition
		isMeta: false.
	self
		compareDictionary: classMethodDefinitions
		againstBaseDictionaryForNewClassVersion: aDefinition classMethodDefinitions
		into: classMethodsModification
		elementClass: RwMethodDefinition
		isMeta: true.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^ modification
%

category: '*rowan-gemstone-definitions'
method: RwClassDefinition
gs_constraints

	^self properties
				at: 'gs_constraints'
				ifAbsent: [ #() ]
%

category: '*rowan-gemstone-definitions'
method: RwClassDefinition
gs_constraints: constraintsArray

	(constraintsArray _isArray and: [ constraintsArray isEmpty not ])
		ifTrue: [ 
			self properties
				at: 'gs_constraints'
				put: constraintsArray ]
%

category: '*rowan-gemstone-definitions'
method: RwClassDefinition
gs_options

	^self properties
				at: 'gs_options'
				ifAbsent: [ #() ]
%

category: '*rowan-gemstone-definitions'
method: RwClassDefinition
gs_options: optionsArray

	(optionsArray _isArray and: [ optionsArray isEmpty not ])
		ifTrue: [ 
			self properties
				at: 'gs_options'
				put:
					(optionsArray collect: [ :each | each asString ]) asSortedCollection asArray ]
%

category: '*rowan-gemstone-definitions'
method: RwClassDefinition
gs_symbolDictionary

	^ self properties
		at: 'gs_SymbolDictionary'
		ifAbsent: []
%

category: '*rowan-gemstone-definitions'
method: RwClassDefinition
gs_symbolDictionary: aSymbolDictionaryName

	self properties
		at: 'gs_SymbolDictionary'
		put: aSymbolDictionaryName asString
%

category: '*rowan-gemstone-definitions'
method: RwClassDefinition
_compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue

	propertyKey = 'comment' ifFalse: [ ^super _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue ].
	propertyValue = baseValue
		ifTrue: [ ^ true ]
		ifFalse: [ 
			"empty or nil comments need to compare equal in GemStone"
			^(propertyValue == nil or: [ propertyValue isEmpty]) and: [ baseValue == nil or: [ baseValue isEmpty] ] ]
%

category: '*rowan-gemstone-definitions'
method: RwClassDefinition
_modificationForcingNewClassVersion

	^ RwClassModificationForcingNewClassVersion
%

! Class extensions for 'RwGsClassVersioningPatch'

!		Instance methods for 'RwGsClassVersioningPatch'

category: '*rowan-gemstone-loader-36x'
method: RwGsClassVersioningPatch
_newSubclassWithSuperclass: newSuperclass isEquivalentToSubclass: oldClass 
newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars 
newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars newConstraints: aConstraint 
suprBlock: suprBlock optsBlock: optsBlock ivsBlock: ivsBlock civsBlock: civsBlock poolsBlock: poolsBlock cvarsBlock: cvarsBlock consBlock: consBlock

	" based on Class>>_equivalentSubclass:superCls:name:newOpts:newFormat:newInstVars:newClassInstVars:newPools:newClassVars:inDict:isKernel: and ultimately needs to be rolled back into base, so that class creation and Rowan use the same new class version rules.
"

	"Class>>_equivalentSubclass:... has unwanted side effects"

	"squeezed down to the bare minimum"

	"oldClass is equivalent to the subclass that would be created using
 the other arguments if 
     instVar names match exactly ,
   and class instVar names match exactly ,
   and the classVars in oldClass can be modified to add/remove Associations 
     to match anArrayOfClassVars ,
   and pool dictionaries match exactly

  With respect to options and format, oldClass is equivalent if
    The state of format bits dbTransient, instancesNonPersistent, instancesInvariant  
     match exactly ,
    and subclassesDisallowed cannot be set in the new subclass if it not set in oldClass ,
    and modifiable  cannot be set if it is not set in oldClass  ,
    and  (SELF_CAN_BE_SPECIAL, NSC_DUPLICATES, INDEXABLE, IMPLEMENTATION, NO_STRUCT_UPDATE bits)
        of the formats must match exactly.
"

	| fmtArr |
	fmtArr := newSuperclass 
		_validateOptions: optionsArray 
		withFormat: theFormat
		newClassName: oldClass name asString.
	(oldClass isKindOf: Class)
		ifFalse: [ oldClass _validateClass: Class ].
	suprBlock value: oldClass superClass == newSuperclass.
	optsBlock value: (oldClass _optionsChangableTo: fmtArr).
	ivsBlock value: (oldClass _instVarsEqual: anArrayOfInstvarNames).
	civsBlock value: (oldClass class _instVarsEqual: anArrayOfClassInstVars).
	poolsBlock value: (oldClass _poolDictsEqual: anArrayOfPoolDicts).
	cvarsBlock value: (oldClass _classVarsChangableTo: anArrayOfClassVars copy).
	consBlock value: (aConstraint size = 0 or: [oldClass _constraintsEqual: aConstraint ])
%

! Class extensions for 'RwGsFileUtilities'

!		Class methods for 'RwGsFileUtilities'

category: '*rowan-gemstone-core-36x'
classmethod: RwGsFileUtilities
directoryExists: aDirectory

	"handle the case where GsFile class>>existsOnServer: returns nil"
	aDirectory ifNil: [ ^false ].
	^ (GsFile existsOnServer: aDirectory) == true
%

! Class extensions for 'RwGsSymbolDictionaryRegistry_Implementation'

!		Class methods for 'RwGsSymbolDictionaryRegistry_Implementation'

category: '*rowan-gemstone-loader-36x'
classmethod: RwGsSymbolDictionaryRegistry_Implementation
addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance

	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassExtension |
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	methodDictionary at: selector put: compiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior moveMethod: selector toCategory: protocolSymbol.

	existing := registryInstance methodRegistry at: compiledMethod ifAbsent: [ nil ].
	existing
		ifNotNil: [ 
			registryInstance
				error:
					'Internal error -- existing LoadedMethod found for extension compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	registryInstance methodRegistry at: compiledMethod put: loadedMethod.

	loadedPackage := Rowan image 
		loadedPackageNamed: packageName 
		ifAbsent: [ self existingOrNewLoadedPackageNamed: packageName instance: registryInstance ].

	loadedClassExtension := loadedPackage
		loadedClassExtensionForClass: behavior
		ifAbsent: [ 
			| class ext |
			class := behavior theNonMetaClass.
			ext := RwGsLoadedSymbolDictClassExtension
				newForClass: class
				inPackage: loadedPackage.
			(registryInstance classExtensionRegistry
				at: class classHistory
				ifAbsentPut: [ IdentitySet new ]) add: ext.
			ext ].
	loadedClassExtension addLoadedMethod: loadedMethod.
	^ registryInstance
%

category: '*rowan-gemstone-loader-36x'
classmethod: RwGsSymbolDictionaryRegistry_Implementation
addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance

	"there is an existing compiled method that has already been deleted from another package ... so we're adding it
		back using specialized processing"

	| methodDictionary selector |
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	methodDictionary 
		at: selector 
		ifAbsent: [  
			registryInstance
				error:
					'Internal error -- attempt to move a method that does not exist'  ].
	self _addMovedDeletedMethod: compiledMethod instance: registryInstance.
	^ self moveCompiledMethod: compiledMethod toProtocol: protocolString instance: registryInstance
%

category: '*rowan-gemstone-loader-36x'
classmethod: RwGsSymbolDictionaryRegistry_Implementation
addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance
	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassOrExtension |
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	(methodDictionary at: selector ifAbsent: [  ])
		ifNotNil: [ :oldCompiledMethod | 
			"there is an existing compiled method ... that means we're adding a recompiled methoded and moving it to the (possibly new) protocol"
			self addRecompiledMethod: compiledMethod instance: registryInstance.
			^ self
				moveCompiledMethod: compiledMethod
				toProtocol: protocolString
				instance: registryInstance ].
	methodDictionary at: selector put: compiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior moveMethod: selector toCategory: protocolSymbol.


	existing := registryInstance methodRegistry at: compiledMethod ifAbsent: [ nil ].
	existing
		ifNotNil: [ registryInstance error: 'Internal error -- existing LoadedMethod found for compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	registryInstance methodRegistry at: compiledMethod put: loadedMethod.

	loadedPackage := self
		loadedPackageNamed: packageName
		ifAbsent: [ 
			registryInstance
				error: 'Internal error -- attempt to add a method to a nonexistent package.' ]
		instance: registryInstance.

	loadedClassOrExtension := loadedPackage
		loadedClassOrClassExtensionForClass: behavior
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- attempt to add a method to a package in which its class is neither defined nor extended.' ].
	loadedClassOrExtension addLoadedMethod: loadedMethod.
	^ registryInstance
%

category: '*rowan-gemstone-loader-36x'
classmethod: RwGsSymbolDictionaryRegistry_Implementation
addRecompiledMethod: newCompiledMethod instance: registryInstance

	"add a recompiled compiled method to behavior and update the loaded things"

	| selector behavior methodDictionary oldCompiledMethod loadedMethod |
	selector := newCompiledMethod selector.
	behavior := newCompiledMethod inClass.
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	oldCompiledMethod := methodDictionary
		at: selector
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- expected an existing compiled method in the method dictionary' ].

	oldCompiledMethod == newCompiledMethod
		ifTrue: [ 
			"exit early, no more work to be done"
			^ registryInstance ].
	methodDictionary at: selector put: newCompiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	loadedMethod := registryInstance methodRegistry
		at: oldCompiledMethod
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the old compiledMethod.' ].
	registryInstance methodRegistry removeKey: oldCompiledMethod.
	loadedMethod handle: newCompiledMethod.
	registryInstance methodRegistry at: newCompiledMethod put: loadedMethod.
	^ registryInstance
%

category: '*rowan-gemstone-loader-36x'
classmethod: RwGsSymbolDictionaryRegistry_Implementation
moveCompiledMethod: compiledMethod toProtocol: newProtocol instance: registryInstance

	"move a compiled method into a different protocol and update loaded things"

	| behavior selector loadedMethod oldCat catSym catDict methodDictionary existingCompiledMethod |
	selector := compiledMethod selector.
	behavior := compiledMethod inClass.

	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	existingCompiledMethod := methodDictionary
		at: selector
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- no existing CompileMethod found for patched method.' ].
	existingCompiledMethod == compiledMethod
		ifFalse: [ 
			registryInstance
				error:
					'Internal error - the existingCompiledMethod is not identical to the compiled method arg' ].

	oldCat := behavior categoryOfSelector: selector environmentId: 0.
	catSym := newProtocol asSymbol.
	catDict := behavior _baseCategorysForStore: 0.
	oldCat ifNotNil: [ (catDict at: oldCat) remove: selector ].
	catDict
		at: catSym
		ifAbsent: [ behavior addCategory: newProtocol environmentId: 0 ].
	(catDict at: catSym) add: selector.

	behavior moveMethod: selector toCategory: newProtocol environmentId: 0.

	loadedMethod := registryInstance methodRegistry
		at: compiledMethod
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the compiledMethod.' ].

	loadedMethod updateForProtocolChange.
	^ registryInstance
%

category: '*rowan-gemstone-loader-36x'
classmethod: RwGsSymbolDictionaryRegistry_Implementation
_addMovedDeletedMethod: newCompiledMethod  instance: registryInstance

	"add a recompiled compiled method that was previously removed from loaded things
		to behavior and update the loaded things appropriately"

	| selector behavior methodDictionary oldCompiledMethod loadedMethod |
	selector := newCompiledMethod selector.
	behavior := newCompiledMethod inClass.
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	oldCompiledMethod := methodDictionary
		at: selector
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- expected an existing compiled method in the method dictionary' ].

	oldCompiledMethod == newCompiledMethod
		ifTrue: [ 
			"exit early, no more work to be done"
			^ registryInstance ].
	methodDictionary at: selector put: newCompiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	loadedMethod := registryInstance methodRegistry
		at: oldCompiledMethod
		ifAbsent: [].
	loadedMethod ifNotNil: [  
			registryInstance
				error:
					'Internal error -- unexpected loaded method found - deleteMethod processing should have removed the loaded method already' ].

	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: newCompiledMethod.

	registryInstance methodRegistry at: newCompiledMethod put: loadedMethod.
	^ registryInstance
%

! Class extensions for 'RwPackageDefinition'

!		Instance methods for 'RwPackageDefinition'

category: '*rowan-gemstone-definitions'
method: RwPackageDefinition
gs_symbolDictionary

	^ self properties
		at: 'gs_SymbolDictionary'
		ifAbsent: []
%

category: '*rowan-gemstone-definitions'
method: RwPackageDefinition
gs_symbolDictionary: aSymbolDictionaryName

	self properties
		at: 'gs_SymbolDictionary'
		put: aSymbolDictionaryName asString
%

! Class extensions for 'RwProjectDefinition'

!		Instance methods for 'RwProjectDefinition'

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
defaultSymbolDictName

	^ (self specification platformSpec at: 'gemstone') defaultSymbolDictName
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
defaultSymbolDictName: symbolDictName

	(self specification platformSpec at: 'gemstone')
		defaultSymbolDictName: symbolDictName
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
defaultUseSessionMethodsForExtensions: aBool

	(self specification platformSpec at: 'gemstone')
		defaultUseSessionMethodsForExtensions: aBool
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
methodEnvForPackageNamed: packageName

	^ (self specification platformSpec at: 'gemstone')
		methodEnvForPackageNamed: packageName
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
projectOwnerId

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
projectOwnerId: aUserId

	(self specification platformSpec at: 'gemstone') projectOwnerId: aUserId
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
setSymbolDictName: symbolDictName forPackageNamed: packageName

	(self specification platformSpec at: 'gemstone')
		setSymbolDictName: symbolDictName
		forPackageNamed: packageName
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
setUseSessionMethodsForExtensions: aBool forPackageNamed: packageName

	(self specification platformSpec at: 'gemstone')
		setUseSessionMethodsForExtensions: aBool
		forPackageNamed: packageName
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
symbolDictionaryRegistryClass

	^ RwGsSymbolDictionaryRegistry
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
symbolDictNameForPackageNamed: packageName

	^ (self specification platformSpec at: 'gemstone')
		symbolDictNameForPackageNamed: packageName
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
updateGsPlatformSpecLoadedProjectInfo: projectInfo

	| spec gemstoneSpec thePackageMapSpecs |
	spec := self specification.
	thePackageMapSpecs := projectInfo at:  'packageMapSpecs' .
	gemstoneSpec := spec platformSpec at: 'gemstone'.
	(thePackageMapSpecs at: #defaultSymbolDictName otherwise: nil) 
		ifNotNil: [:name | gemstoneSpec defaultSymbolDictName: name ].
	(thePackageMapSpecs at: #defaultUseSessionMethodsForExtensions otherwise: nil) 
		ifNotNil: [:boolean | 
			gemstoneSpec defaultUseSessionMethodsForExtensions: boolean  ].
	(thePackageMapSpecs at: #packageNameToPlatformPropertiesMap otherwise: nil) 
		ifNotNil: [:map | gemstoneSpec packageNameToPlatformPropertiesMap: map]
%

category: '*rowan-gemstone-definitions'
method: RwProjectDefinition
useSessionMethodsForExtensionsForPackageNamed: packageName

	^ (self specification platformSpec at: 'gemstone')
		useSessionMethodsForExtensionsForPackageNamed: packageName
%

! Class extensions for 'RwUnmanagedProjectDefinition'

!		Instance methods for 'RwUnmanagedProjectDefinition'

category: '*rowan-gemstone-definitions'
method: RwUnmanagedProjectDefinition
defaultSymbolDictName

	^ 'UnmanagedPackages'
%

category: '*rowan-gemstone-definitions'
method: RwUnmanagedProjectDefinition
methodEnvForPackageNamed: packageName

	^ 0
%

category: '*rowan-gemstone-definitions'
method: RwUnmanagedProjectDefinition
projectOwnerId

	^ Rowan image currentUserId
%

category: '*rowan-gemstone-definitions'
method: RwUnmanagedProjectDefinition
symbolDictNameForPackageNamed: packageName

	^ self defaultSymbolDictName
%

category: '*rowan-gemstone-definitions'
method: RwUnmanagedProjectDefinition
useSessionMethodsForExtensionsForPackageNamed: packageName

	^ true
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

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
allButLast

	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."

	^ self allButLast: 1
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
allButLast: n

	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: self size - n
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
beginsWith: aSequenceableCollection

	(aSequenceableCollection isEmpty
		or: [ self size < aSequenceableCollection size ])
		ifTrue: [ ^ false ].
	aSequenceableCollection
		withIndexDo: [ :each :index | 
			(self at: index) ~= each
				ifTrue: [ ^ false ] ].
	^ true
%

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

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
fifth

	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 5
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
fourth

	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 4
%

category: '*tonel-gemstonecommon-core'
method: SequenceableCollection
pairsCollect: aBlock 
	"Evaluate aBlock with my elements taken two at a time, and return an Array with the results"

	^ (1 to: self size // 2) collect:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsCollect:
	[:a :b | b, ' is number ', a printString]
"
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
putOn: aStream

	self do: [ :each | each putOn: aStream ]
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
second

	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 2
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
sixth

	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 6
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
sort

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
third

	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 3
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
withIndexDo: elementAndIndexBlock

	"Just like with:do: except that the iteration index supplies the second argument to the block."

	1 to: self size do: [ :index | elementAndIndexBlock value: (self at: index) value: index ]
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
writeStreamPortable

	^ WriteStreamPortable on: self
%

! Class extensions for 'Stream'

!		Instance methods for 'Stream'

category: '*tonel-gemstone-kernel'
method: Stream
<< items

	items putOn: self
%

! Class extensions for 'String'

!		Instance methods for 'String'

category: '*rowan-gemstone-kernel'
method: String
decodeFromUTF8

"Deprecated.  Applications should use instances of Utf8 to hold UTF-8 data.
 Decode receiver from UTF8 format."

"Undeprecated by Rowan, because the method has been undeprecated in later GemStone versions"

" Returns either a Unicode7 , Unicode16 or Unicode32 ,
 using the minimum character size needed to represent decoded result."

 ^ self _decodeFromUtf8: true
%

! Class extensions for 'Symbol'

!		Instance methods for 'Symbol'

category: '*tonel-gemstone-kernel'
method: Symbol
keywords

	"Answer an array of the keywords that compose the receiver."

	| kwd char keywords |
	keywords := Array new.
			kwd := WriteStreamPortable on: String new.
			1 to: self size do: [ :i | 
				kwd nextPut: (char := self at: i).
				char = $:
					ifTrue: [ 
						keywords add: kwd contents.
						kwd reset ] ].
			kwd position = 0
				ifFalse: [ keywords add: kwd contents ].
	(keywords size >= 1 and: [ (keywords at: 1) = ':' ])
		ifTrue: [ 
			"Has an initial keyword, as in #:if:then:else:"
			keywords := keywords allButFirst ].
	(keywords size >= 2 and: [ (keywords at: keywords size - 1) = ':' ])
		ifTrue: [ 
			"Has a final keyword, as in #nextPut::andCR"
			keywords := keywords
				copyReplaceFrom: keywords size - 1
				to: keywords size
				with: {(':' , keywords last)} ].
	^ keywords
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
	[ (aClass := RwGsPatchSet _anyElementOf: toBeOrdered ifEmpty: [ nil ]) isNil ]
		whileFalse: [ 
			RwGsPatchSet
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

! Class extensions for 'TonelParser'

!		Class methods for 'TonelParser'

category: '*tonel-gemstonecommon-core'
classmethod: TonelParser
readStreamClass

	^ ReadStreamPortable
%

category: '*tonel-gemstonecommon-core'
classmethod: TonelParser
writeStreamClass

	^ WriteStreamPortable
%

! Class extensions for 'TonelWriter'

!		Class methods for 'TonelWriter'

category: '*tonel-gemstonecommon-core'
classmethod: TonelWriter
lineEnding
  "Answer the os-specific line endings"

  ^ String with: Character lf
%

category: '*tonel-gemstonecommon-core'
classmethod: TonelWriter
orderedDictionaryClass
  "Answer the platform-specific OrderedDictionary-compatible class"

  ^ GsTonelOrderedDictionary
%

! Class extensions for 'UndefinedObject'

!		Instance methods for 'UndefinedObject'

category: '*rowan-gemstone-kernel-extensions-36x'
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

