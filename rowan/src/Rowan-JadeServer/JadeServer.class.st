Class {
	#name : 'JadeServer',
	#superclass : 'Object',
	#instVars : [
		'classList',
		'classOrganizers',
		'readStream',
		'writeStream',
		'selectedClass',
		'methodFilterType',
		'methodFilters',
		'selections',
		'methodCommandResult'
	],
	#classVars : [
		'ExternalInteger',
		'GciError',
		'GsObject',
		'OopType32',
		'OopType64'
	],
	#category : 'Rowan-JadeServer'
}

{ #category : 'jadeite server' }
JadeServer class >> theJadeiteServer [
  ^ SessionTemps current
    at: #'jadeiteServer'
    ifAbsentPut: [ 
      | jadeServerClass |
      jadeServerClass := (System _gemVersion beginsWith: '3.2')
        ifTrue: [ Rowan jadeServerClassNamed: #'JadeServer64bit32' ]
        ifFalse: [ Rowan jadeServerClassNamed: #'JadeServer64bit35' ].
      jadeServerClass new
        initialize;
        yourself ]
]

{ #category : 'category' }
JadeServer >> _addClass: each toStream: stream [ 

	self
		_addClass: each 
		toStream: stream 
		isVisible: true
		fromDictionary: nil.


]

{ #category : 'category' }
JadeServer >> _addClass: aClass toStream: aStream isVisible: aBoolean fromDictionary: aDictionary [
	"1. OOP; 2. key; 3. category; 4. dictionary name; 5. superclass OOP; 6. children; 7. Visible/Inherited; 8. Class History; 9. isTestCase"

	| testCaseClass history |
"1"	(self oopOf: aClass) printOn: aStream.
"2"	aStream tab; nextPutAll: (aDictionary  isNil ifTrue: [aClass name] ifFalse: [aDictionary keyAtValue: aClass ifAbsent: [aClass name]]); tab.
"3"	aClass category notNil ifTrue: [aStream nextPutAll: aClass category].
"4"	aStream tab; nextPutAll: (aDictionary isNil ifTrue: ['?'] ifFalse: [aDictionary name]).
"5"	aStream tab. (self oopOf: aClass superclass) printOn: aStream.
	aStream 
"6"		tab; "let client build children list"
"7"		tab; nextPut: (aBoolean ifTrue: [$V] ifFalse: [$I]);
		tab.
	(history := self historyOf: aClass) isNil ifTrue: [history := Array with: aClass].
"8"	(history indexOf: aClass) printOn: aStream.
	aStream nextPut: $/.
	history size printOn: aStream.
	aStream tab.
	testCaseClass := Globals
		at: #'TestCase'
		ifAbsent: [nil].
"9"	(testCaseClass notNil and: [aClass isSubclassOf: testCaseClass]) printOn: aStream.
	aStream lf.


]

{ #category : 'category' }
JadeServer >> _addMethod: aGsMethod toStream: aStream [
	"See GsMethod2>>initialize:"

	| inClass testCaseClass |
	inClass := aGsMethod inClass.
"1"	(self oopOf: aGsMethod) printOn: aStream.
	aStream 
"2"		tab; nextPutAll: aGsMethod selector; 
"3"		tab; nextPutAll: (self _behavior: inClass categoryOfSelector: aGsMethod selector);
		tab.

	"Class"
"4"	(self oopOf: inClass) printOn: aStream.
"5"	aStream tab; nextPutAll: inClass name; tab.
"6"	inClass category notNil ifTrue: [aStream nextPutAll: inClass category].
"7"	aStream tab; nextPutAll: (self nameOfFirstDictionaryReferencing: inClass thisClass); tab.

	"SUnit Test Method"
	testCaseClass := Globals
		at: #'TestCase'
		ifAbsent: [nil].
"8"	((testCaseClass notNil and: [inClass isSubclassOf: testCaseClass]) and: [inClass testSelectors includes: aGsMethod selector]) printOn: aStream.
	aStream lf.


]

{ #category : 'category' }
JadeServer >> _addToPureExportSet: anObject [

	System 
		_add: anObject 
		toGciSet: 39.  "PureExportSet"


]

{ #category : 'category' }
JadeServer >> _allSelectors [

	| allSelectors |
	allSelectors := IdentitySet new.
	self classOrganizer classes do: [:each | 
		allSelectors addAll: each selectors; addAll: each class selectors.
	].
	^allSelectors

]

{ #category : 'category' }
JadeServer >> _behavior: aBehavior categoryOfSelector: aSymbol [

	^aBehavior categoryOfSelector: aSymbol

]

{ #category : 'category' }
JadeServer >> _describeMCAddition: anMCAddition on: aStream [

	aStream 
		nextPut: $A; tab;
		nextPutAll: (self oopOf: anMCAddition) printString; tab;
		yourself.
	self 
		_describeMCDefinition: anMCAddition definition 
		on: aStream.


]

{ #category : 'category' }
JadeServer >> _describeMCClassDefinition: anMCClassDefinition on: aStream [

	| string |
	string := anMCClassDefinition definitionString collect: [:char |
		char = Character lf
			ifTrue: [Character cr]
			ifFalse: [char].
	].
	aStream
		nextPut: $C; tab;
		nextPutAll: string; lf;
		yourself.


]

{ #category : 'category' }
JadeServer >> _describeMCDefinition: anMCDefinition on: aStream [

	anMCDefinition isMethodDefinition ifTrue: [
		self 
			_describeMCMethodDefinition: anMCDefinition 
			on: aStream.
		^self.
	].
	anMCDefinition isOrganizationDefinition ifTrue: [
		self 
			_describeMCOrganizationDefinition: anMCDefinition 
			on: aStream.
		^self.
	].
	anMCDefinition isClassDefinition ifTrue: [
		self 
			_describeMCClassDefinition: anMCDefinition 
			on: aStream.
		^self.
	].
	self halt.


]

{ #category : 'category' }
JadeServer >> _describeMCMethodDefinition: anMCMethodDefinition on: aStream [
	| unicodeFreeSource |
	unicodeFreeSource := RowanMethodService removeUnicodeFromSource: anMCMethodDefinition source.
	aStream
		nextPut: $M; tab;
		nextPutAll: anMCMethodDefinition timeStamp; tab;
		nextPutAll: anMCMethodDefinition className; tab;
		nextPutAll: anMCMethodDefinition classIsMeta printString; tab;
		nextPutAll: anMCMethodDefinition category; tab;
		nextPutAll: anMCMethodDefinition selector; tab;
		nextPutAll: unicodeFreeSource size printString; tab;
		nextPutAll: unicodeFreeSource; lf.
]

{ #category : 'category' }
JadeServer >> _describeMCModification: anMCModification on: aStream [

	aStream nextPut: $M; tab;
		nextPutAll: (self oopOf: anMCModification) printString; tab;
		yourself.
	self 
		_describeMCDefinition: anMCModification obsoletion 
		on: aStream.
	self 
		_describeMCDefinition: anMCModification modification 
		on: aStream.


]

{ #category : 'category' }
JadeServer >> _describeMCOrganizationDefinition: anMCOrganizationDefinition on: aStream [

	aStream
		nextPut: $O; tab;
		yourself.
	anMCOrganizationDefinition categories do: [:each | 
		aStream nextPutAll: each; tab.
	].
	aStream lf.


]

{ #category : 'category' }
JadeServer >> _describeMCRemoval: anMCRemoval on: aStream [

	aStream nextPut: $R; tab;
		nextPutAll: (self oopOf: anMCRemoval) printString; tab;
		yourself.
	self 
		_describeMCDefinition: anMCRemoval definition 
		on: aStream.


]

{ #category : 'category' }
JadeServer >> _describeMethod: aMethod [
  "Provide info needed to create a GsMethod in Jade client"

  "Nice to add packageName and mcTimestamp"

  | allSelectors class list oldGsMethod string x |
  self environment: (self environmentForMethod: aMethod).
  writeStream
    nextPutAll: (class := aMethod inClass) asOop printString;
    tab;
    nextPutAll: class printString;
    tab;
    nextPutAll: aMethod asOop printString;
    tab;
    nextPutAll:
        ((x := aMethod selector) isNil
            ifFalse: [ x ]
            ifTrue: [ '' ]);
    tab;
    nextPutAll: (self categoryOfMethod: aMethod);
    tab;
    nextPutAll: (self currentUserMayEditMethod: aMethod) asString;
    tab;
    lf.	"Line 1 for GsMethod (line 3 for JadeSystemBrowserPresenter)"	"1"	"2"	"3"	"4"	"5"	"6"	"Method source"
  writeStream nextPutAll: (string := aMethod sourceString).
  string last = Character lf
    ifFalse: [ writeStream lf ].
  writeStream
    nextPut: $%;
    lf.	"Lines 2-N"	"unimplemented selectors"	"https://github.com/jgfoster/Jade/issues/117"
  ((aMethod class includesSelector: #'_selectorPool')
    and: [ aMethod class includesSelector: #'_sourceOffsetOfFirstSendOf:' ])
    ifTrue: [ 
      allSelectors := self _allSelectors.
      (aMethod _selectorPool reject: [ :each | allSelectors includes: each ])
        do: [ :each | 
          (aMethod _sourceOffsetOfFirstSendOf: each) printOn: writeStream.
          writeStream
            space;
            nextPutAll: each;
            tab ] ].
  writeStream lf.	"Line N+1"	"Array of Associations (offset -> selector) indexed by step points"
  list := self sbUpdateMethodStepPointsFor: aMethod.
  list := list collect: [ :each | each key printString , ' ' , each value ].
  self writeList: list.	"Line N+2"	"breaks"
  list := self sbUpdateMethodBreakPointsFor: aMethod.
  self writeList: (list collect: [ :each | each printString ]).	"Line N+3"	"original method"
  oldGsMethod := (aMethod inClass class
    canUnderstand: #'persistentMethodDictForEnv:')
    ifTrue: [ 
      (aMethod inClass persistentMethodDictForEnv: 0)
        at: aMethod selector
        ifAbsent: [ aMethod ] ]
    ifFalse: [ aMethod ].
  aMethod ~~ oldGsMethod
    ifTrue: [ 
      string := oldGsMethod sourceString.
      writeStream nextPutAll: string.
      (string notEmpty and: [ string last = Character lf ])
        ifFalse: [ writeStream lf ] ].
  writeStream
    nextPut: $%;
    lf.	"method compile warnings"
  string := selections isNil
    ifTrue: [ '' ]
    ifFalse: [ selections at: #'methodWarnings' ifAbsent: [ '' ] ].
  string isNil
    ifTrue: [ string := '' ].
  writeStream
    nextPutAll: string;
    nextPut: $%;
    lf
]

{ #category : 'category' }
JadeServer >> _mcDescriptionOfPatch: aPatch baseName: aString1 alternateName: aString2 [

	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aPatch) printOn: stream.
	stream 
		tab; nextPutAll: (aString1 isNil ifTrue: ['loaded'] ifFalse: [aString1]);
		nextPutAll: ' vs. ';
		nextPutAll: (aString2 isNil ifTrue: ['loaded'] ifFalse: [aString2]);
		lf.
	aPatch operations do: [:each | 
		each isAddition 		ifTrue: [self _describeMCAddition: 		each on: stream].
		each isModification 	ifTrue: [self _describeMCModification: 	each on: stream].
		each isRemoval 		ifTrue: [self _describeMCRemoval: 		each on: stream].
	].
	^stream contents.



]

{ #category : 'category' }
JadeServer >> _mcTopazFrom: aSnapshot on: aStream [

	| classes dict parents methods queue |
	classes := aSnapshot definitions select: [:each | each isClassDefinition].
	dict := Dictionary new.
	classes do: [:each | 
		| parent myself |
		parent := dict 
			at: each superclassName 
			ifAbsentPut: [nil -> Set new].
		myself := dict
			at: each className
			ifAbsentPut: [nil -> Set new].
		myself key: each.
		parent value add: myself.
	].
	dict := dict reject: [:each | each key isNil].
	parents := dict keys.
	dict copy do: [:each | 
		(parents includes: each key superclassName) ifTrue: [
			dict removeKey: each key className.
		].
	].
	queue := (dict asSortedCollection: [:a :b | a key <= b key]) asOrderedCollection.
	[
		queue notEmpty.
	] whileTrue: [
		| assoc children def |
		assoc := queue removeFirst.
		children := (assoc value asSortedCollection: [:a :b | a key <= b key]) asOrderedCollection.
		queue := children , queue.
		def := assoc key.
		aStream 
			nextPutAll: '! - ' , def className; lf;
			nextPutAll: '! - ' , def commentStamp; lf;
			nextPutAll: 'run'; lf;
			nextPutAll: '(' , def superclassName; lf;
			tab; nextPutAll: 'subclass: ' , def className printString; lf;
			tab; nextPutAll: 'instVarNames: #(' , def instanceVariablesString , ')'; lf;
			tab; nextPutAll: 'classVars: #(' , def classVariablesString , ')'; lf;
			tab; nextPutAll: 'classInstVars: #(' , def classInstanceVariablesString , ')'; lf;
			tab; nextPutAll: 'poolDictionaries: #(' , def sharedPoolsString , ')'; lf;
			tab; nextPutAll: 'inDictionary: UserGlobals'; lf;
			tab; nextPutAll: 'instancesInvariant: false'; lf;
			tab; nextPutAll: 'isModifiable: false)'; lf;
			tab; nextPutAll: 'category: ' , def category printString , '.'; lf;
			nextPutAll: 'true.'; lf;
			nextPut: $%; lf;
			yourself.
	].
	methods := aSnapshot definitions select: [:each | each isMethodDefinition].
	methods := methods asSortedCollection.
	classes asSortedCollection do: [:eachClass | 
		| localMethods |
		localMethods := methods select: [:eachMethod | eachClass className = eachMethod className].
		methods removeAll: localMethods.
		aStream
			lf; nextPutAll: '! - *** - ' , eachClass className; lf;
			nextPutAll: 'removeAllClassMethods ' , eachClass className; lf;
			nextPutAll: 'removeAllMethods ' , eachClass className; lf;
			yourself.
		localMethods do: [:eachMethod | 
			| source |
			source := eachMethod source copyReplaceAll: Character cr asString with: Character lf asString.
			aStream
				nextPutAll: 'category: ''' , eachMethod category , ''''; lf;
				nextPutAll: '! - ' , eachMethod timeStamp; lf;
				nextPutAll: (eachMethod classIsMeta ifTrue: ['classMethod: '] ifFalse: ['method: ']) , eachMethod className; lf;
				nextPutAll: source; lf;
				nextPut: $%; lf;
				yourself.
		].
	].
	aStream lf; nextPutAll: '! - *** - loose methods (where class is expected to be already defined)'; lf; lf.
	methods isEmpty ifTrue: [aStream nextPutAll: '! - (none)'; lf; lf].

	methods do: [:eachMethod | 
		| source |
		source := eachMethod source copyReplaceAll: Character cr asString with: Character lf asString.
		aStream
			nextPutAll: 'category: ''' , eachMethod category , ''''; lf;
			nextPutAll: '! - ' , eachMethod timeStamp; lf;
			nextPutAll: (eachMethod classIsMeta ifTrue: ['classMethod: '] ifFalse: ['method: ']) , eachMethod className; lf;
			nextPutAll: source; lf;
			nextPut: $%; lf;
			yourself.
	].

	aStream lf; nextPutAll: '! - *** - class initialization'; lf.
	methods isEmpty ifTrue: [aStream nextPutAll: '! - (none)'; lf; lf].
	classes do: [:each | 
		aStream nextPutAll: 'send ' , each className , ' initialize'; lf.
	].



]

{ #category : 'category' }
JadeServer >> _methodsFor: aClass categories: aList [

	| methods |
	methods := IdentitySet new.
	aList do: [:eachCategory | 
		(aClass _includesCategory: eachCategory) ifTrue: [
			(aClass selectorsIn: eachCategory) do: [:eachSelector |
				methods add: (self compiledMethodAt: eachSelector inClass: aClass).
			].
		].
	].
	^methods.



]

{ #category : 'category' }
JadeServer >> _methodsFor: aClass filter: aList isVariables: aBoolean [

	^aBoolean 
		ifTrue:	[self _methodsFor: aClass variables: 	aList]
		ifFalse:	[self _methodsFor: aClass categories: aList].


]

{ #category : 'category' }
JadeServer >> _methodsFor: aClass variables: aList [

	| methods |
	aList isEmpty ifTrue: [^aClass selectors collect: [:each | self compiledMethodAt: each inClass: aClass]].
	methods := IdentitySet new.
	aClass selectors do: [:each | 
		| method intersect |
		method := self compiledMethodAt: each inClass: aClass.
		intersect := method instVarsAccessed * aList.
		intersect notEmpty ifTrue: [methods add: method].
	].
	^methods.


]

{ #category : 'category' }
JadeServer >> _oopAndStringFor: anObject [

	^(self oopOf: anObject) -> anObject printString.


]

{ #category : 'category' }
JadeServer >> _packageNameFor: aCategoryName [

	| string mcWorkingCopyClass list |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^''].
	string := aCategoryName asUppercase copyFrom: 2 to: aCategoryName size.
	list := mcWorkingCopyClass allManagers collect: [:each | each packageName].
	list := list select: [:each | (string copyFrom: 1 to: (string size min: each size)) = each asUppercase].
	list isEmpty ifTrue: [^''].
	list size = 1 ifTrue: [^list first].
	^(list asSortedCollection: [:a :b | a size <= b size]) last.


]

{ #category : 'category' }
JadeServer >> _sourceForProcess: gsProcess frame: level [

	| frame homeMethod stepPoint keys values gsMethod receiver |
	writeStream := WriteStream on: String new.
	(frame := gsProcess _frameContentsAt: level) isNil ifTrue: [^'No frame found for level ' , level printString].
	gsMethod := frame at: 1.
	stepPoint := self 
		_stepPointFromProcess: gsProcess 
		frame: frame 
		method: gsMethod
		level: level.
	writeStream
		nextPutAll: '<?xml version=''1.0'' ?><frame oop=';
		nextPutAll: (self oopOf: frame) printString printString;
		nextPutAll: ' ipOffset=';
		nextPutAll: (frame at: 2) printString printString;
		nextPutAll: ' frameOffset=';
		nextPutAll: ((frame at: 3) isNil ifTrue: [''] ifFalse: [(frame at: 3) printString]) printString;
		nextPutAll: ' stepPoint=';
		nextPutAll: stepPoint printString printString;
		nextPutAll: '>'; lf;
		yourself.
	receiver := frame at: 10.
	values := OrderedCollection new.
	(self isClientForwarder: receiver) ifTrue: [
		keys := OrderedCollection with: 'clientObject'.
		values add: receiver clientObject.
		receiver := '[aClientForwarder(' , (self oopOf: receiver) printString , ')]'.
	] ifFalse: [
		((receiver isKindOf: BlockClosure) or: [receiver isKindOf: Class]) ifTrue: [
			keys := OrderedCollection new.
		] ifFalse: [
			keys := receiver class allInstVarNames asOrderedCollection collect: [:each | '-' , each].
			1 to: keys size do: [:i |
				values add: (receiver instVarAt: i).
			].
		].
	].
	keys addFirst: #'receiver'.
	values addFirst: receiver.
	keys addAll: (frame at: 9).
	keys := keys reject: [:each | each first == $.].
	values addAll: (frame size >= 11
		ifTrue: [frame copyFrom: 11 to: frame size]
		ifFalse: [#()]).
	1 to: (keys size min: values size) do: [:i | | oop assoc key value |
		key := keys at: i.
		value := values at: i.
		assoc := self _oopAndStringFor: value.
		oop := assoc key.
		value := assoc value.
		value size > 500 ifTrue: [value := (value copyFrom: 1 to: 500) , '...'].
		value := value collect: [:char | (char asciiValue < 32 or: [127 < char asciiValue]) ifTrue: [$?] ifFalse: [char]].
		writeStream
			nextPutAll: '<var oop=';
			nextPutAll: oop asString printString;
			nextPutAll: ' name=';
			nextPutAll: key asString printString;
			nextPutAll: ' ><';
			nextPutAll: '![';
			nextPutAll: 'CDATA';
			nextPutAll: '[';
			nextPutAll: value;
			nextPutAll: ']';
			nextPutAll: ']';
			nextPutAll: '></var>'; lf;
			yourself.
	].
	homeMethod := self homeMethodFor: gsMethod.
	writeStream 
		nextPutAll: '<source';
		nextPutAll: ' ><';
		nextPutAll: '![';
		nextPutAll: 'CDATA';
		nextPutAll: '[';
		yourself.
	self _describeMethod: homeMethod.
	writeStream
		nextPutAll: ']';
		nextPutAll: ']';
		nextPutAll: '></source>';
		nextPutAll: '</frame>'; lf;
		yourself.
	^self asAsciiString: writeStream contents.



]

{ #category : 'category' }
JadeServer >> _stepPointFromProcess: gsProcess frame: aFrame method: gsMethod level: anInteger [

	self subclassResponsibility.

]

{ #category : 'category' }
JadeServer >> _stepPointsForBreaksInMethod: gsMethod [

	^gsMethod _stepPointsFromBreakIpOffsets: gsMethod _breakpointIpOffsets

]

{ #category : 'category' }
JadeServer >> _trimStackOf: aGsProcess toLevel: anInteger [

	aGsProcess _trimStackToLevel: anInteger.
	^aGsProcess.


]

{ #category : 'category' }
JadeServer >> abort [

	classOrganizers := Array new: 4.
	System abortTransaction.
]

{ #category : 'category' }
JadeServer >> addAccessorsFor: aString inBehavior: aBehavior [

	aBehavior compileAccessingMethodsFor: (Array with: aString asSymbol).


]

{ #category : 'category' }
JadeServer >> addCategory: aString to: aClass [ 

	aClass addCategory: aString.


]

{ #category : 'category' }
JadeServer >> addGroup: aString toUser: aUserProfile [

	aUserProfile addGroup: aString.


]

{ #category : 'category' }
JadeServer >> addMethodCategoryNamesToMethodFilters [

	classList do: [:each | methodFilters addAll: each categoryNames].


]

{ #category : 'category' }
JadeServer >> addMissingAccessorsFor: aClass [

	aClass compileMissingAccessingMethods.


]

{ #category : 'category' }
JadeServer >> addPrivilege: aString toUser: aUserProfile [

	aUserProfile addPrivilege: aString.


]

{ #category : 'category' }
JadeServer >> addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler [

	| x |
	aStream lf
"1"	nextPutAll: aString; tab;
"2"	nextPutAll: aProcess asOop printString; tab;
"3"	nextPutAll: aProcess priority printString; tab;
"4"	nextPutAll: (aProcess createdByApplication ifTrue: ['Y'] ifFalse: ['']); tab; 
"5"	nextPutAll: ((x := aProcess stackId) == -1 	ifTrue: [''] ifFalse: [x printString]); tab;
"6"	nextPutAll: ((x := aProcess waitingOn) 	isNil ifTrue: [''] ifFalse: [x asOop printString]); tab;
"7"	nextPutAll: ((x := aProcess _signalTime) 	isNil ifTrue: [''] ifFalse: [(x - aScheduler _now) printString]); tab;
"8"	nextPutAll: (aProcess isPartialContinuation	ifTrue: ['partial'] ifFalse: [aProcess isContinuation ifTrue: ['full'] ifFalse: ['']]); tab;
"9"	"type: forked or main"
"10"	"live or terminated"
	yourself.


]

{ #category : 'category' }
JadeServer >> addSessionWithId: anInteger toStream: aStream [

	| array gsSession timeGmt x |
	array := System descriptionOfSession: anInteger.
	array size: 20.
	gsSession := GsSession sessionWithSerialNumber: (array at: 9).
	timeGmt := System timeGmt.
	aStream
		nextPutAll: '<session oop=';
		nextPutAll: (self oopOf: gsSession) printString printString;
		nextPutAll: ' name=';
		nextPutAll: (array at: 1) userId printString;
		nextPutAll: ' process=';
		nextPutAll: (array at: 2) printString printString;
		nextPutAll: ' host=';
		nextPutAll: (array at: 3) printString;
		nextPutAll: ' primitive=';
		nextPutAll: (array at: 4) printString printString;
		nextPutAll: ' viewAge=';
		nextPutAll: (timeGmt - (array at: 5)) printString printString;
		nextPutAll: ' state=';
		nextPutAll: (array at: 6) printString printString;
		nextPutAll: ' transaction=';
		nextPutAll: (array at: 7) printString printString;
		nextPutAll: ' hasOldestCR=';
		nextPutAll: (array at: 8) printString printString;
		nextPutAll: ' serial=';
		nextPutAll: (array at: 9) printString printString;
		nextPutAll: ' id=';
		nextPutAll: (array at: 10) printString printString;
		nextPutAll: ' ip=';
		nextPutAll: (array at: 11) printString;
		nextPutAll: ' priority=';
		nextPutAll: ((x := array at: 12) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' hostId=';
		nextPutAll: ((x := array at: 13)  isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' quietTime=';
		nextPutAll: ((x := array at: 14) isNil ifTrue: [''] ifFalse: [(timeGmt - x)  printString]) printString;
		nextPutAll: ' lifeTime=';
		nextPutAll: ((x := array at: 15) isNil ifTrue: [''] ifFalse: [(timeGmt - x)  printString]) printString;
		nextPutAll: ' backlog=';
		nextPutAll: ((x := array at: 16) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' description=';
		nextPutAll: ((x := array at: 17) isNil ifTrue: [''] ifFalse: [x]) printString;
		nextPutAll: ' objects=';
		nextPutAll: ((x := array at: 18) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' pages=';
		nextPutAll: ((x := array at: 19) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' voteState=';
		nextPutAll: ((x := array at: 20) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' />';
		yourself.


]

{ #category : 'category' }
JadeServer >> addUser: aUserProfile toStream: aStream [

	(self oopOf: aUserProfile) printOn: aStream.
	aStream tab; nextPutAll: aUserProfile userId.
	aStream tab; nextPutAll: (aUserProfile lastLoginTime asStringUsingFormat: #(1 2 3 $  2 1 $: true true true false)).
	aStream tab. aUserProfile loginsAllowedBeforeExpiration printOn: aStream.
	aStream tab. aUserProfile isDisabled printOn: aStream.
	aStream tab. aUserProfile activeUserIdLimit printOn: aStream.
	aStream tab.	"; nextPutAll: aUserProfile nativeLanguage asString."
	aStream tab. aUserProfile reasonForDisabledAccount printOn: aStream.
	aStream tab; nextPutAll: (aUserProfile lastPasswordChange asStringUsingFormat: #(1 2 3 $  2 1 $: true true true false)).
	aStream tab. aUserProfile passwordNeverExpires printOn: aStream.
	aStream lf.


]

{ #category : 'category' }
JadeServer >> allGroups [

	| allGroups myGroups stream |
	allGroups := AllGroups keys asSortedCollection.
	myGroups := (AllUsers userWithId: 'GcUser') groups.
	stream := WriteStream on: String new.
	allGroups do: [:each | 
		stream nextPutAll: each; tab.
		(myGroups includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> allSessions [

	| list stream |
	stream := WriteStream on: String new.
	stream nextPutAll: '<?xml version=''1.0'' ?><sessions>'.
	list := System currentSessionNames subStrings: Character lf.
	list := list reject: [:each | each isEmpty].
	list := list collect: [:each | (each subStrings at: 3) asNumber].
	list do: [:each | 
		self
			addSessionWithId: each
			toStream: stream.
	].
	^stream 
		nextPutAll: '</sessions>';
		contents.


]

{ #category : 'category' }
JadeServer >> allUsersPasswordLimits [

	| stream |
	stream := WriteStream on: String new.
	AllUsers disallowUsedPasswords printOn: stream. stream tab.
	AllUsers minPasswordSize printOn: stream. stream tab.
	AllUsers maxPasswordSize printOn: stream. stream tab.
	AllUsers maxRepeatingChars printOn: stream. stream tab.
	AllUsers maxConsecutiveChars printOn: stream. stream tab.
	AllUsers maxCharsOfSameType printOn: stream. stream tab.
	AllUsers staleAccountAgeLimit printOn: stream. stream tab.
	AllUsers passwordAgeLimit printOn: stream. stream lf.
	AllUsers disallowedPasswords do: [:each | 
		stream nextPutAll: each; tab.
	].
	stream lf. AllUsers passwordAgeWarning printOn: stream. stream lf.
	^stream contents.


]

{ #category : 'category' }
JadeServer >> asAsciiString: aString [

	^String withAll: (aString asArray collect: [:char | 
		((32 <= char asciiValue and: [char asciiValue <= 127]) or: [char isSeparator])
			ifTrue: [char]
			ifFalse: [$?].
	]).


]

{ #category : 'category' }
JadeServer >> assignClass: aClass toCategory: aString [

	aClass thisClass category: aString.


]

{ #category : 'category' }
JadeServer >> asString: anObject [

	(anObject isKindOf: String) ifTrue: [^anObject].
	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | 
			^'<<printString error: ' , ex printString , '>>'.
		].
	^anObject printString.


]

{ #category : 'category' }
JadeServer >> authorInitials: aString [

	| packagePolicy |
	(packagePolicy := self gsPackagePolicy) isNil ifTrue: [^self].
	packagePolicy authorInitials: aString.


]

{ #category : 'jadeite' }
JadeServer >> autoCommitIfRequired [
	| commitResult |
	Rowan serviceClass autoCommit == true ifTrue:[
		commitResult := System commitTransaction.
		RowanAutoCommitService new autoCommit:  
			(commitResult 
				ifTrue:[true] 
				ifFalse:[#failed])].
]

{ #category : 'category' }
JadeServer >> beginTransaction [

	classOrganizers := Array new: 4.
	System beginTransaction.


]

{ #category : 'category' }
JadeServer >> behaviorFor: selector in: aClass [

	| behavior |
	behavior := aClass.
	[
		behavior notNil.
	] whileTrue: [
		(behavior includesSelector: selector) ifTrue: [^behavior].
		behavior := behavior superclass.
	].
	self error: 'Method not found in class or in any superclass'.


]

{ #category : 'category' }
JadeServer >> categoryListFor: aSymbolDictionary [

	| categories stream |
	categories := Set new.
	aSymbolDictionary do: [:each | 
		each isBehavior ifTrue: [
			categories add: each category.
		].
	].
	categories copy do: [:each | 
		1 to: each size do: [:i | 
			(each at: i) = $- ifTrue: [
				| string |
				string := each copyFrom: 1 to: i - 1.
				(categories includes: string) ifFalse: [
					categories add: string.
					self _addToPureExportSet: string.
				].
			].
		].
	].
	stream := WriteStream on: String new.
	categories asSortedCollection do: [:each | 
		(self oopOf: each) printOn: stream.
		stream tab; nextPutAll: each; lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> categoryOfMethod: aMethod [

	| selector |
	(selector := aMethod selector) isNil ifTrue: [^''].
	^self _behavior: aMethod inClass categoryOfSelector: selector.


]

{ #category : 'category' }
JadeServer >> class: aClass includesSelector: aSelector [

	^aClass includesSelector: aSelector asSymbol.


]

{ #category : 'category' }
JadeServer >> classesForUser: aUserProfile [

	| stream |
	stream := WriteStream on: String new.
	aUserProfile symbolList do: [:eachDict |
		eachDict keysAndValuesDo: [:key :value |
			value isBehavior ifTrue: [
				stream nextPutAll: key; space; nextPutAll: value category asString; tab.
			].
		].
		stream lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> classListFor: aDictionary category: aString [

	| visibleClasses allClasses stream queue |
	visibleClasses := aDictionary asArray select: [:each | 
		each isBehavior and: [aString isNil or: [
			| category |
			(category := each category) notNil and: [
			category = aString or: [
			category matchPattern: (Array with: aString with: $*)]]]]].
	allClasses := visibleClasses asIdentitySet.
	queue := visibleClasses asOrderedCollection.
	[
		queue notEmpty.
	] whileTrue: [
		| parent |
		parent := queue removeFirst superclass.
		(parent notNil and: [(allClasses includes: parent) not]) ifTrue: [
			queue add: parent.
			allClasses add: parent.
		].
	].
	stream := WriteStream on: String new.
	allClasses do: [:each |
		self
			_addClass: each 
			toStream: stream 
			isVisible: (visibleClasses includes: each)
			fromDictionary: aDictionary.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> classOrganizer [

	^ClassOrganizer new


]

{ #category : 'category' }
JadeServer >> clearBreakAtStepPoint: anInteger inMethod: aGsMethod [

	aGsMethod clearBreakAtStepPoint: anInteger.


]

{ #category : 'category' }
JadeServer >> commentFor: aClass [

	| description |
	(Class canUnderstand: #'classComment') ifTrue: [
		^aClass classComment.
	].
	(description := aClass description) isNil ifTrue: [^nil].
	(description class name = #'GsClassDocumentation') ifTrue: [^description detailsAboutClass].
	^description printString.


]

{ #category : 'category' }
JadeServer >> commit [

	classOrganizers := Array new: 4.
	^System commitTransaction.


]

{ #category : 'category' }
JadeServer >> compile: aString frame: anInteger process: aGsProcess [
	"Compile method from within debugger"

	| oldMethod aBehavior selector category result |
	oldMethod := aGsProcess localMethodAt: anInteger.
	result := self recompile: oldMethod withSource: aString.
	(result isKindOf: Boolean) ifTrue: [^result].
	aBehavior := oldMethod inClass.
	selector := oldMethod selector.
	selector isNil ifTrue: [^result].
	category := self _behavior: aBehavior categoryOfSelector: selector.
	result := [[ aBehavior rwCompileMethod: aString category: category ]
							on: RwExecuteClassInitializeMethodsAfterLoadNotification
							do: [:ex | ex resume: false ]] 
								on: RwPerformingUnpackagedEditNotification
								do: [:ex | ex resume ].

					
	^result
]

{ #category : 'category' }
JadeServer >> compiledMethodAt: aSymbol inClass: aClass [

	^aClass compiledMethodAt: aSymbol.


]

{ #category : 'category' }
JadeServer >> compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol [
	"Returns aGsNMethod (if successful) -> anArrayOfErrorsOrWarnings"

	| result |
	"Method is in GsFoundation, but not in GsBase"
	result := (aBehavior class canUnderstand: #'compileMethod:category:using:environmentId:') ifTrue: [
		[
			aBehavior		"returns self or signals a CompileError"
				compileMethod: methodString
				category: categorySymbol
				using: aSymbolList
				environmentId: 0.
			nil.
		] on: (self objectInBaseNamed: #'UndefinedSymbolNotification') do: [:ex | 
			ex resume: false.
		].
	] ifFalse: [(aBehavior class canUnderstand: #'compileMethod:category:using:') ifTrue: [
		[
			aBehavior		"returns self or signals a CompileError"
				compileMethod: methodString
				category: categorySymbol
				using: aSymbolList.
			nil.
		] on: (self objectInBaseNamed: #'UndefinedSymbolNotification') do: [:ex | 
			ex resume: false.
		].
	] ifFalse: [
		aBehavior		"returns nil or an Array of error descriptions"
			compileMethod: methodString
			dictionaries: aSymbolList
			category: categorySymbol.
	]].
	result notNil ifTrue: [
		^nil -> result.
	].
	(aBehavior class canUnderstand: #_primitiveCompileMethod:symbolList:category:oldLitVars:intoMethodDict:intoCategories:intoPragmas:) ifTrue: [
		result := aBehavior 
			_primitiveCompileMethod: methodString
			symbolList: aSymbolList
			category: categorySymbol
			oldLitVars: nil
			intoMethodDict: GsMethodDictionary new 
			intoCategories: GsMethodDictionary new
			intoPragmas: nil.
	] ifFalse: [
		(aBehavior class canUnderstand: #_primitiveCompileMethod:symbolList:category:obsoleteClassNames:oldLitVars:) ifTrue: [
			result := aBehavior 
				_primitiveCompileMethod: methodString
				symbolList: aSymbolList
				category: categorySymbol
				obsoleteClassNames: nil
				oldLitVars: nil.
		] ifFalse: [
			result := aBehavior 
				_primitiveCompileMethod: methodString
				symbolList: aSymbolList
				category: categorySymbol
				oldLitVars: nil
				intoMethodDict: GsMethodDictionary new 
				intoCategories: GsMethodDictionary new.
		].
	].
	(result isKindOf: Array) ifTrue: [
		"in 2.3.x: (Array with: compiledMethod with: errors with: warnings)"
		(result at: 2) notNil ifTrue: [^nil -> (result at: 2)].
		^(result at: 1) -> (result at: 3)
	].
	^result -> nil.


]

{ #category : 'category' }
JadeServer >> compileMethod: methodString behavior: aBehavior user: aUserProfileOrNil inCategory: categoryString [
	"answers a String:
		OOP of new method <TAB> selector of new method (or empty if compile failed)
		ERROR: [details] (repeat line for each error)
		[warnings] (if no errors)"

	| userProfile result gsMethod stream errDict errorList warnings |

	userProfile := aUserProfileOrNil isNil
		ifTrue: [System myUserProfile]
		ifFalse: [aUserProfileOrNil].
	result := self 		"key: GsNMethod value: ((Array withAll: errors) or aStringOfWarnings)"
		compileMethod: methodString 
		behavior: aBehavior 
		symbolList: userProfile symbolList 
		inCategory: categoryString asSymbol.
	(gsMethod := result key) isNil ifTrue: [
		errorList := result value.
		warnings := ''.
	] ifFalse: [
		errorList := #().
		warnings := result value.
	].
	stream := WriteStream on: String new.
	gsMethod notNil ifTrue: [
		stream 
			nextPutAll: gsMethod asOop printString;
			tab;
			nextPutAll: gsMethod selector;
			yourself.
	].
	errDict := GemStoneError at: System myUserProfile nativeLanguage.
	errorList do: [:each |
		stream lf; 
			nextPutAll: 'ERROR:'; tab;
			nextPutAll: (each at: 1) printString; tab;
			nextPutAll: (each at: 2) printString; tab;
			yourself.
		(each size >= 3 and: [(each at: 3) notNil]) ifTrue: [
			stream nextPutAll: (each at: 3); tab.
		] ifFalse: [
			(each at: 1) > errDict size ifTrue: [
				stream nextPutAll: '(unknown error number)'; tab.
			] ifFalse: [
				stream nextPutAll: (errDict at: (each at: 1)) asString; tab.
			].
		].
	].
	warnings isNil ifTrue: [warnings := ''].
	stream lf; nextPutAll: warnings.
	^stream contents.


]

{ #category : 'category' }
JadeServer >> contents [
	"WriteStream method to identify things that have not yet been flushed to the output. We have flushed everything!"

	^''.


]

{ #category : 'category' }
JadeServer >> cr [

	self nextPut: Character cr.


]

{ #category : 'category' }
JadeServer >> currentUserMayEditMethod: aMethod [

	^true

]

{ #category : 'category' }
JadeServer >> debugString: aString fromContext: anObject environment: anInteger [
	anInteger == 0 ifFalse: [self error: 'Only environment 0 is supported in this version!'].
	^(RowanDebuggerService new debugStringFrom: aString)
		evaluateInContext: anObject 
		symbolList: GsSession currentSession symbolList.
]

{ #category : 'category' }
JadeServer >> debugTestNamed: testName of: testCaseClassName [
	"Open a Debugger for the TestCase class named <testCaseClassName> in the test named <testName>"

	^(self objectNamed: testCaseClassName) debug: testName asSymbol

]

{ #category : 'category' }
JadeServer >> defectiveTestsIn: aClass [

	| testClass results stream |
	testClass := aClass thisClass.
	results := testClass suite run.
	stream := WriteStream on: String new.
	stream nextPutAll: results printString; lf.
	results defects asSet do: [:each | 
		| selector class |
		selector := each selector asSymbol.
		class := each class whichClassIncludesSelector: selector.
		stream nextPutAll: class name , ' debug: ' , each selector printString; lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> definitionOfClass: aClass [
	^aClass definition.

]

{ #category : 'category' }
JadeServer >> definitionOfClass: aClass forUser: aUserProfile [

			| stream |
			stream := WriteStream on: String new.
"1"		aClass superclass printOn: stream.
			stream 
"2"			lf; nextPutAll: (self subclassSelectorForClass: aClass);
"3"			lf; nextPutAll: aClass name;
"4"			lf; nextPutAll: (self dictionaryForClass: aClass forUser: aUserProfile);
				yourself.
"5"		stream lf. aClass instancesInvariant printOn: stream.
"6"		stream lf. aClass isModifiable printOn: stream.
"7"		stream lf. ((aClass class canUnderstand: #'instancesDbTransient') and: [aClass instancesDbTransient]) printOn: stream.
"8"		stream lf. ((aClass class canUnderstand: #'instancesNonPersistent') and: [aClass instancesNonPersistent]) printOn: stream.
			stream lf.
		aClass instVarNames do: [:each |
				stream 
"9.*.1"		nextPutAll: each; 
					space;
"9.*.2"		nextPutAll: (aClass constraintOfInstVar: each) name;
					tab.
			].
			stream lf.
			aClass class instVarNames do: [:each | 
"10.*"		stream nextPutAll: each; tab.
			].
			stream lf.
			aClass classVarNames asSortedCollection do: [:each | 
"11.*"		stream nextPutAll: each; tab.
			].
			stream lf.
			aClass sharedPools asSortedCollection do: [:each | 
"12.*"		stream nextPutAll: (self nameForSharedPool: each forUser: aUserProfile); tab.
			].
			^stream 
"13"		lf; nextPutAll: aClass userId;
"14"		lf; nextPutAll: (aClass timeStamp asStringUsingFormat: #(3 2 1 $- 1 1 $: true true false));
				lf; 
				contents.


]

{ #category : 'category' }
JadeServer >> delay [

	(Delay forMilliseconds: 10) wait.


]

{ #category : 'category' }
JadeServer >> describeMethod: aMethod [
	"Provide info needed to create a GsMethod in Jade client"

	writeStream := WriteStream on: String new.
	self _describeMethod: (self homeMethodFor: aMethod).
	^writeStream contents


]

{ #category : 'category' }
JadeServer >> descriptionOfConfigOption: aString [

	| dict key string |
	dict := self systemConfigAsDictionary.
	(string := dict at: aString ifAbsent: [nil]) notNil ifTrue: [^string].	string := aString asUppercase.
	dict keys do: [:each1 | 
		key := (each1 reject: [:each2 | each2 = $_]) asUppercase.
		key = string ifTrue: [^dict at: each1].
	].
	^''

]

{ #category : 'category' }
JadeServer >> descriptionOfErrorNumber: anInteger [

	| array stream |
	array := GemStoneError at: #'English'.
	anInteger <= 0 ifTrue: [^'Invalid number!'].
	array size < anInteger ifTrue: [^'Invalid number!'].
	stream := WriteStream on: String new.
	array := array at: anInteger.
	array isNil ifTrue: [^'No entry in GemStoneError for #' , anInteger printString , '!'].
	(array isKindOf: String) ifTrue: [array := Array with: array].
	array do: [:each | 
		(each isKindOf: Integer) ifTrue: [
			stream space; nextPut: $%.
			each printOn: stream.
		] ifFalse: [
			stream nextPutAll: each.
		].
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> dictionaryAndSymbolOf: aClass [

	^self symbolList dictionaryAndSymbolOf: aClass.


]

{ #category : 'category' }
JadeServer >> dictionaryAndSymbolOf: aClass forUser: aUserProfile [

	^aUserProfile symbolList dictionaryAndSymbolOf: aClass.


]

{ #category : 'category' }
JadeServer >> dictionaryForClass: aClass forUser: aUserProfile [

	| anArray |
	anArray := self dictionaryAndSymbolOf: aClass forUser: aUserProfile.
	anArray isNil ifTrue: [^''].
	anArray := self dictionaryAndSymbolOf: (anArray at: 1) forUser: aUserProfile.
	anArray isNil ifTrue: [^''].
	^(anArray at: 2)


]

{ #category : 'category' }
JadeServer >> dictionaryListFor: aUserProfile [

	| symbolList list stream |
	symbolList := aUserProfile symbolList.
	list := symbolList namesReport subStrings: Character lf.
	list := list reject: [:each | each isEmpty].
	list := list collect: [:each | each subStrings].
	stream := WriteStream on: String new.
	list do: [:each | 
		(self oopOf: (symbolList at: (each at: 1) asNumber)) printOn: stream.
		stream tab; nextPutAll: (each at: 2); lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> does: newClass replace: oldClass [

	^newClass name = oldClass name.


]

{ #category : 'category' }
JadeServer >> environment [
	"Ignored prior to 3.x"

	^0

]

{ #category : 'category' }
JadeServer >> environment: anInteger [
	"Ignored prior to 3.x"

]

{ #category : 'category' }
JadeServer >> environmentForMethod: aGsNMethod [

	^0

]

{ #category : 'category' }
JadeServer >> environmentSuperClassFor: aBehavior [

	^aBehavior superclass

]

{ #category : 'category' }
JadeServer >> errorListFor: aCollection [

	| stream |
	aCollection class name == #'ErrorDescription' ifTrue: [^''].
	stream := WriteStream on: String new.
	aCollection do: [:each | 
		stream
			nextPutAll: (each at: 1) printString; tab;
			nextPutAll: (each at: 2) printString; tab;
			nextPutAll: ((2 < each size and: [(each at: 3) notNil]) ifTrue: [(each at: 3)] ifFalse: [(GemStoneError at: #English) at: (each at: 1)]); tab;
			lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> executeString: aString fromContext: anObject environment: anInteger [

	anInteger == 0 ifFalse: [self error: 'Only environment 0 is supported in this version!'].
	^aString
		evaluateInContext: anObject 
		symbolList: GsSession currentSession symbolList. 

]

{ #category : 'category' }
JadeServer >> fileInClass: aString [

	| list className index dictionaryName dictionary oldClass oldString |
	list := aString subStrings.
	className := list at: 3.
	className first = $' ifFalse: [self error: 'Class name ' , className printString , ' expected to begin and end with a quote!'].
	className last = $' ifFalse: [self error: 'Class name ' , className printString , ' expected to begin and end with a quote!'].
	className := className copyFrom: 2 to: className size - 1.
	index := list indexOf: 'inDictionary:'.
	dictionaryName := list at: index + 1.
	dictionary := self objectNamed: dictionaryName.
	oldClass := dictionary at: className ifAbsent: [nil].
	oldClass notNil ifTrue: [
		oldString := (oldClass _modifiableDefinitionInDictionary: dictionary named: dictionaryName) , '.'.
	].
	oldString = aString ifFalse: [aString evaluate].


]

{ #category : 'category' }
JadeServer >> fileOutForClass: aClass [

	^aClass thisClass fileOutClass.
 

]

{ #category : 'category' }
JadeServer >> gemLogPath [

	^''


]

{ #category : 'category' }
JadeServer >> getMethodsNamesOf: classNamed [
	"NO SENDERS"
	"Answer a collection with selectors of GemStone/s class name <className>"

	^(self objectNamed: classNamed) selectors asSortedCollection


]

{ #category : 'category' }
JadeServer >> getSubclassesNamesOf: classNamed [ 
	"NO SENDERS"
	"Answer a collection with all subclasses of GemStone/S class named <classNamed>"

	^((self objectNamed: classNamed) allSubclasses collect: [:each | each name]) asSortedCollection

]

{ #category : 'category' }
JadeServer >> getTestCaseClassesNamesOf: packageName [
	"Answer a collection with all test classes of the GemStone/S package named <packageNamed>"

	| classes comma stream |
	packageName isEmpty ifTrue: [^''].
	classes := (self objectInBaseNamed: #'TestCase') allSubclasses.
	packageName = '<All>' ifFalse: [
		| packageInfo packageOrganizer |
		packageOrganizer := (self objectInBaseNamed: #'PackageOrganizer') default.
		packageInfo := packageOrganizer packageNamed: packageName ifAbsent: [^''].
		packageInfo classes isEmpty ifTrue: [^''].
		classes := packageInfo classes select: [:each | classes includes: each].
	].
	stream := WriteStream on: String new.
	comma := ''.
	classes do: [:each | stream nextPutAll: comma; nextPutAll: each name. comma := ','].
	^stream contents

]

{ #category : 'category' }
JadeServer >> getTestMethodsNamesOf: classNamed [ 
	"Answer a collection with all test selectors of the GemStone/S class named <classNamed>"

	| class comma stream testSelectors gsClass |
	stream := WriteStream on: String new.
	gsClass := self objectNamed: classNamed.
	gsClass isNil ifTrue: [^''].
	(class := self objectNamed: #'TestCase') isNil ifTrue: [^''].
	(gsClass isSubclassOf: class) ifFalse: [^''].
	testSelectors :=  gsClass testSelectors asSortedCollection.
	testSelectors isEmpty ifTrue: [^''].
	comma := ''.
	 testSelectors do: [:each | stream nextPutAll: comma; nextPutAll: each. comma := ','].
	^stream contents

]

{ #category : 'category' }
JadeServer >> globalsFor: aSymbolDictionary [

	| stream |
	stream := WriteStream on: String new.
	aSymbolDictionary keysAndValuesDo: [:eachKey :eachValue | 
		eachValue isBehavior ifFalse: [
			| data |
			data := (self _oopAndStringFor: eachValue) value.
			data size > 200 ifTrue: [data := data copyFrom: 1 to: 200].
			data := String withAll: (data asArray collect: [:each | (each >= Character space and: [each <= $~]) ifTrue: [each] ifFalse: [$?]]).
			stream
	"1"		nextPutAll: (self oopOf: eachValue) printString; tab;
	"2"		nextPutAll: eachKey; tab;
	"3"		nextPutAll: eachValue class name; tab;
	"4"		nextPutAll: data; tab;
				lf;
				yourself.
		].
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> groupListFor: aUserProfile [

	| allGroups myGroups stream |
	allGroups := AllGroups keys asSortedCollection.
	myGroups := aUserProfile groups.
	stream := WriteStream on: String new.
	allGroups do: [:each | 
		stream nextPutAll: each; tab.
		(myGroups includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.


]

{ #category : 'jadeite' }
JadeServer >> gsInteractionInformFailureHandler [
  self interactionHandlerActive
    ifFalse: [ 
      ^ GsInteractionHandler new
        defaultBlock: [ :ignored | self assert: false description: 'expected a confirmation' ];
        confirmBlock: [ :interaction | interaction ok ];
        informBlock: [ :interaction |  ];
        inspectBlock: [ :interaction |  ];
        yourself ].
  ^ GsInteractionHandler new
    confirmBlock: [ :interaction | 
          | exception answer |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'confirmMessageBox:'
            args: (Array with: interaction prompt).
          answer := exception defaultAction.	"expect printString of answer back. Jadeite has limited ability to convert client objects to oops"
          answer evaluate ];
    informBlock: [ :interaction | 
          | exception |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'informMessageBox:'
            args: (Array with: interaction message).
          exception defaultAction.
          nil ];
    inspectBlock: [ :interaction | 
          | exception |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'inspectServerObject:'
            args: (Array with: interaction theObject asOop).
          exception defaultAction.
          interaction theObject ]
]

{ #category : 'category' }
JadeServer >> gsPackagePolicy [

	| class |
	class := self gsPackagePolicyClass.
	class isNil ifTrue: [^nil].
	^class current.


]

{ #category : 'category' }
JadeServer >> gsPackagePolicyClass [

	^self objectInBaseNamed: #'GsPackagePolicy'.


]

{ #category : 'category' }
JadeServer >> historyOf: aClass [

	| history |
	(history := aClass classHistory) isNil ifTrue: [
		history := Array with: aClass.
	].
	^history.


]

{ #category : 'category' }
JadeServer >> homeMethodFor: aGsMethod [

	^aGsMethod

]

{ #category : 'category' }
JadeServer >> implementorsOf: anObject [

	| symbol |
	symbol := (anObject isKindOf: String)
		ifTrue: [anObject asSymbol]
		ifFalse: [anObject selector].
	^self streamOfMethods: (self classOrganizer implementorsOf: symbol).


]

{ #category : 'category' }
JadeServer >> implementorsOf: aGsMethod startingAt: aClass [

	| selector myClass list |
	selector := aGsMethod selector.
	myClass := aClass.
	list := OrderedCollection new.
	[
		(myClass includesSelector: selector) ifTrue: [list add: myClass].
		(myClass := myClass superclass) notNil.
	] whileTrue: [].
	^self stringForClassList: list.


]

{ #category : 'category' }
JadeServer >> initialize [
	"#installTranscript is run from the Jadeite
	client post login method only. This avoids
	setting up ClientForwarder sends in a topaz
	session"

	classOrganizers := Array new: 4.
	self 
		registerOBNotifications;
		yourself.
]

{ #category : 'category' }
JadeServer >> inspect: anObject [

	| stream string |
	(stream := WriteStream on: String new)
		nextPutAll: anObject class name; tab;
		yourself.
	(self oopOf: anObject) printOn: stream.
	stream lf.
	(anObject isKindOf: Dictionary superclass) ifTrue: [^self inspectDictionary: anObject on: stream].
	self inspectNamedInstanceVariablesOf: anObject on: stream.
	anObject class format > 0 ifTrue: [
		1 to: (anObject _basicSize) do: [:i | 
			i printOn: stream.
			stream tab.
			self print: (self oopOf: (anObject _at: i)) on: stream.
			stream lf.
		].
	].
	(string := anObject printString) size > 5000 ifTrue: [string := (string copyFrom: 1 to: 5000) , '...'].
	string class == String ifFalse: [
		string := String withAll: (string collect: [:each | (32 <= each asciiValue and: [each asciiValue <= 255]) ifTrue: [each] ifFalse: [$?]]).
	].
	^stream 
		nextPutAll: string; 
		contents.
]

{ #category : 'category' }
JadeServer >> inspectDictionary: aDictionary on: aStream [

	| keys keyDict |
	keys := self keysForDictionary: aDictionary.
	keyDict := Dictionary new.
	keys do: [:each | 
		| key |
		key := each printString , '~' , (self oopOf: each) printString.
		key := key collect: [:char | char asciiValue < 32 ifTrue: [$?] ifFalse: [char]].
		keyDict
			at: key
			put: each.
	].
	keys size printOn: aStream.
	aStream lf.
	keyDict keys asSortedCollection do: [:each | 
		| index keyString key value valueString |
		index := each findLast: [:char | char = $~].
		keyString := each copyFrom: 1 to: index - 1.
		keyString charSize = 1 ifFalse:[
			keyString := '<<unprintable key. charSize > 1>>']. 
		key := keyDict at: each.
		value := aDictionary at: key. 
		valueString := (self printStringOf: value to: 10).
		valueString charSize = 1 ifFalse:[
			valueString := '<<unprintable value. charSize > 1>>']. 
		aStream nextPutAll: keyString , '->' , valueString; tab.
		self print: (self oopOf: value) on: aStream.
		aStream lf.
	].
	^aStream 
		lf; 
		contents.
]

{ #category : 'category' }
JadeServer >> inspectNamedInstanceVariablesOf: anObject on: aStream [

	| list size |
	list := anObject class allInstVarNames.
	size := list size.
	anObject class format > 0 ifTrue: [
		size := size + (anObject _basicSize min: 200).
	].
	size printOn: aStream.
	aStream lf.
	1 to: list size do: [:i | 
		aStream nextPutAll: (list at: i); tab.
		self print: (self oopOf: (anObject instVarAt: i)) on: aStream.
		aStream lf.
	].


]

{ #category : 'category' }
JadeServer >> installTranscript [

	| transcript |
	transcript := self objectInBaseNamed: #'Transcript'.

	"If no Transcript object, then install me!"
	(transcript == nil or: [transcript class name == self class name]) ifTrue: [
		UserGlobals at: #'Transcript' put: self.
		System commitTransaction.
		^self.
	].
	
	"Transcript object from Seaside"
	transcript class name = 'TranscriptProxy class' ifTrue: [
		| clientForwarder |
		clientForwarder := ClientForwarder new.
		clientForwarder	clientObject: 2.
		transcript registerTranscriptClientForwarder: clientForwarder.
		^self.
	].


]

{ #category : 'jadeite' }
JadeServer >> interactionHandlerActive [
  ^ SessionTemps current at: #'rowanServiceInteractionActive' ifAbsent: [ true ]
]

{ #category : 'category' }
JadeServer >> is32Bit [

	^false.


]

{ #category : 'category' }
JadeServer >> isClientForwarder: anObject [

	^anObject _class name == #'ClientForwarder'.


]

{ #category : 'category' }
JadeServer >> isPackagePolicyEnabled [

	^self gsPackagePolicy notNil

]

{ #category : 'category' }
JadeServer >> isResumableCategory: category number: number context: context [

	| exceptionA receiver |
	category == GemStoneError ifTrue: [
		^number // 1000 = 2 or: [number // 1000 = 6].
	].
	(exceptionA := Globals at: #ExceptionA ifAbsent: [nil]) isNil ifTrue: [
		^true.
	].
	receiver := (context _frameContentsAt: 1) at: 8.
	(receiver isKindOf: exceptionA) ifTrue: [
		^receiver isResumable.
	].
	^true.


]

{ #category : 'category' }
JadeServer >> keysForDictionary: aDictionary [ 

	^aDictionary keys.


]

{ #category : 'category' }
JadeServer >> makeListener [

	^nil.


]

{ #category : 'category' }
JadeServer >> mcAddHttpRepository: aString [

	| list repositoryClass repository group |
	list := aString subStrings: (Character codePoint: 255).
	(repositoryClass := self mcHttpRepositoryClass) isNil ifTrue: [self error: 'MCHttpRepository not found!'].
	repository := repositoryClass
		location: (list at: 1)
		user: (list at: 2)
		password: (list at: 3).
	(group := self mcRepositoryGroup) isNil ifTrue: [self error: 'MCRepositoryGroup not found!'].
	group addRepository: repository.
	^repository

]

{ #category : 'category' }
JadeServer >> mcAddPackage: aString [

	self mcWorkingCopyClass forPackage: (self mcPackageClass named: aString).


]

{ #category : 'category' }
JadeServer >> mcAddRepository: aRepository toPackage: aMCWorkingCopy [

	aMCWorkingCopy repositoryGroup addRepository: aRepository.


]

{ #category : 'category' }
JadeServer >> mcAllFileNamesIn: anMCRepository [

	| stream |
	stream := WriteStream on: String new.
	anMCRepository allFileNames do: [:each | 
		stream nextPutAll: each; lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> mcAllVersionInfoNamesIn: anMCRepository [

	| stream |
	stream := WriteStream on: String new.
	anMCRepository allVersionInfos do: [:each | 
		stream nextPutAll: each name; lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> mcAllVersionNamesInDict: anMCRepository [

	| stream list |
	stream := WriteStream on: String new.
	list := anMCRepository dictionary values.
	list := list asSortedCollection: [:a :b | 
		a package name < b package name or: [
		a package name = b package name and: [
		a info date > b info date or: [
		a info date = b info date and: [
		a info time > b info time
	]]]]].
	stream := WriteStream on: String new.
	list do: [:each | 
		stream nextPutAll: each info name; lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> mcClassesInCategory: aString package: aMCWorkingCopy [

	| visibleClasses allClasses stream queue |
	visibleClasses := aString isNil ifTrue: [
		aMCWorkingCopy packageInfo classes.
	] ifFalse: [
		aMCWorkingCopy packageInfo classes select: [:each | 
			each _classCategory notNil and: [
			each _classCategory = aString or: [
			aString notNil and: [each _classCategory matchPattern: (Array with: aString with: $*)]]]]
	].
	allClasses := visibleClasses asIdentitySet.
	queue := visibleClasses asOrderedCollection.
	[
		queue notEmpty.
	] whileTrue: [
		| parent |
		parent := queue removeFirst superclass.
		(parent notNil and: [(allClasses includes: parent) not]) ifTrue: [
			queue add: parent.
			allClasses add: parent.
		].
	].
	stream := WriteStream on: String new.
	allClasses do: [:each |
		self
			_addClass: each 
			toStream: stream 
			isVisible: (visibleClasses includes: each)
			fromDictionary: nil.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> mcCreationTemplateFor: anMCRepository [

	^anMCRepository asCreationTemplate.


]

{ #category : 'category' }
JadeServer >> mcHttpRepository [

	^self objectInBaseNamed: #'MCHttpRepository'.


]

{ #category : 'category' }
JadeServer >> mcHttpRepository: aRepository user: userString password: passwordString [

	aRepository
		user: userString;
		password: passwordString;
		yourself.


]

{ #category : 'category' }
JadeServer >> mcHttpRepositoryClass [

	^self objectInBaseNamed: #'MCHttpRepository'.


]

{ #category : 'category' }
JadeServer >> mcInitials: aString [
	"Do initial setup and return useful information"

	| mcPlatformSupport packagePolicyEnabledFlag string x |
	string := 'Jade-' , GsSession currentSession serialNumber printString , '-' , System myUserProfile userId.
	[
		self mcInitialsA: string.
	] whileFalse: [	"Keep shortening it till it fits!"
		string := string copyFrom: 1 to: string size - 1.
	].
	mcPlatformSupport := self objectInBaseNamed: #'MCPlatformSupport'.
	mcPlatformSupport notNil ifTrue: [mcPlatformSupport setAuthorInitials: aString].
	packagePolicyEnabledFlag := (x := self objectInBaseNamed: #'GsPackagePolicy') isNil ifTrue: ['0'] ifFalse: [x current enabled ifTrue: ['1'] ifFalse: ['0']].
	^System session printString , Character space asString , 
		(GsSession serialOfSession: System session) printString , Character space asString , 
		packagePolicyEnabledFlag


]

{ #category : 'category' }
JadeServer >> mcInitialsA: aString [
	"Subclasses provide error handling, typically means string is too long"

	System _cacheName: aString.


]

{ #category : 'category' }
JadeServer >> mcLoadedVersionNames [

	| mcWorkingCopyClass stream |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^nil].
	stream := WriteStream on: String new.
	mcWorkingCopyClass allManagers do: [:each | 
		| packageOrVersion |
		packageOrVersion := each ancestors
			detect: [:ignored | true]
			ifNone: [each package].
		packageOrVersion := packageOrVersion notNil
			ifTrue: [packageOrVersion name]
			ifFalse: [''].
		stream
			nextPutAll: packageOrVersion; tab;
			nextPut: (each modified ifTrue: [$Y] ifFalse: [$N]); tab;
			nextPutAll: each package name;
			lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> mcNewDirectoryRepository: aString [

	| mcRepositoryClass fileDirectoryClass repository |
	(mcRepositoryClass := self objectInBaseNamed: #'MCDirectoryRepository') isNil ifTrue: [self error: 'Monticello not available!'].
	(fileDirectoryClass := self objectInBaseNamed: #'FileDirectory') isNil ifTrue: [self error: 'Monticello not available!'].
	repository := mcRepositoryClass new directory: (fileDirectoryClass on: aString).
	self mcRepositoryGroup addRepository: repository.
	^repository

]

{ #category : 'category' }
JadeServer >> mcNewFileTreeRepository: aString [

	| mcRepositoryClass fileDirectoryClass repository |
	(mcRepositoryClass := self objectInBaseNamed: #'MCFileTreeRepository') isNil ifTrue: [self error: 'Monticello not available!'].
	(fileDirectoryClass := self objectInBaseNamed: #'ServerFileDirectory') isNil ifTrue: [self error: 'Monticello not available!'].
	repository := mcRepositoryClass new directory: (fileDirectoryClass on: aString).
	self mcRepositoryGroup addRepository: repository.
	^repository

]

{ #category : 'category' }
JadeServer >> mcNewGitHubRepository: aString [

	| mcRepositoryClass repository |
	(mcRepositoryClass := self objectInBaseNamed: #'MCGitHubRepository') isNil ifTrue: [self error: 'Monticello not available!'].
	repository := mcRepositoryClass location: aString.
	self mcRepositoryGroup addRepository: repository.
	^mcRepositoryClass

]

{ #category : 'category' }
JadeServer >> mcNewServerDirectoryRepository: aString [

	| mcDirectoryRepositoryClass fileDirectoryClass repository |
	(mcDirectoryRepositoryClass := self objectInBaseNamed: #'MCServerDirectoryRepository') isNil ifTrue: [self error: 'Monticello not available!'].
	(fileDirectoryClass := self objectInBaseNamed: #'ServerFileDirectory') isNil ifTrue: [self error: 'Monticello not available!'].
	repository := mcDirectoryRepositoryClass new directory: (fileDirectoryClass on: aString).
	self mcRepositoryGroup addRepository: repository.
	^repository

]

{ #category : 'category' }
JadeServer >> mcPackageClass [

	^self objectInBaseNamed: #'MCPackage'.


]

{ #category : 'category' }
JadeServer >> mcPatchFrom: aString1 to: aString2 inFileBasedRepository: aFileRepository [

	| index name leftSnapshot rightSnapshot patch |
	index := aString2 findLast: [:each | each = $-].
	name := aString2 copyFrom: 1 to: index - 1.
	(name includes: $.) ifTrue: [name := (name subStrings: $.) first].
	leftSnapshot := aString1 isNil ifTrue: [
		(self mcWorkingCopyClass allManagers detect: [:each | each package name = name]) package snapshot.
	] ifFalse: [
		(aFileRepository versionFromFileNamed: aString1) snapshot.
	].
	rightSnapshot := (aFileRepository versionFromFileNamed: aString2) snapshot.
	patch := rightSnapshot patchRelativeToBase: leftSnapshot.
	^self 
		_mcDescriptionOfPatch: patch
		baseName: aString1
		alternateName: aString2.


]

{ #category : 'category' }
JadeServer >> mcputDefinition: aDefinition on: aStream [

	| mcOrganizationDefinitionClass mcClassDefinitionClass mcMethodDefinitionClass |
	(mcOrganizationDefinitionClass := self objectInBaseNamed: 'MCOrganizationDefinition') isNil ifTrue: [^nil].
	(mcClassDefinitionClass := self objectInBaseNamed: 'MCClassDefinitionClass') isNil ifTrue: [^nil].
	(mcMethodDefinitionClass := self objectInBaseNamed: 'MCMethodDefinition') isNil ifTrue: [^nil].

	self _addToPureExportSet: aDefinition.
		aStream nextPutAll: (self oopOf: aDefinition) printString; tab;
			nextPutAll: aDefinition class name; tab.
			
		aDefinition class == mcOrganizationDefinitionClass ifTrue: [
			aDefinition categories do: [:eachCategory | 
				aStream nextPutAll: eachCategory; space]
		] ifFalse: [	aDefinition class == mcClassDefinitionClass ifTrue: [
			aStream
				nextPutAll: aDefinition className; tab;
				nextPutAll: aDefinition superclassName; tab;
				nextPutAll: aDefinition category; tab;
				nextPutAll: aDefinition type; tab;
				yourself.
		] ifFalse: [aDefinition class == mcMethodDefinitionClass ifTrue: [
			aStream
				nextPutAll: aDefinition classIsMeta printString; tab;
				nextPutAll: aDefinition category; tab;
				nextPutAll: aDefinition selector; tab;
				nextPutAll: aDefinition className; tab;
				nextPutAll: aDefinition timeStamp printString; tab]]].
	
	^aStream.
	

]

{ #category : 'category' }
JadeServer >> mcRemoveRepository: aRepository [

	| repositoryClass group |
	(repositoryClass := self mcHttpRepositoryClass) isNil ifTrue: [self error: 'MCHttpRepository not found!'].
	(group := self mcRepositoryGroup) isNil ifTrue: [self error: 'MCRepositoryGroup not found!'].
	group removeRepository: aRepository.


]

{ #category : 'category' }
JadeServer >> mcRemoveRepository: aRepository toPackage: aMCWorkingCopy [

	aMCWorkingCopy repositoryGroup removeRepository: aRepository.


]

{ #category : 'category' }
JadeServer >> mcRepositoryFrom: aRepository [

	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aRepository) printOn: stream.
	stream 
		tab;
		nextPutAll: aRepository description;
		tab;
		nextPutAll: aRepository class name;
		tab.
	^stream contents.


]

{ #category : 'category' }
JadeServer >> mcRepositoryGroup [

	| groupClass |
	(groupClass := self objectInBaseNamed: 'MCRepositoryGroup') isNil ifTrue: [^nil].
	^groupClass default.


]

{ #category : 'category' }
JadeServer >> mcRepositoryList [

	| group stream |
	(group := self mcRepositoryGroup) isNil ifTrue: [^nil].
	stream := WriteStream on: String new.
	group repositories do: [:each | 
		stream nextPutAll: (self mcRepositoryFrom: each).
		stream lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> mcTopazFrom: aString inDictionaryRepository: aDictionaryRepository [

	| snapshot stream |
	snapshot := (aDictionaryRepository versionFromVersionNamed: aString) snapshot.
	stream := (WriteStream on: String new)
		nextPutAll: '! ' , aString , ' in ' , aDictionaryRepository printString; lf;
		yourself.
	self
		_mcTopazFrom: snapshot
		on: stream.
	^stream contents.



]

{ #category : 'category' }
JadeServer >> mcTopazFrom: aString inFileRepository: aFileRepository [

	| snapshot stream |
	snapshot := (aFileRepository versionFromFileNamed: aString) snapshot.
	stream := (WriteStream on: String new)
		nextPutAll: '! ' , aString , ' in ' , aFileRepository printString; lf;
		yourself.
	self
		_mcTopazFrom: snapshot
		on: stream.
	^stream contents.



]

{ #category : 'category' }
JadeServer >> mcUniqueVersionNameFor: anMCWorkingCopy [

	^anMCWorkingCopy uniqueVersionName.


]

{ #category : 'category' }
JadeServer >> mcUserAndPasswordInHTTP: anMCHttpRepository [

	^anMCHttpRepository user , Character tab asString , anMCHttpRepository password.


]

{ #category : 'category' }
JadeServer >> mcVersionInfoFrom: aVersionInfo [

	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aVersionInfo) printOn: stream.
	stream 
		lf; nextPutAll: aVersionInfo name; 
		lf; nextPutAll: aVersionInfo date yyyymmdd;
		lf.
	aVersionInfo time printOn: stream.
	stream 
		lf; nextPutAll: aVersionInfo author; 
		lf; nextPutAll: aVersionInfo id asString;
		lf.
	aVersionInfo ancestors do: [:each | 
		stream nextPutAll: each name; tab.
	].
	stream lf.
	aVersionInfo stepChildren do: [:each | 
		stream nextPutAll: each name; tab.
	].
	stream lf.
	stream nextPutAll: aVersionInfo message.
	^stream contents.


]

{ #category : 'category' }
JadeServer >> mcVersionInfoFromDictionaryPackageNamed: aString in: anMCDictionaryRepository [

	| versionInfo |
	(versionInfo := anMCDictionaryRepository versionInfoFromVersionNamed: aString) isNil ifTrue: [^''].
	^self mcVersionInfoFrom: versionInfo.


]

{ #category : 'category' }
JadeServer >> mcVersionInfoFromFileNamed: aString in: anMCFileBasedRepository [

	| versionInfo |
	(versionInfo := anMCFileBasedRepository versionInfoFromFileNamed: aString) isNil ifTrue: [^''].
	^self mcVersionInfoFrom: versionInfo.


]

{ #category : 'category' }
JadeServer >> mcVersionLoad: aString fromDictionary: anMCDictionaryRepository autoMigrate: aBoolean [

	| version package workingCopy mcPlatformSupport autoMigrate |
	mcPlatformSupport := self objectInBaseNamed: #'MCPlatformSupport'.
	autoMigrate := mcPlatformSupport autoMigrate.
	mcPlatformSupport autoMigrate: aBoolean.
	version := anMCDictionaryRepository versionFromVersionNamed: aString.
	version load.
	package := version package.
	workingCopy := self mcWorkingCopyClass forPackage: package.
	workingCopy repositoryGroup addRepository: anMCDictionaryRepository.
	mcPlatformSupport autoMigrate: autoMigrate.


]

{ #category : 'category' }
JadeServer >> mcVersionLoad: aString fromFile: anMCFileBasedRepository autoMigrate: aBoolean [

	| version package workingCopy mcPlatformSupport autoMigrate |
	mcPlatformSupport := self objectInBaseNamed: #'MCPlatformSupport'.
	autoMigrate := mcPlatformSupport autoMigrate.
	mcPlatformSupport autoMigrate: aBoolean.
	version := anMCFileBasedRepository loadVersionFromFileNamed: aString.
	version load.
	package := version package.
	workingCopy := self mcWorkingCopyClass forPackage: package.
	workingCopy repositoryGroup addRepository: anMCFileBasedRepository.
	mcPlatformSupport autoMigrate: autoMigrate.


]

{ #category : 'category' }
JadeServer >> mcVersionNameAndMessageFrom: aMCWorkingCopy [

	(aMCWorkingCopy needsSaving or: [aMCWorkingCopy ancestors isEmpty]) ifTrue: [
		^'<new>	<new>'.
	].
	^aMCWorkingCopy currentVersionInfo name , Character tab asString , aMCWorkingCopy currentVersionInfo message.


]

{ #category : 'category' }
JadeServer >> mcwcbWorkingCopies [

	| mcWorkingCopyClass list stream |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^nil].
	list := mcWorkingCopyClass allManagers.
	list := list asSortedCollection: [:a :b | a package name <= b package name].
	stream := WriteStream on: String new.
	list do: [:each |
		self saveWorkingCopy: each to: stream.
		stream lf].
	^stream contents

]

{ #category : 'category' }
JadeServer >> mcWorkingCopyClass [

	^self objectInBaseNamed: #'MCWorkingCopy'.


]

{ #category : 'category' }
JadeServer >> mcWorkingCopyNamed: aString [

	| mcWorkingCopyClass workingCopy stream |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^nil].
	workingCopy := mcWorkingCopyClass allManagers 
		detect: [:each | each package name = aString]
		ifNone: [^nil].
	stream := WriteStream on: String new.
	self 
		saveWorkingCopy: workingCopy 
		to: stream.
	^stream contents.


]

{ #category : 'category' }
JadeServer >> methodsContaining: aString [

	^self streamOfMethods: (self classOrganizer substringSearch: aString) first.


]

{ #category : 'category' }
JadeServer >> methodsFor: childClass upTo: parentClass filter: aString isVariables: aBoolean [ 

	| filterList answerList aClass stream selectors |
	filterList := (aString subStrings: Character tab) reject: [:each | each isEmpty].
	aBoolean ifTrue: [filterList := (filterList collect: [:each | each asSymbol]) asIdentitySet].
	aClass := childClass.
	answerList := IdentitySet new.
	selectors := IdentitySet new.
	[
		| methods |
		methods := self 
			_methodsFor: aClass
			filter: filterList
			isVariables: aBoolean.
		methods do: [:each | 
			(selectors includes: each selector) ifFalse: [
				answerList add: each.
				selectors add: each selector.
			].
		].
		aClass = parentClass.
	] whileFalse: [
		aClass := aClass superclass.
	].
	stream := WriteStream on: String new.
	answerList do: [:each | self _addMethod: each toStream: stream].
	^stream contents

]

{ #category : 'category' }
JadeServer >> methodSignatureForSelector: aSymbol [

	^aSymbol.


]

{ #category : 'category' }
JadeServer >> millisecondsElapsedTime: aBlock [

	^Time millisecondsElapsedTime: aBlock.


]

{ #category : 'category' }
JadeServer >> moveClassesInDictionary: sourceDictionary category: aString to: destinationDictionary [

	sourceDictionary copy keysAndValuesDo: [:eachKey :eachValue | 
		(eachValue isBehavior and: [eachValue category = aString]) ifTrue: [
			sourceDictionary removeKey: eachKey.
			destinationDictionary
				at: eachKey
				put: eachValue.
		].
	].


]

{ #category : 'category' }
JadeServer >> moveDictionary: source toBefore: target forUser: aUserProfile [

	| list |
	list := aUserProfile symbolList.
	list remove: source.
	target notNil ifTrue: [
		list
			add: source 
			before: target.
	] ifFalse: [
		list addLast: source.
	].



]

{ #category : 'category' }
JadeServer >> moveMethod: aGsMethod toCategory: aString [
	aGsMethod inClass
		rwMoveMethod: aGsMethod selector
		toCategory: aString.


]

{ #category : 'category' }
JadeServer >> mySessionInfo [

	| dict stream |
	stream := WriteStream on: String new.
	stream nextPutAll: self gemLogPath; cr.
	dict := System gemVersionReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	dict := System gemConfigurationReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	^stream contents

]

{ #category : 'category' }
JadeServer >> nameForSharedPool: anObject forUser: aUserProfile [

	| anArray dict sharedPoolClass |
	anArray := self dictionaryAndSymbolOf: anObject forUser: aUserProfile.
	anArray notNil ifTrue: [^anArray at: 2].
	(dict := aUserProfile objectNamed: anObject name) isNil ifTrue: [^'???'].
	(sharedPoolClass := self objectNamed: 'SharedPool') isNil ifTrue: [^'???'].
	((dict isKindOf: Class) and: [dict isSubclassOf: sharedPoolClass]) ifTrue: [^anObject name , ' _classVars'].
	^'???'.


]

{ #category : 'category' }
JadeServer >> nameOfFirstDictionaryReferencing: aGlobal [

	| list |
	list := self symbolList dictionaryAndSymbolOf: aGlobal.
	list isNil ifTrue: [^''].
	^list first name

]

{ #category : 'category' }
JadeServer >> newUser: aString [

	| userProfile stream |
	userProfile := UserProfile 
		newWithUserId: aString
		password: 'swordfish'
		privileges: #()
		inGroups: #().
	stream := WriteStream on: String new.
	self
		addUser: userProfile 
		toStream: stream.
	^stream contents.


]

{ #category : 'category' }
JadeServer >> nextLine [

	^readStream upTo: Character lf.


]

{ #category : 'category' }
JadeServer >> nextLineAsList [

	^(self nextLine subStrings: Character tab) reject: [:each | each isEmpty].


]

{ #category : 'category' }
JadeServer >> nextPut: aCharacter [

	self nextPutAll: aCharacter asString.


]

{ #category : 'category' }
JadeServer >> nextPutAll: anObject [

	| string args |
	string := self asString: anObject.
	args := Array
		with: self
		with: 1
		with: #'nextPutAll:'
		with: (Array with: string).
	System
		signal: 2336
		args: args
		signalDictionary: GemStoneError.


]

{ #category : 'category' }
JadeServer >> obConfirmationRequest: anOBConfirmationRequest [

	^String new 
		addAll: anOBConfirmationRequest cancelChoice;
		add: Character lf;
		addAll: anOBConfirmationRequest okChoice;
		add: Character lf;
		addAll: anOBConfirmationRequest prompt;
		yourself.


]

{ #category : 'category' }
JadeServer >> obInformRequest: anOBInformRequest [

	^anOBInformRequest message

]

{ #category : 'category' }
JadeServer >> objectForOop: anInteger [

	self subclassResponsibility.

]

{ #category : 'category' }
JadeServer >> objectInBaseNamed: aString [

	^[(SymbolList withAll: self class sharedPools) objectNamed: aString asSymbol] on: Error do: [:ex | ex return: nil].


]

{ #category : 'category' }
JadeServer >> objectNamed: aString [

	^System myUserProfile objectNamed: aString asSymbol.


]

{ #category : 'category' }
JadeServer >> objectSecurityPolicyFor: anObject [

	^anObject segment.


]

{ #category : 'category' }
JadeServer >> obTextRequest: anOBTextRequest [

	| prompt template |
	prompt := anOBTextRequest prompt.
	template := anOBTextRequest template.
	^String new 
		addAll: prompt size printString;
		add: Character lf;
		addAll: prompt;
		addAll: template;
		yourself.


]

{ #category : 'category' }
JadeServer >> oopOf: anObject [

	^anObject asOop.


]

{ #category : 'category' }
JadeServer >> packagePolicy: aPackagePolicy includesSelector: aSymbol forClass: aClass [

	^aPackagePolicy notNil and: [aPackagePolicy includesSelector: aSymbol for: aClass].


]

{ #category : 'category' }
JadeServer >> postSaveClass: aGsClass activities: aString [ 

	| gsClass copyMethods migrateInstances recompileSubclasses removeFromClassHistory symbolList list index key oldClass newClass oldNewList stream |
	gsClass := (self historyOf: aGsClass) last.
	list := aString subStrings: Character tab.
	list := list collect: [:each | each = 'true'].
	symbolList := self symbolList.
	copyMethods := list at: 1.
	recompileSubclasses := list at: 2.
	migrateInstances := list at: 3.
	removeFromClassHistory := list at: 4.
	oldNewList := OrderedCollection new.
	stream := WriteStream on: String new.
	oldClass := (self historyOf: gsClass) asArray reverse at: 2.
	oldNewList add: oldClass -> gsClass.
	recompileSubclasses ifTrue: [
		(self classOrganizer allSubclassesOf: oldClass) do: [:each | 
			gsClass := GsSession currentSession execute: each definition.
			oldNewList add: each -> gsClass.
		].
	].
	copyMethods ifTrue: [
		oldNewList do: [:eachAssoc | 
			oldClass := eachAssoc key.
			newClass := eachAssoc value.
			index := symbolList findFirst: [:eachDict | eachDict includes: newClass].
			index = 0 ifTrue: [self error: 'Where did the class go?'].
			key := (symbolList at: index) keyAtValue: newClass.
			list := newClass copyMethodsFrom: oldClass dictionaries: symbolList.
			list do: [:eachMethod | 
				stream
					nextPutAll: 'method'; tab;
					nextPutAll: index printString; tab;
					nextPutAll: key; tab;
					nextPutAll: eachMethod selector; lf;
					yourself]]].
	migrateInstances ifTrue: [
		System commitTransaction ifFalse: [self error: 'commit failed!'].
		oldNewList do: [:eachAssoc | 
			oldClass := eachAssoc key.
			newClass := eachAssoc value.
			list := oldClass migrateInstancesTo: newClass.
			list do: [:each | 
				each notEmpty ifTrue: [
					stream
						nextPutAll: 'migrate'; tab;
						nextPutAll: newClass name; tab;
						nextPutAll: each size printString; lf;
						yourself.
				].
			].
		].
	].
	removeFromClassHistory ifTrue: [
		oldNewList do: [:eachAssoc | 
			newClass := eachAssoc value.
			((self historyOf: newClass) asArray copyFrom: 1 to: (self historyOf: newClass) size - 1) do: [:each | 
				(self historyOf: newClass) removeVersion: each.
			].
		].
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> print: anObject on: aStream [
	"convert multi-byte strings to single-byte"

	| string |
	string := self printStringOf: anObject.
	string class == String ifFalse: [
		string := String withAll: (string collect: [:each | (32 <= each asciiValue and: [each asciiValue <= 255]) ifTrue: [each] ifFalse: [$?]]).
	].
	aStream nextPutAll: string.


]

{ #category : 'category' }
JadeServer >> printStringOf: anObject [

	^anObject printString.

]

{ #category : 'category' }
JadeServer >> printStringOf: anObject to: anInteger [

	| string |
	(string := self printStringOf: anObject) size > anInteger ifTrue: [string := (string copyFrom: 1 to: anInteger) , '...'].
	string := String withAll: (string collect: [:each | (32 <= each asciiValue and: [each asciiValue <= 255]) ifTrue: [each] ifFalse: [$?]]).
	^string.
]

{ #category : 'category' }
JadeServer >> privilegeListFor: aUserProfile [

	| allPrivileges myPrivileges stream |
	allPrivileges := (aUserProfile class instVarAt: 6) at: #'PrivilegeNames'.
	myPrivileges := aUserProfile privileges.
	stream := WriteStream on: String new.
	allPrivileges do: [:each | 
		stream nextPutAll: each; tab.
		(myPrivileges includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> processes [

	| scheduler stream |
	scheduler := ProcessorScheduler scheduler.
	stream := (WriteStream on: String new)
		nextPutAll: 'highestPriority'; 			space; nextPutAll: scheduler highestPriority 			printString; tab;
		nextPutAll: 'highIOPriority'; 			space; nextPutAll: scheduler highIOPriority 			printString; tab;
		nextPutAll: 'lowestPriority'; 			space; nextPutAll: scheduler lowestPriority 				printString; tab;
		nextPutAll: 'lowIOPriority'; 				space; nextPutAll: scheduler lowIOPriority 				printString; tab;
		nextPutAll: 'systemBackgroundPriority'; 	space; nextPutAll: scheduler systemBackgroundPriority 	printString; tab;
		nextPutAll: 'timingPriority'; 			space; nextPutAll: scheduler timingPriority 			printString; tab;
		nextPutAll: 'userBackgroundPriority'; 		space; nextPutAll: scheduler userBackgroundPriority 		printString; tab;
		nextPutAll: 'userInterruptPriority'; 		space; nextPutAll: scheduler userInterruptPriority 		printString; tab;
		nextPutAll: 'userSchedulingPriority'; 		space; nextPutAll: scheduler userSchedulingPriority 		printString; tab;
		yourself.
	scheduler readyProcesses 		do: [:each | self addProcess: each to: stream withStatus: 'ready'		scheduler: scheduler].
	scheduler suspendedProcesses 	do: [:each | self addProcess: each to: stream withStatus: 'suspended'	scheduler: scheduler].
	self waitingProcesses			do: [:each | self addProcess: each to: stream withStatus: 'waiting'	scheduler: scheduler].
	^stream contents.



]

{ #category : 'category' }
JadeServer >> referencesToObject: anObject [

	^self streamOfMethods: (self classOrganizer referencesToObject: anObject).


]

{ #category : 'category' }
JadeServer >> registerOBNotifications [

	| platform clientForwarder |
	(platform := self objectInBaseNamed: #'OBGemStonePlatform') isNil ifTrue: [^self].
	clientForwarder := ClientForwarder new.
	clientForwarder	clientObject: 1.
	self
		registerOBNotificationsForPlatform: platform 
		clientForwarder: clientForwarder.


]

{ #category : 'category' }
JadeServer >> registerOBNotificationsForPlatform: platform clientForwarder: clientForwarder [

	platform 
		registerBrowseClientForwarder: clientForwarder;
		registerChoiceClientForwarder: clientForwarder;
		registerCloseClientForwarder: clientForwarder;
		registerConfirmationClientForwarder: clientForwarder;
		registerInformClientForwarder: clientForwarder;
		registerMultiLineTextClientForwarder: clientForwarder;
		registerTextClientForwarder: clientForwarder;
		yourself.


]

{ #category : 'category' }
JadeServer >> removeCategory: aString fromBehavior: aBehavior [

	aBehavior rwRemoveCategory: aString.


]

{ #category : 'category' }
JadeServer >> removeClass: aClass from: aDictionary [

	| key |
	key := aDictionary
		keyAtValue: aClass
		ifAbsent: [^false].
	aDictionary removeKey: key.
	^true.


]

{ #category : 'category' }
JadeServer >> removeDictionary: aDictionary fromUser: aUserProfile [

	| symbolList index |
	symbolList := aUserProfile symbolList.
	index := symbolList indexOf: aDictionary.
	aUserProfile removeDictionaryAt: index.


]

{ #category : 'category' }
JadeServer >> removeGroup: aString fromUser: aUserProfile [

	aUserProfile removeGroup: aString.


]

{ #category : 'category' }
JadeServer >> removeKey: aString fromSymbolDictionary: aSymbolDictionary [

	aSymbolDictionary removeKey: aString asSymbol.



]

{ #category : 'category' }
JadeServer >> removeMethod: aGsMethod [

	aGsMethod inClass removeSelector: aGsMethod selector.


]

{ #category : 'category' }
JadeServer >> removePriorVersionsOf: aClass [

	[
		1 < (self historyOf: aClass) size.
	] whileTrue: [
		(self historyOf: aClass) removeVersion: (self historyOf: aClass) first.
	].


]

{ #category : 'category' }
JadeServer >> removePrivilege: aString fromUser: aUserProfile [

	aUserProfile deletePrivilege: aString.


]

{ #category : 'category' }
JadeServer >> renameCategory: oldString to: newString inBehavior: aBehavior [

	aBehavior
		renameCategory: oldString asSymbol
		to: newString.


]

{ #category : 'category' }
JadeServer >> reset [
	"WriteStream protocol"

]

{ #category : 'category' }
JadeServer >> runAsTest: aGsMethod [

	aGsMethod inClass debug: aGsMethod selector.
	^true.


]

{ #category : 'category' }
JadeServer >> runTestNamed: testName in: gsClass [
	"The receiver run the test named <testName> of GemStone/S class named <gsClass>"
	| testResult |

	testResult := (self objectNamed: gsClass) run: testName asSymbol.

	^testResult errorCount printString, ',' ,testResult failureCount printString, ',', testResult passedCount printString

]

{ #category : 'category' }
JadeServer >> runTestsNamed: testCollection in: gsClass [
	"NO SENDERS"
	"The receiver run the all test of the collection <testCollection> of GemStone/S class named <gsClass>"
	| testResult |

	testResult := ((self objectNamed: gsClass) buildSuiteFromMethods: testCollection) run.

"	stream := WriteStream on: String new.
	testResult failures do: [:each | stream nextPutAll: ',', 'F_', each selector].
	testResult errors do: [:each | stream nextPutAll: ',', 'E_', each selector].
	testResult passed do: [:each | stream nextPutAll: ',', 'P_', each selector].

	^stream contents"

	^testResult printString

]

{ #category : 'category' }
JadeServer >> saveWorkingCopy: wc to: stream [

	self _addToPureExportSet: wc.
	stream
		nextPutAll: (self oopOf: wc) printString; tab;
		nextPutAll: wc package name; tab;
		nextPutAll: wc modified printString; tab;
		nextPutAll: wc ancestors size printString; tab;
		yourself.
	wc ancestors do: [:ancestor |
		self _addToPureExportSet: ancestor.
		(self oopOf: ancestor) printOn: stream.
		stream tab.
	].
	stream nextPutAll: wc repositoryGroup repositories size printString; tab.
	wc repositoryGroup repositories do: [:repository |
		self _addToPureExportSet: repository.
		(self oopOf: repository) printOn: stream.
		stream tab.
	].


]

{ #category : 'category' }
JadeServer >> sbAddDictionary: anOrderedCollection [

	| currentName newName symbolList index |
	symbolList := self symbolList.
	newName := anOrderedCollection removeFirst.
	anOrderedCollection notEmpty ifTrue: [
		currentName := anOrderedCollection removeFirst asSymbol.
		index := symbolList findFirst: [:each | each name = currentName].
	] ifFalse: [
		index := symbolList size + 1.
	].
	symbolList
		createDictionaryNamed: newName
		at: index.
	selections at: #'dictionary' put: newName.
	self systemBrowserUpdate.
]

{ #category : 'category' }
JadeServer >> sbAddMethodCategory: anOrderedCollection [

	(self sbClassFrom: anOrderedCollection) addCategory: anOrderedCollection first.
	selections at: #'methodCategory' put: anOrderedCollection first.
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbAddMissingAccessors: anOrderedCollection [

	(self sbClassFrom: anOrderedCollection) compileMissingAccessingMethods.
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbAddNameOf: aClass [

	writeStream nextPutAll: aClass name.
	1 < (self historyOf: aClass) size ifTrue: [
		writeStream nextPutAll: ' ('.
		((self historyOf: aClass) indexOf: aClass) printOn: writeStream.
		writeStream nextPut: $/.
		(self historyOf: aClass) size printOn: writeStream.
		writeStream nextPut: $).
	].
	writeStream tab.


]

{ #category : 'category' }
JadeServer >> sbAddRepository: list [

	| description repository packages |
	description := list removeFirst.
	repository := self mcRepositoryGroup repositories detect: [:each | each description = description].
	packages := self mcWorkingCopyClass allManagers select: [:each | list includes: each package name].
	packages do: [:each | each repositoryGroup addRepository: repository].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbBreak: anOrderedCollection [

	| myClass gsMethod stepPoint |
	myClass := self sbClassFrom: anOrderedCollection.
	gsMethod := self compiledMethodAt: anOrderedCollection removeFirst asSymbol inClass: myClass.
	stepPoint := anOrderedCollection removeFirst asNumber.
	anOrderedCollection removeFirst = 'set' ifTrue: [
		gsMethod setBreakAtStepPoint: stepPoint.
	] ifFalse: [
		gsMethod clearBreakAtStepPoint: stepPoint.
	].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbBrowseClassReferences: anOrderedCollection [

	| class |
	class := (self sbClassFrom: anOrderedCollection) thisClass.
	writeStream 
		nextPutAll: 'browseClassReferences'; lf;
		nextPutAll: (self referencesToObject: class); 
		yourself.


]

{ #category : 'category' }
JadeServer >> sbBrowseGlobalReferences: anOrderedCollection [

	| global |
	global := self objectForOop: anOrderedCollection removeFirst asNumber.
	writeStream 
		nextPutAll: 'browseGlobalReferences'; lf;
		nextPutAll: (self referencesToObject: global); 
		yourself.


]

{ #category : 'category' }
JadeServer >> sbBrowseImplementors: anOrderedCollection [

	writeStream 
		nextPutAll: 'browseImplementors'; lf;
		nextPutAll: (self implementorsOf: anOrderedCollection removeFirst);
		yourself.


]

{ #category : 'category' }
JadeServer >> sbBrowseMethodsContaining: anOrderedCollection [

	writeStream 
		nextPutAll: 'browseMethodsContaining'; lf;
		nextPutAll: (self methodsContaining: anOrderedCollection removeFirst);
		yourself.


]

{ #category : 'category' }
JadeServer >> sbBrowseMethodsWithPragma: anOrderedCollection [

	writeStream 
		nextPutAll: 'browseMethodsWithPragma'; lf;
		yourself.


]

{ #category : 'category' }
JadeServer >> sbBrowseSenders: anOrderedCollection [

	writeStream 
		nextPutAll: 'browseSenders'; lf;
		nextPutAll: (self sendersOf: anOrderedCollection removeFirst);
		yourself.


]

{ #category : 'category' }
JadeServer >> sbChangeClassName: aList [

	| oldName class newName changedIn |
	oldName := aList removeFirst asSymbol.
	class := self objectForOop: aList removeFirst asNumber.
	class name == oldName ifFalse: [self error: 'Current name is ' , class name printString].
	newName := aList removeFirst asSymbol.
	class changeNameTo: newName.
	changedIn := OrderedCollection new.
	self symbolList do: [:each | 
		(each includes: class) ifTrue: [
			(each at: oldName ifAbsent: [nil]) == class ifFalse: [self error: 'Class not at name!'].
			(each includesKey: newName) ifTrue: [self error: 'Key already in use!'].
			each
				removeKey: oldName;
				at: newName put: class;
				yourself.
			changedIn add: each.
		].
	].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbChangesInPackage: anOrderedCollection [
	"where anOrderedCollection is {packageName, projectName}"

	self error: 'this message is no longer supported'
]

{ #category : 'category' }
JadeServer >> sbCheckUniqueClassName: aList [

	| oldName class newName |
	oldName := aList removeFirst asSymbol.
	class := self objectForOop: aList removeFirst asNumber.
	class name == oldName ifFalse: [
		writeStream nextPutAll: 'Current name is ' , class name printString. 
		^self.
	].
	newName := aList removeFirst asSymbol.
	self symbolList do: [:each | 
		((each includes: class) and: [each includesKey: newName]) ifTrue: [
			writeStream nextPutAll: 'Dictionary '.
			each name printOn: writeStream.
			writeStream nextPutAll: ' already has a global with name '.
			newName printOn: writeStream.
			^self.
		].
	].
	

]

{ #category : 'category' }
JadeServer >> sbClass: aList [

	| string newClass mcWorkingCopyClass packages dictName |
	string := aList first.
	newClass := string evaluate.
	self classOrganizer update.
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [
		packages := Array with: nil.
	] ifFalse: [
		packages := mcWorkingCopyClass allManagers collect: [:each | each package name].
		packages := packages select: [:each | (newClass category copyFrom: 1 to: (newClass category size min: each size)) = each].
		packages isEmpty ifTrue: [
			packages := Array with: nil.
		].
	].
	dictName := (newClass class canUnderstand: #'symbolDictionaryName')
		ifTrue: [newClass symbolDictionaryName]
		ifFalse: [
			| array |
			array := self dictionaryAndSymbolOf: newClass.
			array isNil
				ifTrue: ['UserGlobals']
				ifFalse: [array first name]].
	selections 
		at: #'package' 		put: packages first;
		at: #'dictionary' 		put: dictName asString;
		at: #'category' 		put: newClass category;
		at: #'className'	put: newClass name;
		at: #'class'				put: newClass;
		yourself.
	selectedClass := newClass.
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbClassCategory: aList [

	| category classes |
	category := aList removeFirst.
	category := category copyFrom: 1 to: category size - 1.
	classes := aList removeFirst subStrings reject: [:each | each isEmpty].
	classes := classes collect: [:each | self objectNamed: each asSymbol].
	classes := classes collect: [:each | each thisClass].
	classes do: [:each | each category: category].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbClassComment: anOrderedCollection [

	| class doc txt |
	class := (self sbClassFrom: anOrderedCollection) thisClass.
	doc := (self objectInBaseNamed: #'GsClassDocumentation') newForClass: self.
	txt := (self objectInBaseNamed: #'GsDocText') new details: self sbNextParagraph trimSeparators.
	doc documentClassWith: txt.
	class rwComment: doc.
	self systemBrowserUpdate.
]

{ #category : 'category' }
JadeServer >> sbClassesToDictionary: anOrderedCollection [

	| action targetName target sourceNames sources classNames |
	action := anOrderedCollection removeFirst.
	targetName := anOrderedCollection removeFirst asSymbol.
	target := self symbolList detect: [:each | each name = targetName].
	sourceNames := self nextLineAsList collect: [:each | each asSymbol].
	sources := sourceNames collect: [:eachName | self symbolList detect: [:eachDictionary | eachDictionary name = eachName]].
	classNames := self nextLineAsList collect: [:each | each asSymbol].
	classNames do: [:eachName | 
		| source class |
		source := sources detect: [:eachDict | 
			class := eachDict detect: [:eachGlobal | eachGlobal isBehavior and: [eachGlobal name = eachName]] ifNone: [nil].
			class notNil.
		].
		target at: class name put: class.
		action = 'move' ifTrue: [source removeKey: class name].
	].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbClassFrom: anOrderedCollection [

	| selectedClassName selectedClassOop set myClass |
	selectedClassName := (anOrderedCollection removeFirst subStrings: Character space) first.
	selectedClassOop := anOrderedCollection removeFirst asNumber.
	set := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal |
			eachGlobal isBehavior ifTrue: [
				set addAll: (self historyOf: eachGlobal).
			].
		].
	].
	myClass := set detect: [:each | (self oopOf: each) = selectedClassOop].
	myClass name asString = selectedClassName ifFalse: [self error: 'Class not found!'].
	anOrderedCollection removeFirst = 'classTab' ifTrue: [myClass := myClass class].
	^myClass.


]

{ #category : 'category' }
JadeServer >> sbComparePackages: anOrderedCollection [

	| current ancestor repository patch string |
	current := self mcWorkingCopyClass forPackage: (self mcPackageClass named: anOrderedCollection removeFirst).
	ancestor := anOrderedCollection removeFirst.
	repository := anOrderedCollection removeFirst.
	repository := current repositoryGroup repositories detect: [:each | each description = repository].
	ancestor := repository class name = #'MCDictionaryRepository'
		ifTrue: [repository versionFromVersionNamed: ancestor]
		ifFalse: [repository versionFromFileNamed: ancestor , '.mcz'].
	patch := current package snapshot patchRelativeToBase: ancestor snapshot.
	string := self 
		_mcDescriptionOfPatch: patch
		baseName: ancestor info name
		alternateName: nil.
	writeStream 
		nextPutAll: 'comparePackages'; lf;
		nextPutAll: string;
		yourself.


]

{ #category : 'category' }
JadeServer >> sbCopyMethodsFor: newClass [

	| history oldClass symbolList |
	newClass isMeta ifFalse: [self sbCopyMethodsFor: newClass class].
	history := self historyOf: newClass thisClass.
	oldClass := history at: history size - 1.
	newClass isMeta ifTrue: [oldClass := oldClass class].
	symbolList := self symbolList.
	oldClass selectors do: [:each | 
		| source category errors |
		source := (self compiledMethodAt: each inClass: oldClass) sourceString.
		category := self _behavior: oldClass categoryOfSelector: each.
		errors := newClass 
			compileMethod: source
			dictionaries: symbolList
			category: category.
		errors notNil ifTrue: [
			writeStream
				nextPutAll: 'compileError'; lf;
				nextPutAll: newClass name; tab;
				nextPutAll: category; lf;
				nextPutAll: source; lf;
				nextPut: $%; lf;
				yourself.
			newClass removeSelector: each ifAbsent: [].
		].
	].


]

{ #category : 'category' }
JadeServer >> sbFileOutClass: anOrderedCollection [

	writeStream nextPutAll: (self sbClassFrom: anOrderedCollection) thisClass fileOutClass.


]

{ #category : 'category' }
JadeServer >> sbFileOutDictionary: anOrderedCollection [

	| dictionary |
	dictionary := self objectNamed: anOrderedCollection first.
	writeStream nextPutAll: '! ------- Create dictionary if it is not present
run
| aSymbol names userProfile |
aSymbol := ' , dictionary name printString , '.
userProfile := System myUserProfile.
names := userProfile symbolList names.
(names includes: aSymbol) ifFalse: [
	| symbolDictionary |
	symbolDictionary := SymbolDictionary new name: aSymbol; yourself.
	userProfile insertDictionary: symbolDictionary at: names size + 1.
].
' , '%
'.
	self classOrganizer
		fileOutClassesAndMethodsInDictionary: dictionary
		on: writeStream.


]

{ #category : 'category' }
JadeServer >> sbFileOutMethod: anOrderedCollection [

	| aClass |
	aClass := (self sbClassFrom: anOrderedCollection) thisClass.
	writeStream nextPutAll: aClass fileOutMethod: anOrderedCollection removeFirst.


]

{ #category : 'category' }
JadeServer >> sbFindClass [

	| classToPackageMap |
	classToPackageMap := self sbFindClassPackageMap.
	self symbolList do: [:eachDict | 
		| name |
		name := eachDict name.
		eachDict do: [:eachGlobal | 
			eachGlobal isBehavior ifTrue: [
				| category |
				category := eachGlobal category.
				category isNil ifTrue: [category := ''].
"1"			self sbAddNameOf: eachGlobal.
				writeStream
"2"				nextPutAll: name; tab;
"3"				nextPutAll: category; tab;		"Class category"
"4"				nextPutAll: (classToPackageMap at: eachGlobal ifAbsent: ['']); tab;		"Package name if available"
					lf.
			].
		].
	].


]

{ #category : 'category' }
JadeServer >> sbFindClassPackageMap [

	| systemOrganizerClass mcWorkingCopyClass dictionary packageInfoList |
	dictionary := Dictionary new.
	(systemOrganizerClass := self objectInBaseNamed: #'SystemOrganizer') isNil ifTrue: [^dictionary].
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^dictionary].
	packageInfoList := mcWorkingCopyClass allManagers collect: [:each | each packageInfo].
	systemOrganizerClass new categoryDict keysAndValuesDo: [:catName :classes |
		| symbol packageInfo |
		symbol := catName asSymbol.
		packageInfo := packageInfoList detect: [:each | each includesSystemCategory: symbol] ifNone: [nil].
		packageInfo notNil ifTrue: [
			| name |
			name := packageInfo name.
			classes do: [:each | dictionary at: each put: name].
		].
	].
	^dictionary.


]

{ #category : 'category' }
JadeServer >> sbFindSelectors: anOrderedCollection [

	| allSymbols pattern |
	pattern := (anOrderedCollection collect: [:each | each = '*' ifTrue: [$*] ifFalse: [each]]) asArray.
	allSymbols := ((AllUsers userWithId: #SymbolUser ifAbsent: [AllUsers userWithId: #DataCurator]) resolveSymbol: #AllSymbols) value.
	allSymbols := allSymbols select: [:each |each asUppercase matchPattern: pattern].
	allSymbols := allSymbols select: [:each | (self classOrganizer implementorsOf: each) notEmpty].
	allSymbols := allSymbols asSortedCollection.
	allSymbols do: [:each | writeStream nextPutAll: each; nextPut: Character lf; yourself].


]

{ #category : 'category' }
JadeServer >> sbInstVarsOldParent: oldParent newParent: newParent oldChild: oldChild [

	| added removed newList used missing |
	added := newParent allInstVarNames asIdentitySet - oldParent allInstVarNames asIdentitySet.
	removed := oldParent allInstVarNames asIdentitySet - newParent allInstVarNames asIdentitySet.
	newList := oldChild instVarNames.
	used := IdentitySet new.
	(oldChild class canUnderstand: #'_methodDict') ifTrue: [
		oldChild _methodDict do: [:each | used addAll: each instVarsAccessed].
	].
	(oldChild class canUnderstand: #'persistentMethodDictsDo:') ifTrue: [
		oldChild persistentMethodDictsDo: [:eachDict | 
			eachDict do: [:eachMethod | 
				used addAll: eachMethod instVarsAccessed.
			].
		].
	].
	used := used * removed.	"Only interested in things that have been removed."
	missing := (used - newList asIdentitySet) asSortedCollection asArray.
	newList := newList , missing.
	newList := newList reject: [:each | added includes: each].
	^newList.


]

{ #category : 'category' }
JadeServer >> sbListMethodPragmas [
	"none before 3x"


]

{ #category : 'category' }
JadeServer >> sbLoadLatestVersionOfConfiguration: anOrderedCollection [

	anOrderedCollection do: [:each | 
		(self objectNamed: each) project latestVersion load.
	].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbMethodCategory: anOrderedCollection [
	| behavior category |
	behavior := self sbClassFrom: anOrderedCollection.
	category := anOrderedCollection removeFirst.
	anOrderedCollection do: [:each | behavior rwMoveMethod: each asSymbol toCategory: category].
	self systemBrowserUpdate

]

{ #category : 'category' }
JadeServer >> sbMethodClass: anOrderedCollection [
	"Drag/drop method onto class"

	| sourceBehavior action targetName set target |
	sourceBehavior := self sbClassFrom: anOrderedCollection.
	action := anOrderedCollection removeFirst.
	targetName := anOrderedCollection removeFirst asSymbol.
	set := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal | 
			(eachGlobal isBehavior and: [eachGlobal name = targetName]) ifTrue: [set add: eachGlobal].
		].
	].
	1 < set size ifTrue: [self error: 'Target name is ambiguous!'].
	1 = set size ifFalse: [self error: 'Target not found!'].
	target := set asArray first.
	sourceBehavior isMeta ifTrue: [target := target class].
	anOrderedCollection do: [:each | 
		| gsMethod result |
		gsMethod := self compiledMethodAt: each asSymbol inClass: sourceBehavior.
		result := self		"key: GsNMethod value: (Array withAll: errors and warnings)"
				compileMethod: gsMethod sourceString
				behavior: target
				symbolList: self symbolList 
				inCategory: (self _behavior: sourceBehavior categoryOfSelector: gsMethod selector).
		(result key notNil and: [action = 'move']) ifTrue: [
			sourceBehavior removeSelector: gsMethod selector.
		].
	].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbMigrateAll: aClass [

	| mcPlatformSupport classes instances |
	((mcPlatformSupport := self objectInBaseNamed: #'MCPlatformSupport') notNil and: [mcPlatformSupport autoMigrate]) ifTrue: [^self].
	System commitTransaction ifFalse: [self error: 'commit failed!'].
	classes := (ClassOrganizer new allSubclassesOf: aClass) 
		inject: (IdentitySet withAll: (self historyOf: aClass))
		into: [:set :each | set addAll: (self historyOf: each); yourself].
	classes := classes asArray.
	instances := (self objectInBaseNamed: #'SystemRepository') listInstances: classes.
	1 to: classes size do: [:i | 
		| class |
		class := classes at: i.
		class 
			migrateInstances: (instances at: i) 
			to: (self historyOf: class) last.
		System commitTransaction ifFalse: [self error: 'commit failed!'].
	].


]

{ #category : 'category' }
JadeServer >> sbNextParagraph [

	| stream |
	stream := WriteStream on: String new.
	[
		readStream peek = $%.
	] whileFalse: [
		stream nextPutAll: self nextLine; lf.
	].
	self nextLine.
	^stream contents.


]

{ #category : 'category' }
JadeServer >> sbObjectLog: anOrderedCollection [

	| command priorities class log debuggerLogEntryClass | 
	(class := self objectInBaseNamed: #'ObjectLogEntry') isNil ifTrue: [^self].
	debuggerLogEntryClass := self objectInBaseNamed: #'DebuggerLogEntry'.
	(command := anOrderedCollection removeFirst) = 'delete' ifTrue: [
		anOrderedCollection do: [:each | 			| oop entry |
			oop := each asNumber.			entry := class objectLog detect: [:each2 | (self oopOf: each2) = oop] ifNone: [nil].			entry notNil ifTrue: [class objectLog remove: entry].
		].
		^self systemBrowserCommand.
	].
	writeStream nextPutAll: 'objectLog'; lf.
	priorities := anOrderedCollection removeFirst asArray collect: [:each | each asString asNumber].
	log := class objectLog select: [:each | priorities includes: each priority].
	log reverseDo: [:each | 
		| labelString objectString |
		objectString := String withAll: (each objectString asArray collect: [:char | 
			char asciiValue < 32 ifTrue: [Character space] ifFalse: [
			127 < char asciiValue ifTrue: [$?] ifFalse: [char]]]).
		500 < objectString size ifTrue: [objectString := (objectString copyFrom: 1 to: 500) , '...'].
		each label = each object printString ifTrue: [
			labelString := ''.
		] ifFalse: [
			labelString := String withAll: (each labelString asArray collect: [:char | 
				char asciiValue < 32 ifTrue: [Character space] ifFalse: [
				127 < char asciiValue ifTrue: [$?] ifFalse: [char]]]).
			500 < labelString size ifTrue: [labelString := (labelString copyFrom: 1 to: 500) , '...'].
		].
"1"	(self oopOf: each) printOn: writeStream.
"2"	writeStream tab; nextPutAll: each class name; tab.
"3"	each pid printOn: writeStream. 
		writeStream tab.
"4"	each stamp rounded printOn: writeStream.
"5"	writeStream tab; nextPutAll: labelString; tab.
"6"	each priority printOn: writeStream.
		writeStream tab.
"7"	each tag printOn: writeStream.
"8"	writeStream tab; nextPutAll: objectString; tab.
		(debuggerLogEntryClass notNil and: [each isKindOf: debuggerLogEntryClass]) ifTrue: [
"9"		(self oopOf: each continuation) printOn: writeStream.
		] ifFalse: [
			writeStream nextPutAll: '0'.
		].
		writeStream lf.
	].


]

{ #category : 'category' }
JadeServer >> sbPostSaveClass: anOrderedCollection [
	"this has been Rowanized"

	self systemBrowserUpdate 

]

{ #category : 'category' }
JadeServer >> sbReadMethodFilter [

	| pieces |
	pieces := self nextLine subStrings: Character tab.
	methodFilterType := pieces at: 1.
	methodCommandResult type: methodFilterType.
	methodCommandResult writeTypeTo: writeStream


]

{ #category : 'category' }
JadeServer >> sbRecompileSubclassesOf: newClass andCopyMethods: aBoolean [

	| history oldClass symbolList list |
	history := self historyOf: newClass thisClass.
	oldClass := history at: history size - 1.
	symbolList := self symbolList.
	list := self classOrganizer subclassesOf: oldClass.
	list do: [:oldSubclass |
		| instVars classInstVars definition string newSubclass i j |
		instVars := self sbInstVarsOldParent: oldClass newParent: newClass oldChild: oldSubclass.
		classInstVars := self sbInstVarsOldParent: oldClass class newParent: newClass class oldChild: oldSubclass class.

		definition := oldSubclass definition.
		0 < (i := definition findString: 'instVarNames:' startingAt: 1) ifTrue: [
			j := definition indexOf: Character lf startingAt: i.
			string := String withAll: 'instVarNames: #('.
			instVars do: [:each | string addAll: each; add: Character space].
			string add: $).
			definition := (definition copyFrom: 1 to: i - 1) , string , (definition copyFrom: j to: definition size).
		].
		0 < (i := definition findString: 'classInstVars:' startingAt: 1) ifTrue: [
			j := definition indexOf: Character lf startingAt: i.
			string := String withAll: 'classInstVars: #('.
			classInstVars do: [:each | string addAll: each; add: Character space].
			string add: $).
			definition := (definition copyFrom: 1 to: i - 1) , string , (definition copyFrom: j to: definition size).
		].
		newSubclass := definition evaluate.
		aBoolean ifTrue: [self sbCopyMethodsFor: newSubclass].
		self classOrganizer update.
	].


]

{ #category : 'category' }
JadeServer >> sbRemoveClasses [
	| containers classNames |
	self nextLine = 'packageList'. "ignore this" 
	containers := self nextLineAsList.
	classNames := (self nextLineAsList reject: [:each | each isEmpty])
				collect: [:each | (each subStrings: Character space) first asSymbol].
	containers do: 
			[:packageName |
			classNames
				do: [:className | (Rowan packageServiceClass forPackageNamed: packageName) removeClassNamed: className]].
	^self systemBrowserUpdate
]

{ #category : 'category' }
JadeServer >> sbRemoveDictionaries: anOrderedCollection [

	anOrderedCollection do: [:each | 
		self symbolList removeDictionaryNamed: each asSymbol.
	].
	self systemBrowserUpdate.
]

{ #category : 'category' }
JadeServer >> sbRemoveGlobals [

	| symbolList dictionaries globals |
	symbolList := self symbolList.
	dictionaries := self nextLineAsList collect: [:each | each asSymbol].
	dictionaries := dictionaries collect: [:eachName | symbolList detect: [:eachDict | eachDict name = eachName]].
	globals := self nextLineAsList collect: [:each | each asSymbol].
	dictionaries do: [:eachDict | 
		globals do: [:eachKey | 
			eachDict removeKey: eachKey ifAbsent: [].
		].
	].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbRemoveHistory: aClass [

	(ClassOrganizer new allSubclassesOf: aClass) asArray , (Array with: aClass) do: [:eachNewClass | 
		(self historyOf: eachNewClass) asArray do: [:eachClass | 
			eachClass ~~ eachNewClass ifTrue: [
				(self historyOf: eachNewClass) removeVersion: eachClass.
			].
		].
	].


]

{ #category : 'category' }
JadeServer >> sbRemoveKey: aSymbol fromDictionary: aDictionary [

	aDictionary removeKey: aSymbol.


]

{ #category : 'category' }
JadeServer >> sbRemoveMethodCategories: anOrderedCollection [

	| behavior |
	behavior := self sbClassFrom: anOrderedCollection.
	anOrderedCollection do: [:each | behavior rwRemoveCategory: each].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbRemoveMethods: anOrderedCollection [
	| behavior classEntity  notRemoved|
	behavior := self sbClassFrom: anOrderedCollection.
	classEntity := Rowan classServiceClass forClassNamed: behavior name meta: behavior isMeta.
	notRemoved := Array new. 
	anOrderedCollection do: [:each | classEntity removeSelector: each asSymbol ifAbsent: [notRemoved add: each]].
	self systemBrowserUpdate.
	notRemoved isEmpty ifFalse:[self error: 'The following selectors were not removed. Possibly in a superclass? ', notRemoved printString].
]

{ #category : 'category' }
JadeServer >> sbRemovePriorVersions [

	| isPackages containers classNames |
	isPackages := self nextLine = 'packageList'.
	containers := self nextLineAsList.
	classNames := (self nextLineAsList reject: [:each | each isEmpty]) collect: [:each | (each subStrings: Character space) first asSymbol].
	self symbolList do: [:eachDictionary | 
		| dictionaryName |
		dictionaryName := eachDictionary name asString.
		classNames do: [:eachName |
			| class flag |
			(class := eachDictionary at: eachName ifAbsent: [nil]) notNil ifTrue: [
				isPackages ifTrue: [
					flag := false.
					containers do: [:each | flag := flag or: [(class category copyFrom: 1 to: (class category size min: each size)) = each]].
				] ifFalse: [
					flag := containers includes: dictionaryName.
				].
				flag ifTrue: [
					| classHistory |
					classHistory := self historyOf: class.
					classHistory size - 1 timesRepeat: [
						classHistory removeVersion: classHistory first.
					].
				].
			].
		].
	].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbRevertClass [

	| isPackages container className |
	isPackages := self nextLine = 'packageList'.
	container := self nextLine trimSeparators.
	className := self nextLine trimSeparators.
	self symbolList do: [:eachDictionary | 
		| dictionaryName class flag |
		dictionaryName := eachDictionary name asString.
		(class := eachDictionary at: className ifAbsent: [nil]) notNil ifTrue: [
			isPackages ifTrue: [
				flag := (class category copyFrom: 1 to: (class category size min: container size)) = container.
			] ifFalse: [
				flag := container = dictionaryName.
			].
			flag ifTrue: [
				| history |
				history := class classHistory.
				(class == history last and: [1 < history size]) ifFalse: [self error: 'Unexpected class history!'].
				history removeVersion: class.
				class := history last.
				eachDictionary at: class name put: class.
			].
		].
	].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbRunClassTests: aString [

	| behavior |
	behavior := self sbClassFrom: (aString subStrings: Character tab).
	^self defectiveTestsIn: behavior.


]

{ #category : 'category' }
JadeServer >> sbRunMethodTests: aString [

	| list class |
	list := aString subStrings: Character tab.
	class := (self sbClassFrom: list) thisClass.
	list do: [:each | class debug: each asSymbol].
	^true.


]

{ #category : 'category' }
JadeServer >> sbSaveMethod: anOrderedCollection [
	"Save in method editor"

	| behavior category string association gsMethod |
	behavior := self sbClassFrom: anOrderedCollection.
	category := anOrderedCollection notEmpty ifTrue: [anOrderedCollection removeFirst] ifFalse: ['other'].
	string := self sbNextParagraph.
	association := self		"key: GsNMethod value: (Array withAll: errors and warnings)"
		compileMethod: string 
		behavior: behavior 
		symbolList: self symbolList 
		inCategory: category asSymbol.
	(gsMethod := association key) isNil ifTrue: [
		System
			signal: 1001 
			args: (Array with: association value)
			signalDictionary: GemStoneError.
	].
	selections 
		at: #'methodCategory' 	put: (self _behavior: gsMethod inClass categoryOfSelector: gsMethod selector) asString;
		at: #'method'					put: gsMethod selector asString;
		at: #'methodWarnings'	put: association value;
		yourself.
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbSavePackage: list [

	| packageName package repositoryDescription repository versionName httpUser httpPassword comment |
	packageName := list removeFirst.
	package := self mcWorkingCopyClass allManagers detect: [:each | each package name = packageName].
	repositoryDescription := list removeFirst.
	repository := self mcRepositoryGroup repositories detect: [:each | each description = repositoryDescription].
	versionName := list removeFirst.
	list notEmpty ifTrue: [httpUser := list removeFirst].
	list notEmpty ifTrue: [httpPassword := list removeFirst].
	comment := self sbNextParagraph.
	[
		comment notEmpty and: [comment last asciiValue <= 32].
	] whileTrue: [
		comment := comment copyFrom: 1 to: comment size - 1.
	].
	(repository class name = #'MCHttpRepository') ifTrue: [
		repository
			user: httpUser;
			password: httpPassword;
			yourself.
	].
	self 
		mcStore: package 
		name: versionName 
		message: comment 
		repository: repository.
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbSetHomeDictionary: list [

	| name dictionary packagePolicy |
	name := list removeFirst asSymbol.
	dictionary := self symbolList detect: [:each | each name = name].
	(packagePolicy := self gsPackagePolicy) notNil ifTrue: [
		packagePolicy homeSymbolDict: dictionary.
	].
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer >> sbUniqueVersionName: aList [

	| packageName package |
	packageName := aList removeFirst.
	package := self mcWorkingCopyClass allManagers detect: [:each | each package name = packageName].
	writeStream
		nextPutAll: 'uniqueVersionName'; lf;
		nextPutAll: package uniqueVersionName;
		yourself.



]

{ #category : 'category' }
JadeServer >> sbUnloadPackage: anOrderedCollection [
	| service |
	service := Rowan packageServiceClass forPackageNamed: anOrderedCollection removeFirst.
	service deletePackage.
	self systemBrowserUpdate

]

{ #category : 'category' }
JadeServer >> sbUpdateClasses [

	| tabName |
	tabName := self nextLine.
	writeStream nextPutAll: tabName; lf.
	tabName = 'classList' ifTrue: [^self sbUpdateClassList].
	tabName = 'classHierarchy' ifTrue: [^self sbUpdateClassHierarchy].
	self error: 'Unexpected token!'.


]

{ #category : 'category' }
JadeServer >> sbUpdateMethod [

	| classes method names selection aSymbol |

	aSymbol := methodCommandResult selectedSelectors first. 

	"Inherited implimentors"
	classes := self sbUpdateMethodInheritedImplementationsOf: aSymbol.
	names := classes collect: [:each | each name asString].
	methodCommandResult inheritedClasses: names. 
	methodCommandResult writeInheritedClassesTo: writeStream.  "Line 1"

	"Which inherited implementation is selected?"
	selection := self nextLine.
	(names includes: selection) ifFalse: [selection := names last].
	methodCommandResult inheritedClass: selection.
	methodCommandResult writeInheritedClassTo: writeStream.	"Line 2"

	method := self compiledMethodAt: aSymbol inClass: (classes detect: [:each | each name asString = selection]).
	self _describeMethod: method.


]

{ #category : 'category' }
JadeServer >> sbUpdateMethodBreakPointsFor: aMethod [
	"Answers an Array of step points"

	^aMethod _stepPointsFromBreakIpOffsets: aMethod _breakpointIpOffsets.		"at least as far back as 32-bit 6.3.0 and 64-bit 2.3.0, but not in 64-bit 3.0"


]

{ #category : 'category' }
JadeServer >> sbUpdateMethodCategories [

	methodCommandResult classNamesFrom: classList. 
	methodCommandResult updateMethodCategories; 
		writeMethodCategoriesTo: writeStream. 
	methodCommandResult updateMethodFilterSelections: self nextLineAsList 
			hasCategoryOverride: (selections at: #'methodCategory' ifAbsent: [nil]).
	methodFilters := methodCommandResult selectionNames.
	methodCommandResult writeSelectionsTo: writeStream.

]

{ #category : 'category' }
JadeServer >> sbUpdateMethodFilter [
	
	selectedClass isNil ifTrue: [^self].
	methodFilterType = 'categoryList' ifTrue: [^self sbUpdateMethodCategories].
	methodFilterType = 'variableList' ifTrue: [^self sbUpdateMethodVariables].
	methodFilterType = 'pragmaList' ifTrue: [^self sbUpdateMethodPragmas].
	self error: 'Unexpected token!'.


]

{ #category : 'category' }
JadeServer >> sbUpdateMethodFilterSelections [

	| mySelections override |
	mySelections := self nextLineAsList.
	(override := selections at: #'methodCategory' ifAbsent: [nil]) notNil ifTrue: [mySelections := Array with: override].
	mySelections := methodFilters select: [:each | mySelections includes: each asString].
	mySelections notEmpty ifTrue: [methodFilters := mySelections].
	self writeList: mySelections.


]

{ #category : 'category' }
JadeServer >> sbUpdateMethodInheritedImplementationsOf: aSymbol [

	| classes currentClass |
	classes := OrderedCollection new.
	currentClass := classList last.
	[
		currentClass notNil.
	] whileTrue: [
		(self class: currentClass includesSelector: aSymbol) ifTrue: [classes add: currentClass].
		currentClass := currentClass superclass.
	].
	^classes reverse.


]

{ #category : 'category' }
JadeServer >> sbUpdateMethodPragmas [

	self sbUpdateMethodFilterSelections.


]

{ #category : 'category' }
JadeServer >> sbUpdateMethods [

	| selectors |
	selectedClass isNil ifTrue: [^self].
	methodFilterType = 'categoryList' ifTrue: [selectors := self sbUpdateMethodsByCategories] ifFalse: [
	methodFilterType = 'variableList' ifTrue: [selectors := self sbUpdateMethodsByVariables] ifFalse: [
	methodFilterType = 'pragmaList' ifTrue: [selectors := self sbUpdateMethodsByPragmas] ifFalse: [
		self error: 'Unrecognized methodFilterType: ' , methodFilterType printString]]].
	methodCommandResult selectors: selectors asSortedCollection asArray.
	methodCommandResult updateMethodsInfo.
	methodCommandResult writeMethodsTo: writeStream. 
	self sbUpdateMethodSelections


]

{ #category : 'category' }
JadeServer >> sbUpdateMethodsByCategories [

	| selectors |
	selectors := IdentitySet new.
	classList do: [:eachClass |
		eachClass selectors do: [:eachSelector |
			( methodFilters includes: (self _behavior: eachClass categoryOfSelector: eachSelector)) ifTrue: [selectors add: eachSelector].
		].
	].
	^selectors.


]

{ #category : 'category' }
JadeServer >> sbUpdateMethodsByPragmas [
	"none before 3.x"

	^#()

]

{ #category : 'category' }
JadeServer >> sbUpdateMethodsByVariables [

	| selectors filters |
	selectors := IdentitySet new.
	filters := IdentitySet withAll: (methodFilters select: [:each | each isSymbol]).
	selectedClass selectors do: [:eachSelector | 
		| gsMethod |
		gsMethod := self compiledMethodAt: eachSelector inClass: selectedClass.
		(gsMethod instVarsAccessed * filters) notEmpty ifTrue: [selectors add: eachSelector].
	].
	^selectors.


]

{ #category : 'category' }
JadeServer >> sbUpdateMethodSelections [

	| priorSelections override newSelections aList |
	aList := methodCommandResult selectors asArray. 
	priorSelections := self nextLineAsList.
	(override := selections at: #'method' ifAbsent: [nil]) notNil ifTrue: [priorSelections := Array with: override].
	newSelections := aList select: [:each | priorSelections includes: each asString].
	methodCommandResult selectedSelectors: newSelections. 
	methodCommandResult writeSelectedSelectorsTo: writeStream.
	newSelections size = 1 ifTrue: [self sbUpdateMethod].



]

{ #category : 'category' }
JadeServer >> sbUpdateMethodVariables [

	methodCommandResult classNamesFrom: classList. 
	methodCommandResult updateMethodVariables. 
	methodCommandResult writeMethodFiltersTo: writeStream. 
	methodCommandResult updateMethodFilterSelections: self nextLineAsList 
		hasCategoryOverride: (selections at: #'methodCategory' ifAbsent: [nil]).
	methodFilters := methodCommandResult selectionNames.
	methodCommandResult writeSelectionsTo: writeStream. 



]

{ #category : 'category' }
JadeServer >> sbUpdatePackage: aString [
	| package workingCopy list index |
	self mcPackageClass isNil
		ifTrue: 
			[writeStream
				nextPut: $%;
				lf.
			writeStream
				nextPut: $%;
				lf.
			^self ].
	package := self mcPackageClass named: aString.
	workingCopy := self mcWorkingCopyClass forPackage: package.
	list := workingCopy ancestors collect: [:each | 0 -> each].
	index := 1.
	[list size < 4 and: [index <= list size]] whileTrue: 
			[| assoc |
			assoc := list at: index.
			assoc value ancestors do: [:parent | list add: assoc key + 1 -> parent].
			index := index + 1].
	list do: 
			[:each |
			| date time |
			date := each value date isNil
						ifTrue: ['']
						ifFalse: [each value date asStringUsingFormat: #(3 2 1 $- 1 1)].
			time := each value time isNil
						ifTrue: ['']
						ifFalse: [each value time asStringUsingFormat: #($: true false)].
			writeStream
				nextPutAll: each key printString;
				tab;
				nextPutAll: each value name;
				tab;
				nextPutAll: date;
				nextPut: $T;
				nextPutAll: time;
				tab;
				nextPutAll: (each value message
							collect: [:char | (char asciiValue < 32 or: [127 < char asciiValue]) ifTrue: [$.] ifFalse: [char]]);
				lf].
	writeStream
		nextPut: $%;
		lf.
	workingCopy repositoryGroup repositories do: 
			[:each |
			writeStream
				nextPutAll: each class name;
				tab;
				nextPutAll: each description;
				tab;
				yourself.
			each class name = #MCHttpRepository
				ifTrue: 
					[writeStream
						nextPutAll: each user;
						tab;
						nextPutAll: each password;
						yourself]
				ifFalse: 
					[writeStream
						tab;
						tab].
			writeStream lf].
	writeStream
		nextPut: $%;
		lf

]

{ #category : 'category' }
JadeServer >> sbUpdatePackagesOrDictionaries [

	| selectedTab |
	selectedTab := self nextLine.
	"Removed for Rowan which may not have Monticello loaded, but have a package tab"
	"(self mcWorkingCopyClass isNil or: [self gsPackagePolicy isNil]) ifTrue: [selectedTab := 'dictionaryList']." 
	writeStream nextPutAll: selectedTab; lf.
	classList := OrderedCollection new.
	selectedTab = 'dictionaryList' ifTrue: [^self sbUpdateDictionaries].
	selectedTab = 'packageList' ifTrue: [^self sbUpdatePackages].
	selectedTab = 'projectList' ifTrue:[
		self sbUpdatePackages. 
		^self updateProjects].
	self error: 'unexpected token'.


]

{ #category : 'category' }
JadeServer >> selectedClassOverridesSelector: aSymbol [

	^selectedClass superclass notNil and: [selectedClass superclass canUnderstand: aSymbol].


]

{ #category : 'category' }
JadeServer >> selectorsMatching: aString [

	| user stream list |
	list := (aString subStrings: $*) asOrderedCollection collect: [:each | each asUppercase].
	list size - 1 to: 1 do: [:i | list add: $* afterIndex: i].
	aString last = $* ifTrue: [list addLast: $*].
	stream := WriteStream on: String new.
	user := AllUsers 
		userWithId: #SymbolUser 
		ifAbsent: [AllUsers userWithId: #DataCurator].
	list := list asArray.
	list := (user resolveSymbol: #AllSymbols) value select: [:each |each asUppercase matchPattern: list].
	list := list select: [:each | (self classOrganizer implementorsOf: each) notEmpty].
	list := list asSortedCollection.
	list do: [:each | stream nextPutAll: each; nextPut: Character lf; yourself].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> sendersOf: anObject [

	| symbol |
	symbol := (anObject isKindOf: String)
		ifTrue: [anObject asSymbol]
		ifFalse: [anObject selector].
	^self streamOfMethods: (self classOrganizer sendersOf: symbol) first.


]

{ #category : 'category' }
JadeServer >> sendSigAbortToSession: anInteger [

	System sendSigAbortToSession: anInteger negated.


]

{ #category : 'category' }
JadeServer >> sendSigUsr1ToSession: anInteger [

	| description command result |
	description := System descriptionOfSession: anInteger.
	command := 'kill -usr1 ' , (description at: 2) printString.
	result := System performOnServer: command.
	result trimSeparators notEmpty ifTrue: [self error: result trimSeparators].


]

{ #category : 'category' }
JadeServer >> setBreakAtStepPoint: anInteger inMethod: aGsMethod [

	aGsMethod setBreakAtStepPoint: anInteger.


]

{ #category : 'category' }
JadeServer >> show: anObject [

	self nextPutAll: anObject printString.


]

{ #category : 'category' }
JadeServer >> sleepAndCommit [

	[
		System commitTransaction.
	] whileTrue: [
		(Delay forSeconds: 30) wait.
	].


]

{ #category : 'category' }
JadeServer >> sourceFor: anObject in: aClass [

	| behavior selector packageName category mcTimestamp dict source |
	selector := (anObject isKindOf: String) 
		ifTrue: [anObject asSymbol]
		ifFalse: [anObject selector].
	behavior := self
		behaviorFor: selector 
		in: aClass.
	category := self _behavior: behavior categoryOfSelector: selector.
	packageName := category first = $*
		ifTrue: [self _packageNameFor: category]
		ifFalse: [behavior thisClass _classCategory].
	packageName isNil ifTrue: [packageName := ''].
	mcTimestamp := ''.
	dict := behavior extraDict.
	dict notNil ifTrue: [
		dict := dict at: #'GSMethodStampDict' ifAbsent: [nil].
		dict notNil ifTrue: [
			mcTimestamp := dict
				at: selector
				ifAbsent: [''].
		].
	].
	source := behavior sourceCodeAt: selector.
	^(WriteStream on: String new)
		nextPutAll: packageName; tab;
		nextPutAll: category; tab;
		nextPutAll: mcTimestamp; lf;
		nextPutAll: source;
		contents.


]

{ #category : 'category' }
JadeServer >> sourceForProcess: gsProcess frame: level [

	self subclassResponsibility

]

{ #category : 'category' }
JadeServer >> stackForProcess: aGsProcess [

	| array stream |
	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | nil].
	array := aGsProcess _reportOfSize: 5000.
	stream := WriteStream on: String new.
	array do: [:each | 
		stream nextPutAll: each; lf.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> step: aGsProcess inFrame: anInteger [
	aGsProcess _stepOverInFrame: anInteger.
]

{ #category : 'category' }
JadeServer >> stepPointsFor: aGsMethod in: aClass [

	| behavior method source breakStepPoints stepPoint stream |
	behavior := self
		behaviorFor: aGsMethod selector
		in: aClass.
	source := behavior sourceCodeAt: aGsMethod selector.
	method := self compiledMethodAt: aGsMethod selector inClass: behavior.
	stream := WriteStream on: String new.
	breakStepPoints := (aGsMethod class canUnderstand: #'_breakpointIpOffsets')
		ifTrue: [aGsMethod _stepPointsFromBreakIpOffsets: aGsMethod _breakpointIpOffsets]
		ifFalse: [#()].
	stepPoint := 0.
	method _sourceOffsets do: [:each | 
		stepPoint := stepPoint + 1.
		(breakStepPoints includes: stepPoint) ifTrue: [stream nextPut: $B].
		each printOn: stream.
		stream nextPut: Character space.
	].
	stream lf; 
		nextPutAll: (self stringOfLineNumbersWithBreaksIn: method); lf;
		nextPutAll: source;
		yourself.
	^stream contents.


]

{ #category : 'category' }
JadeServer >> stoneInfo [

	| dict stream |
	stream := (WriteStream on: String new)
		nextPutAll: self streamType; tab;
		nextPutAll: self stringType; tab;
		cr;
		yourself.
	dict := System stoneVersionReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	dict := System stoneConfigurationReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	^stream contents

]

{ #category : 'category' }
JadeServer >> stopSession: anInteger [

	System stopSession: anInteger.


]

{ #category : 'category' }
JadeServer >> streamOfMethods: aList [

	| stream |
	stream := WriteStream on: String new.
	aList do: [:each | 
		self
			_addMethod: each 
			toStream: stream.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> streamType [

	^'Legacy'

]

{ #category : 'category' }
JadeServer >> stringForClassList: aList [

	| stream |
	stream := WriteStream on: String new.
	aList do: [:each | 
		self 
			_addClass: each 
			toStream: stream.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> stringType [

	^'String'

]

{ #category : 'category' }
JadeServer >> subclassSelectorForClass: aClass [

	(aClass isBytes and: [aClass superclass notNil and: [aClass superclass isBytes not]]) ifTrue: [
		^'byteSubclass:'.
	].
	(aClass isIndexable and: [aClass superclass notNil and: [aClass superclass isIndexable not]]) ifTrue: [
		^'indexableSubclass:'.
	].
	((aClass class canUnderstand: #'isTransientDB') and: [aClass isTransientDB]) ifTrue: [
		^'transientSubclass:'.
	].
	^'subclass:'.


]

{ #category : 'category' }
JadeServer >> superclassesOf: aClass isMeta: aBoolean [

	| myClass list |
	myClass := aBoolean ifTrue: [aClass class] ifFalse: [aClass].
	list := myClass _allSuperList , (Array with: myClass).
	^self stringForClassList: list.


]

{ #category : 'category' }
JadeServer >> symbolList [

	^Rowan image symbolList
]

{ #category : 'category' }
JadeServer >> systemBrowser: aString [
	^self copy systemBrowserA: aString.


]

{ #category : 'category' }
JadeServer >> systemBrowserA: aString [

	| time |
	time := self millisecondsElapsedTime: [
		selections := Dictionary new.
		readStream := ReadStream on: aString.
		writeStream := WriteStream on: String new.
		writeStream lf.
		self environment: (readStream upTo: Character space) asNumber.
		self systemBrowserCommand.
	].
	^time printString , writeStream contents.


]

{ #category : 'category' }
JadeServer >> systemBrowserCommand [

	| list command |
	list := self nextLineAsList asOrderedCollection.
	command := list removeFirst.
	command = 'addDictionary'				ifTrue: [^self sbAddDictionary: list].
	command = 'addMethodCategory' 		ifTrue: [^self sbAddMethodCategory: list].
	command = 'addMissingAccessors'		ifTrue: [^self sbAddMissingAccessors: list].
	command = 'addPackage' 				ifTrue: [^self sbAddPackage: list].
	command = 'addRepository'			ifTrue: [^self sbAddRepository: list].

	command = 'break' 					ifTrue: [^self sbBreak: list].
	command = 'browseClassReferences'		ifTrue: [^self sbBrowseClassReferences: list].
	command = 'browseGlobalReferences'		ifTrue: [^self sbBrowseGlobalReferences: list].
	command = 'browseImplementors'		ifTrue: [^self sbBrowseImplementors: list].
	command = 'browseMethodHistory'		ifTrue: [^self sbBrowseMethodHistory: list].
	command = 'browseMethodsContaining'	ifTrue: [^self sbBrowseMethodsContaining: list].
	command = 'browseMethodsWithPragma'	ifTrue: [^self sbBrowseMethodsWithPragma: list].
	command = 'browseSenders'			ifTrue: [^self sbBrowseSenders: list].

	command = 'changeClassName'			ifTrue: [^self sbChangeClassName: list].
	command = 'changesInPackage'			ifTrue: [^self sbChangesInPackage: list].
	command = 'checkUniqueClassName'		ifTrue: [^self sbCheckUniqueClassName: list].
	command = 'class' 					ifTrue: [^self sbClass: list].
	command = 'classCategory'				ifTrue: [^self sbClassCategory: list].
	command = 'classComment'			ifTrue: [^self sbClassComment: list].
	command = 'classesToDictionary'		ifTrue: [^self sbClassesToDictionary: list].
	command = 'comparePackages'			ifTrue: [^self sbComparePackages: list].

	command = 'fileOutClass'				ifTrue: [^self sbFileOutClass: list].
	command = 'fileOutDictionary'			ifTrue: [^self sbFileOutDictionary: list].
	command = 'fileOutMethod'			ifTrue: [^self sbFileOutMethod: list].
	command = 'findClass' 				ifTrue: [^self sbFindClass].
	command = 'findSelectors'				ifTrue: [^self sbFindSelectors: list].


	command = 'listMethodPragmas'			ifTrue: [^self sbListMethodPragmas: list].
	command = 'loadLatestVersion'			ifTrue: [^self sbLoadLatestVersionOfConfiguration: list].

	command = 'method' 					ifTrue: [^self sbSaveMethod: list].
	command = 'methodCategory'			ifTrue: [^self sbMethodCategory: list].
	command = 'methodClass'				ifTrue: [^self sbMethodClass: list].

	command = 'objectLog'				ifTrue: [^self sbObjectLog: list].

	command = 'postSaveClass'			ifTrue: [^self sbPostSaveClass: list].

	command = 'removeClasses'			ifTrue: [^self sbRemoveClasses].
	command = 'removeDictionaries'			ifTrue: [^self sbRemoveDictionaries: list].
	command = 'removeGlobals'			ifTrue: [^self sbRemoveGlobals].
	command = 'removeMethodCategories' 	ifTrue: [^self sbRemoveMethodCategories: list].
	command = 'removeMethods'			ifTrue: [^self sbRemoveMethods: list].
	command = 'removePriorVersions'		ifTrue: [^self sbRemovePriorVersions].
	command = 'removeRepository'			ifTrue: [^self sbRemoveRepository: list].
	command = 'revertClass'				ifTrue: [^self sbRevertClass].

	command = 'savePackage'				ifTrue: [^self sbSavePackage: list].
	command = 'setHomeDictionary'			ifTrue: [^self sbSetHomeDictionary: list].

	command = 'uniqueVersionName'		ifTrue: [^self sbUniqueVersionName: list].
	command = 'unloadPackage'			ifTrue: [^self sbUnloadPackage: list].
	command = 'update' 					ifTrue: [^self systemBrowserUpdate].

	self error: 'Unknown command: ' , command printString.


]

{ #category : 'category' }
JadeServer >> systemBrowserSTON: aString [
	^self copy systemBrowserSTONA: aString.


]

{ #category : 'category' }
JadeServer >> systemBrowserSTONA: aString [

	| time |
	time := self millisecondsElapsedTime: [
		selections := Dictionary new.
		readStream := ReadStream on: aString.
		writeStream := WriteStream on: String new.
		writeStream lf.
		self environment: (readStream upTo: Character space) asNumber.
		self systemBrowserCommand.
	].
	^STON toString: RowanCommandResult results


]

{ #category : 'category' }
JadeServer >> systemConfigAsDictionary [

	| char dict i line list stream |
	list := Array new.
	stream := GsFile openReadOnServer: '$GEMSTONE/data/system.conf'.
	[
		[
			line := stream nextLine reject: [:each | each == Character cr or: [each == Character lf]].
			(2 < line size and: [(line copyFrom: 1 to: 2) = '#=']) ifTrue: [
				list add: (WriteStream on: String new).
			] ifFalse: [
				list last nextPutAll: line; cr.
			].
			stream atEnd not.
		] whileTrue: [].
	] ensure: [
		stream close.
	].
	list := list copyFrom: 3 to: list size.
	list := list collect: [:each | each contents].
	dict := Dictionary new.
	list do: [:each | 
		line := (ReadStream on: each) nextLine.
		line = '# End of Default GemStone Configuration Options' ifTrue: [^dict].
		(line copyFrom: 1 to: 2) = '# ' ifFalse: [line error: 'Unrecognized config file format!'].
		i := 3.
		[
			i <= line size and: [(char := line at: i) == $_ or: [char isAlphaNumeric]].
		] whileTrue: [
			i := i + 1.
		].
		dict at: (line copyFrom: 3 to: i - 1) put: each.
	].
	self error: 'End of file not recognized!'.

]

{ #category : 'category' }
JadeServer >> terminate: aGsProcess [

	aGsProcess isNil ifTrue: [^self].
	aGsProcess terminate.
	(Delay forMilliseconds: 10) wait.	"allow forked processes to finish"


]

{ #category : 'jadeite' }
JadeServer >> updateFromSton: stonString [
  | services organizer resultString |
  [ 
  Rowan commandResultClass initializeResults.
  services := STON fromString: stonString.
  organizer := ClassOrganizer new.
  [ 
  services
    do: [ :service | 
      service organizer: organizer.
      service updateType: nil.	"Update type is only for returned commands"
      service command ifNil: [ service command: #'update' ].
      service servicePerform: service command withArguments: service commandArgs ] ]
    on: GsInteractionRequest
    do: [ :ex | 
      ex
        response:
          (ex interaction interactWith: self gsInteractionInformFailureHandler) ].
  self autoCommitIfRequired.
  Rowan loggingServiceClass current logSentServices.
  resultString := STON toString: Rowan commandResultClass results.
  ^ resultString ]
    on: Error
    do: [ :ex | 
      RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
      ex pass ]
]

{ #category : 'category' }
JadeServer >> userList [

	| list me stream |
	list := (AllUsers asSortedCollection: [:a :b | a userId <= b userId]) asOrderedCollection.
	me := System myUserProfile.
	list
		remove: me;
		addFirst: me;
		yourself.
	stream := WriteStream on: String new.
	list do: [:each | 
		self
			addUser: each 
			toStream: stream.
	].
	^stream contents.


]

{ #category : 'category' }
JadeServer >> waitingProcesses [

	^ProcessorScheduler scheduler waitingProcesses


]

{ #category : 'category' }
JadeServer >> writeList: aList [

	aList do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.


]
