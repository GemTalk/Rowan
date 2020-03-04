Class {
	#name : 'JadeServer64bit3x',
	#superclass : 'JadeServer64bit24',
	#instVars : [
		'environment'
	],
	#category : 'Rowan-JadeServer'
}

{ #category : 'category' }
JadeServer64bit3x >> _allSelectors [

	| allSelectors |
	allSelectors := IdentitySet new.
	self classOrganizer classes do: [:each | 
		allSelectors addAll: (each selectorsForEnvironment: environment); addAll: (each class selectorsForEnvironment: environment).
	].
	^allSelectors

]

{ #category : 'category' }
JadeServer64bit3x >> _behavior: aBehavior categoryOfSelector: aSymbol [

	^aBehavior categoryOfSelector: aSymbol environmentId: environment

]

{ #category : 'category' }
JadeServer64bit3x >> _describeMethod: aMethod [

	(aMethod class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , aMethod class name].
	^super _describeMethod: aMethod

]

{ #category : 'category' }
JadeServer64bit3x >> _methodsFor: aClass categories: aList [

	| methods |
	methods := IdentitySet new.
	aList do: [:eachCategory | 
		(aClass includesCategory: eachCategory) ifTrue: [
			(aClass selectorsIn: eachCategory) do: [:eachSelector |
				methods add: (self compiledMethodAt: eachSelector inClass: aClass).
			].
		].
	].
	^methods.



]

{ #category : 'category' }
JadeServer64bit3x >> _stepPointsForBreaksInMethod: gsMethod [
	"Presumably there is a way to do this, just not the same as 32-bit and 64-bit 2.x"

	^#()

]

{ #category : 'category' }
JadeServer64bit3x >> addMethodCategoryNamesToMethodFilters [

	classList do: [:each | 
		each 
			env: environment 
			categorysDo:[ :categName :selectors | methodFilters add: categName ].
	].


]

{ #category : 'category' }
JadeServer64bit3x >> addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler [

	| instVarNumber modeInfo modeInfo_forked modeInfo_terminated |
	super addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler.
	(instVarNumber := GsProcess instVarNames indexOf: #'modeInfo') == 0 ifTrue: [^self].
	modeInfo := aProcess instVarAt: instVarNumber.
	(modeInfo_forked := GsProcess _classVars at: #'ModeInfo_forked' ifAbsent: [nil]) ifNil: [^self].
	(modeInfo_terminated := GsProcess _classVars at: #'ModeInfo_terminated' ifAbsent: [nil]) ifNil: [^self].
	aStream
"9"		nextPutAll: (0 < (modeInfo bitAnd: modeInfo_forked) ifTrue: ['forked'] ifFalse: ['main']); tab;
"10"		nextPutAll: (0 < (modeInfo bitAnd: modeInfo_terminated) ifTrue: ['terminated'] ifFalse: ['']); tab;
		yourself.


]

{ #category : 'category' }
JadeServer64bit3x >> asString: anObject [

	(anObject isKindOf: String) ifTrue: [^anObject].
	(anObject _class name == #'ClientForwarder') ifTrue: [^'aClientForwarder(' , (self asString: anObject clientObject) , ')'].
	^[
		anObject printString.
	] on: Error , Admonition do: [:ex | 
		ex return: '<<printString error: ' , ex description , '>>'.
	].


]

{ #category : 'category' }
JadeServer64bit3x >> categoryOfMethod: aMethod [

	| category selector |
	(selector := aMethod selector) isNil ifTrue: [^''].
	category := self _behavior: aMethod inClass categoryOfSelector: aMethod selector.
	category ifNil: [category := #'other'].
	^category.


]

{ #category : 'category' }
JadeServer64bit3x >> class: aClass includesSelector: aSelector [

	^aClass includesSelector: aSelector asSymbol environmentId: environment.


]

{ #category : 'category' }
JadeServer64bit3x >> compiledMethodAt: aSymbol inClass: aClass [

	| method | 
	method := aClass compiledMethodAt: aSymbol environmentId: environment.
	method ifNil: [self error: 'Lookup failed for selector ' , aSymbol , ' inClass ' , aClass name , ' in environment ' , environment printString].
	^method.

]

{ #category : 'category' }
JadeServer64bit3x >> compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol [
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| method warnings | 

	[[ 
			[[ method := aBehavior rwCompileMethod: methodString category: categorySymbol ]
					on: RwExecuteClassInitializeMethodsAfterLoadNotification
					do: [:ex | ex resume: false ]]
						on: RwPerformingUnpackagedEditNotification
						do: [:ex | ex resume ].
		] on: CompileError do: [:ex |
		^nil -> (ex gsArguments at: 1)
	]] on: CompileWarning do: [:ex |
		warnings := ex gsArguments at: 1.
		ex resume.
	].
	^[	
		(self compiledMethodAt: method key selector inClass: aBehavior) -> warnings.
	] on: Error do: [:ex | 
		ex return: method -> warnings.
	].
]

{ #category : 'category' }
JadeServer64bit3x >> debugString: aString fromContext: anObject environment: anInteger [

	[
		^super debugString: aString fromContext: anObject environment: anInteger.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].


]

{ #category : 'category' }
JadeServer64bit3x >> describeMethod: aMethod [
	"Provide info needed to create a GsMethod in Jade client"

	(aMethod class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , aMethod class name].
	^super describeMethod: aMethod

]

{ #category : 'category' }
JadeServer64bit3x >> environment [

	^environment

]

{ #category : 'category' }
JadeServer64bit3x >> environment: anInteger [

	environment := anInteger.


]

{ #category : 'category' }
JadeServer64bit3x >> environmentForMethod: aGsNMethod [

	^aGsNMethod environmentId

]

{ #category : 'category' }
JadeServer64bit3x >> executeString: aString fromContext: anObject environment: anInteger [

	[
		^super executeString: aString fromContext: anObject environment: anInteger.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].


]

{ #category : 'category' }
JadeServer64bit3x >> homeMethodFor: aGsMethod [

	| result |
	result := super homeMethodFor: aGsMethod.
	(result class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , result class name].
	^result

]

{ #category : 'category' }
JadeServer64bit3x >> initialize [

	super initialize.
	environment := 0.


]

{ #category : 'category' }
JadeServer64bit3x >> inspect: anObject [
	| dynamic dynamicSize indexedSize instVarNames namedSize stream string isRcBag |
	(self isClientForwarder: anObject) ifTrue: [^self inspectClientForwarder: anObject].
	(stream := WriteStream on: String new)
		nextPutAll: anObject class name; tab;
		yourself.
	(self oopOf: anObject) printOn: stream.
	stream lf.
	(anObject isKindOf: Dictionary superclass) ifTrue: [^self inspectDictionary: anObject on: stream].
	instVarNames := anObject class allInstVarNames.
	namedSize := instVarNames size.
	dynamic := anObject dynamicInstanceVariables.
	dynamicSize := dynamic size.
	isRcBag := anObject class name == #RcIdentityBag.
	indexedSize := (anObject class isNsc or: [anObject class isIndexable]) ifFalse: [
		0.
	] ifTrue: [
		isRcBag ifTrue: [ anObject size] ifFalse: [(anObject _primitiveSize - namedSize)].
	].
	
	namedSize + dynamicSize + indexedSize printOn: stream.
	stream lf.
	1 to: instVarNames size do: [:i | 
		stream nextPutAll: (instVarNames at: i); tab.
		self print: (self oopOf: (anObject instVarAt: i)) on: stream.
		stream lf.
	].
	1 to: dynamicSize do: [:i | 
		stream nextPutAll: (dynamic at: i); tab.
		self print: (self oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) on: stream.
		stream lf.
	].
	isRcBag
		ifTrue: [ |aBag |
			aBag := anObject _asIdentityBag.
			1 to: indexedSize do: [:i | 
				i printOn: stream.
				stream tab.
				self print: (self oopOf: (aBag _at: i )) on: stream.
				stream lf]]
		ifFalse: [
			1 to: indexedSize do: [:i | 
				i printOn: stream.
				stream tab.
				self print: (self oopOf: (anObject _primitiveAt: i + namedSize)) on: stream.
				stream lf] ].

	(string := anObject printString) size > 100000 ifTrue: [string := (string copyFrom: 1 to: 100000) , '...'].
	string class == String ifFalse: [
		string := String withAll: (string collect: [:each | (32 <= each codePoint and: [each codePoint <= 255]) ifTrue: [each] ifFalse: [$?]]).
	].
	^stream 
		nextPutAll: string; 
		contents.
]

{ #category : 'category' }
JadeServer64bit3x >> inspectNamedInstanceVariablesOf: anObject on: aStream [

	| list dynamic size |
	list := anObject class allInstVarNames.
	dynamic := anObject dynamicInstanceVariables.
	size := list size + dynamic size.
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
	1 to: dynamic size do: [:i | 
		aStream nextPutAll: (dynamic at: i); tab.
		self print: (self oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) on: aStream.
		aStream lf.
	].


]

{ #category : 'transcript' }
JadeServer64bit3x >> installTranscript [

	Transcript class name == #'TranscriptStreamPortable' ifFalse: [^self].
	SessionTemps current at: #'TranscriptStream_SessionStream' put: self.
]

{ #category : 'category' }
JadeServer64bit3x >> keysForDictionary: aDictionary [ 
	"RubyHash does not implement #'keys' or #'keysDo:'!"

	| keys |
	(aDictionary isKindOf: RubyHash) ifFalse: [^super keysForDictionary: aDictionary].
	keys := Set new.
	aDictionary keysAndValuesDo: [:eachKey :eachValue | keys add: eachKey].
	^keys.


]

{ #category : 'category' }
JadeServer64bit3x >> methodSignatureForSelector: aSymbol [
	"Ruby bridge methods can have some strange selectors!"

	| class comma i j method source |
	environment ~~ 1 ifTrue: [^aSymbol].
	class := selectedClass whichClassIncludesSelector: aSymbol environmentId: environment.
	method := class compiledMethodAt: aSymbol environmentId: environment.
	source := (method sourceString subStrings: Character lf) first trimBlanks.
	(4 < source size and: [(source copyFrom: 1 to: 4) = 'def ']) ifTrue: [
		source := source copyFrom: 5 to: source size.
		(source includes: $#) ifTrue: [source := (source copyFrom: 1 to: (source indexOf: $#) - 1) trimBlanks].
		^source.
	].
	(i := aSymbol indexOf: $#) == 0 ifTrue: [^aSymbol].
	source := aSymbol copyFrom: 1 to: i - 1.
	(aSymbol copyFrom: i to: aSymbol size) = '#0__' ifTrue: [^source].
	comma := ''.
	source add: $(.
	j := (aSymbol at: i + 1) asString asNumber.
	1 to: j do: [:k | 
		source 
			add: comma;
			add: 'arg'.
		1 < j ifTrue: [source add: k printString].
		comma := $,.
	].
	(aSymbol at: i + 2) == $* ifTrue: [
		source 
			add: comma;
			add: (0 == j ifTrue: ['args'] ifFalse: ['rest']).
		comma := $,.
	].
	aSymbol last == $& ifTrue: [
		source
			add: comma;
			add: '&block'.
	].
	source add: $).
	^source.


]

{ #category : 'category' }
JadeServer64bit3x >> nameOfFirstDictionaryReferencing: aGlobal [

	| list |
	list := self symbolList dictionariesAndSymbolsOf: aGlobal.
	list isEmpty ifTrue: [^''].
	^list first first name

]

{ #category : 'category' }
JadeServer64bit3x >> nextPutAll: anObject [

	| exception |
	exception := ClientForwarderSend new 
		receiver: self 
		clientObj: 2
		selector:#'nextPutAll:'
		args: (Array with: (self asString: anObject)).
	exception defaultAction.  "return error direct to GCI"

]

{ #category : 'category' }
JadeServer64bit3x >> objectSecurityPolicyFor: anObject [

	^anObject objectSecurityPolicy.


]

{ #category : 'category' }
JadeServer64bit3x >> packagePolicy: aPackagePolicy includesSelector: aSymbol forClass: aClass [

	| dict |
	^aPackagePolicy notNil and: [
		(dict := aClass transientMethodDictForEnv: environment) notNil and: [
			dict keys includes: aSymbol.		"includesKey: requires protected mode!"
		].
	].


]

{ #category : 'category' }
JadeServer64bit3x >> sbBrowseMethodsWithPragma: anOrderedCollection [

	| methods symbol |
	symbol := anOrderedCollection removeFirst asSymbol.
	methods := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal | 
			eachGlobal isBehavior ifTrue: [
				(Array with: eachGlobal class with: eachGlobal) do: [:eachBehavior | 
					(eachBehavior methodDictForEnv: environment) do: [:eachMethod | 
						(eachMethod pragmas anySatisfy: [:eachPragma | symbol == eachPragma keyword]) ifTrue: [methods add: eachMethod].
					].
				].
			].
		].
	].
	writeStream 
		nextPutAll: 'browseMethodsWithPragma'; lf;
		nextPutAll: (self streamOfMethods: methods);
		yourself.


]

{ #category : 'category' }
JadeServer64bit3x >> sbClassComment: anOrderedCollection [

	(self sbClassFrom: anOrderedCollection) thisClass rwComment: self sbNextParagraph trimSeparators.
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer64bit3x >> sbFileOutMethod: anOrderedCollection [

	| aClass |
	aClass := self sbClassFrom: anOrderedCollection.
	writeStream nextPutAll: (aClass fileOutMethod: anOrderedCollection removeFirst asSymbol environmentId: environment).



]

{ #category : 'category' }
JadeServer64bit3x >> sbListMethodPragmas: aList [

	| pragmas |
	pragmas := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal | 
			eachGlobal isBehavior ifTrue: [
				(Array with: eachGlobal with: eachGlobal class) do: [:eachBehavior | 
					(eachBehavior methodDictForEnv: environment) do: [:eachMethod | 
						pragmas addAll: (eachMethod pragmas collect: [:eachPragma | eachPragma keyword]).
					].
				].
			].
		].
	].
	pragmas asSortedCollection do: [:each | writeStream nextPutAll: each; tab].
	^pragmas

]

{ #category : 'category' }
JadeServer64bit3x >> sbMethod: anOrderedCollection [

	| behavior category string gsMethod |
	behavior := self sbClassFrom: anOrderedCollection.
	category := anOrderedCollection notEmpty ifTrue: [anOrderedCollection removeFirst] ifFalse: ['other'].
	string := self sbNextParagraph.
	gsMethod := behavior
		compileMethod: string 
		dictionaries: self symbolList 
		category: category asSymbol 
		environmentId: environment.
	selections 
		at: #'methodCategory' 	put: (self _behavior: gsMethod inClass categoryOfSelector: gsMethod selector) asString;
		at: #'method'			put: gsMethod selector asString;
		yourself.
	self systemBrowserUpdate.


]

{ #category : 'category' }
JadeServer64bit3x >> sbUpdateMethodBreakPointsFor: aMethod [
	"Answers an Array of step points"

	| list array |
	(array := aMethod _allBreakpoints) isNil ifTrue: [^#()].      "{ breakpointNumber1 . method . ipOffset1 . ... }"
	list := Array new.
	1 to: array size by: 3 do:[:k |
		list add: (aMethod
			_stepPointForMeth: (array at: k + 1)
			ip: (array at: k + 2)).
	].
	^list.


]

{ #category : 'category' }
JadeServer64bit3x >> sbUpdateMethodPragmas [

	selectedClass notNil ifTrue:[
		methodCommandResult selections add: selectedClass asString].
	methodCommandResult updateMethodPragmas.
	methodCommandResult writeMethodFiltersTo: writeStream. 
	methodCommandResult updateMethodFilterSelections: self nextLineAsList 
		hasCategoryOverride: (selections at: #'methodCategory' ifAbsent: [nil]).
	methodFilters := methodCommandResult selectionNames.
	methodCommandResult writeSelectionsTo: writeStream. 



]

{ #category : 'category' }
JadeServer64bit3x >> sbUpdateMethodsByCategories [

	| selectors |
	selectors := IdentitySet new.
	classList do: [:eachClass |
		(eachClass selectorsForEnvironment: environment) do: [:eachSelector |
			(methodFilters isEmpty or: [
				| category |
				category := eachClass categoryOfSelector: eachSelector environmentId: environment.
				(category isNil and: [ methodFilters includes: #'other']) or: [ methodFilters includes: category asSymbol]]) ifTrue: [
				| method |
				method := eachClass compiledMethodAt: eachSelector environmentId: environment.
				(method respondsTo: #isRubyBridgeMethod)
					ifTrue: [ 
						method isRubyBridgeMethod ifFalse: [
							selectors add: eachSelector ] ]
					ifFalse: [ selectors add: eachSelector ]
			].
		].
	].
	^selectors.
]

{ #category : 'category' }
JadeServer64bit3x >> sbUpdateMethodsByPragmas [

	| selectors |
	selectors := IdentitySet new.
	methodFilters isEmpty ifTrue: [^#()].
	(selectedClass selectorsForEnvironment: environment) do: [:eachSelector | 
		| gsMethod |
		gsMethod := selectedClass compiledMethodAt: eachSelector environmentId: environment.
		gsMethod pragmas do: [:eachPragma | 
			( methodFilters includes: eachPragma keyword) ifTrue: [selectors add: eachSelector].
		].
	].
	^selectors.


]

{ #category : 'category' }
JadeServer64bit3x >> sbUpdateMethodsByVariables [

	| selectors filters |
	selectors := IdentitySet new.
	filters := IdentitySet withAll: (methodFilters select: [:each | each isSymbol]).
	(selectedClass selectorsForEnvironment: environment) do: [:eachSelector | 
		| gsMethod |
		gsMethod := selectedClass compiledMethodAt: eachSelector environmentId: environment.
		(gsMethod instVarsAccessed * filters) notEmpty ifTrue: [selectors add: eachSelector].
	].
	^selectors.


]

{ #category : 'category' }
JadeServer64bit3x >> sbUpdateMethodStepPointsFor: aMethod [
	"Answers an Array of Associations (offset -> selector) indexed by step point"

	|  selectors list |
	(selectors := aMethod _allDebugInfo: 10) ifNil: [^#()].
	list := (self homeMethodFor: aMethod)  _sourceOffsets.
	list := list collect: [:each | 		"exists as far back as 32-bit 6.3.0"
		| index selector |
		selector := ''.
		index := selectors indexOf: each.
		0 < index ifTrue: [selector := selectors at: index + 1].
		each -> selector.
	].
	^list.



]

{ #category : 'category' }
JadeServer64bit3x >> selectedClassOverridesSelector: aSymbol [

	^selectedClass superclass notNil and: [(selectedClass superclass whichClassIncludesSelector: aSymbol environmentId: environment) ~~ nil].


]

{ #category : 'transcript' }
JadeServer64bit3x >> uninstallTranscript [

	Transcript class name == #'TranscriptStreamPortable' ifFalse: [^self].
	SessionTemps current at: #'TranscriptStream_SessionStream' put: nil.
]
