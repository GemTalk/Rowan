Class {
	#name : 'JadeServer64bit',
	#superclass : 'JadeServer',
	#category : 'Rowan-JadeServer'
}

{ #category : 'category' }
JadeServer64bit >> _oopAndStringFor: anObject [

	^[
		super _oopAndStringFor: anObject.
	] on: Error do: [:ex | 
		ex return: 0 -> ('<ERROR IN #printString for ' , anObject class name , '>').
	].


]

{ #category : 'category' }
JadeServer64bit >> _stepPointFromProcess: gsProcess frame: aFrame method: gsMethod level: anInteger [

	^gsProcess _stepPointAt: anInteger


]

{ #category : 'category' }
JadeServer64bit >> _trimStackOf: aGsProcess toLevel: anInteger [

	^[
		super
			_trimStackOf: aGsProcess 
			toLevel: anInteger.
	] on: Error do: [:ex | 
		self 
			_trimStackOf: aGsProcess 
			toLevel: anInteger - 1.
		ex return.
	].


]

{ #category : 'category' }
JadeServer64bit >> addSessionWithId: anInteger toStream: aStream [

	[
		super
			addSessionWithId: anInteger
			toStream: aStream.
	] on: Error do: [:ex | 
		ex resume: '?????'.
	].


]

{ #category : 'category' }
JadeServer64bit >> addUser: aUserProfile toStream: aStream [

	[
		super
			addUser: aUserProfile 
			toStream: aStream.
	] on: Error do: [:ex | 
		aStream lf.
		ex return.
	].


]

{ #category : 'category' }
JadeServer64bit >> asString: anObject [

	^[
		super asString: anObject.
	] on: Error do: [:ex | 
		ex return: '???'.
	].


]

{ #category : 'category' }
JadeServer64bit >> homeMethodFor: aGsMethod [

	^[aGsMethod homeMethod] on: Error do: [:ex | ex return: aGsMethod]

]

{ #category : 'category' }
JadeServer64bit >> installTranscript [

	[
		super installTranscript.
	] on: Error do: [:ex | 
		ex return.
	].


]

{ #category : 'category' }
JadeServer64bit >> mcInitialsA: aString [

	^[
		super mcInitialsA: aString.
		true.
	] on: Error do: [:ex | 
		ex return: false.
	].


]

{ #category : 'category' }
JadeServer64bit >> metacelloConfigurations [

	| list |
	list := Array new.
	Rowan image symbolList do: [:eachSymbolList | 
		eachSymbolList do: [:eachGlobal | 
			(eachGlobal isBehavior and: [
			(eachGlobal class includesSelector: #'isMetacelloConfig') and: [
			eachGlobal isMetacelloConfig]]) ifTrue: [list add: eachGlobal].
		].
	].
	^list
]

{ #category : 'category' }
JadeServer64bit >> objectForOop: anInteger [

	^Object _objectForOop: anInteger.


]

{ #category : 'category' }
JadeServer64bit >> recompile: aMethod withSource: aString [
	| behavior |
	behavior := aMethod inClass.
	[[ behavior rwCompileMethod: aString
				category: (self _behavior: behavior categoryOfSelector: aMethod selector) ]
					on: RwExecuteClassInitializeMethodsAfterLoadNotification
					do: [:ex | ex resume: false ]]
								on: RwPerformingUnpackagedEditNotification
								do: [:ex | ex resume ].
			Rowan serviceClass rowanFixMe.	"need to handle compile errors"
			^true
]

{ #category : 'category' }
JadeServer64bit >> sbRemoveKey: aSymbol fromDictionary: aDictionary [

	| aClass array |
	aClass := aDictionary at: aSymbol.
	array := self dictionaryAndSymbolOf: aClass.
	((array at: 1) == aDictionary and: [
		(array at: 2) == aSymbol and: [
		(Class canUnderstand: #'removeFromSystem') and: [	"mark package as modified"
		aClass removeFromSystem]]]) ifFalse: [
			aDictionary removeKey: aSymbol.
		].

]

{ #category : 'category' }
JadeServer64bit >> sourceForProcess: gsProcess frame: level [

	^[
		self
			_sourceForProcess: gsProcess 
			frame: level.
	] on: Error do: [:ex | 
			ex return: (self asAsciiString: ('?????' , ex description , Character cr asString , (GsProcess stackReportToLevel: 50))).
	].


]

{ #category : 'category' }
JadeServer64bit >> streamType [

	| isLegacy type |
	type := Globals at: #'PositionableStream_position' ifAbsent: [#'Legacy'].
	(Globals includesKey: #'PositionableStreamLegacy') ifFalse: [^type].
	isLegacy := PositionableStream isLegacyStreamImplementation.
	(type = #'Legacy') == isLegacy ifTrue: [^type].
	self error: 'Inconsistent PositionableStream configuration'.


]

{ #category : 'category' }
JadeServer64bit >> stringType [

	^(Globals at: #StringConfiguration ifAbsent: [String]) name


]

{ #category : 'category' }
JadeServer64bit >> systemBrowser: aString [

	[
		^super systemBrowser: aString.
	] on: Error do: [:ex |
		readStream := nil.
		ex pass.
	].


]

{ #category : 'category' }
JadeServer64bit >> systemBrowserSTON: aString [

	[
		^super systemBrowserSTON: aString.
	] on: Error do: [:ex |
		readStream := nil.
		ex pass.
	].


]
