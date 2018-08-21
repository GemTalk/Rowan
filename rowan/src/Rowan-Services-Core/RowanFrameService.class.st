Class {
	#name : 'RowanFrameService',
	#superclass : 'RowanService',
	#instVars : [
		'label',
		'method',
		'stepPoint',
		'vars'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'other' }
RowanFrameService class >> process: aGsProcess level: anInteger organizer: aClassOrganizer [

	^self basicNew
		initializeProcess: aGsProcess level: anInteger organizer: aClassOrganizer;
		yourself

]

{ #category : 'other' }
RowanFrameService >> compileMethod: methodString behavior: aBehavior user: aUserProfileOrNil inCategory: categoryString [
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

{ #category : 'other' }
RowanFrameService >> initializeProcess: aGsProcess level: anInteger organizer: aClassOrganizer [
	"In 3.2.15 the server does some whacky things with IP, stepPoint, and nested methods.
	See http://kermit.gemtalksystems.com/bug?bug=45553 --JGF"

	| frameData gsNMethod |
	frameData := aGsProcess _frameContentsAt: anInteger.
	gsNMethod := frameData at: 1.
	label := aGsProcess _reportAt: anInteger.
	method := RowanMethodService forGsNMethod: gsNMethod organizer: aClassOrganizer.
	stepPoint := gsNMethod == gsNMethod homeMethod
		ifTrue: [aGsProcess _stepPointAt: anInteger]
		ifFalse: [gsNMethod homeMethod _stepPointForMeth: gsNMethod ip: (frameData at: 2)].
	vars := self varsFor: frameData.
]

{ #category : 'other' }
RowanFrameService >> recompile: aMethod withSource: aString [

	| result behavior |
	behavior := aMethod inClass.
	(UserGlobals at: #rowanCompile ifAbsent: [false])
		ifTrue: 
			[behavior rwCompileMethod: aString
				category: (behavior categoryOfSelector: aMethod selector).
			Rowan serviceClass rowanFixMe.	"need to handle compile errors"
			^true]
		ifFalse: 
			[result := aMethod _recompileWithSource: aString.
			result isNil ifTrue: [^true].	"Bug 41195 returns nil if success so assume it is the same method"
			^result]
]

{ #category : 'other' }
RowanFrameService >> saveMethod: aString [

	| oldMethod aBehavior selector category result |
	oldMethod := method gsNMethodFromOop.
	result := self recompile: oldMethod withSource: aString.
	(result isKindOf: Boolean) ifTrue: [^result].
	aBehavior := oldMethod inClass.
	selector := oldMethod selector.
	selector isNil ifTrue: [^result].
	category := aBehavior categoryOfSelector: selector.
	result := (UserGlobals at: #rowanCompile ifAbsent: [false])
					ifTrue: [aBehavior rwCompileMethod: aString category: category]
					ifFalse: 
						[self
							compileMethod: aString
							behavior: aBehavior
							user: nil
							inCategory: category].
self halt.
	^result
]

{ #category : 'other' }
RowanFrameService >> testMethod [

	self halt.
	^0
]

{ #category : 'other' }
RowanFrameService >> varsFor: anArray [

	| keys list receiver values |
	receiver := anArray at: 10.
	values := OrderedCollection new.
	(Reflection classOf: receiver) name == #'ClientForwarder' ifTrue: [
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
	keys addAll: (anArray at: 9).
	keys := keys reject: [:each | each first == $.].
	values addAll: (anArray size >= 11
		ifTrue: [anArray copyFrom: 11 to: anArray size]
		ifFalse: [#()]).
	list := Array new.
	1 to: (keys size min: values size) do: [:i | | oop key value valueClass |
		key := keys at: i.
		value := values at: i.
		valueClass := value class.
		oop := value asOop.
		value := [
			value printString.
		] on: Error do: [:ex | 
			ex return: '(' , value class name , ' printString error: ' , ex description , ')'. 
		].
		value size > 500 ifTrue: [value := (value copyFrom: 1 to: 500) , '...'].
		value := value collect: [:char | (char asciiValue < 32 or: [127 < char asciiValue]) ifTrue: [$?] ifFalse: [char]].
		list add: (RowanVariableService oop: oop key: key value: value className: valueClass name asString).
	].
	^list

]
