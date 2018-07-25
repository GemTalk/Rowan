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
RowanFrameService >> initializeProcess: aGsProcess level: anInteger organizer: aClassOrganizer [

	| frameData gsNMethod |
	frameData := aGsProcess _frameContentsAt: anInteger.
	gsNMethod := frameData at: 1.

	label := aGsProcess _reportAt: anInteger.
	method := RowanMethodService forGsNMethod: gsNMethod organizer: aClassOrganizer.
	stepPoint := aGsProcess _stepPointAt: anInteger.
	vars := self varsFor: frameData.

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
	1 to: (keys size min: values size) do: [:i | | oop key value |
		key := keys at: i.
		value := values at: i.
		oop := value asOop.
		value := value printString.
		value size > 500 ifTrue: [value := (value copyFrom: 1 to: 500) , '...'].
		value := value collect: [:char | (char asciiValue < 32 or: [127 < char asciiValue]) ifTrue: [$?] ifFalse: [char]].
		list add: (RowanVariableService oop: oop key: key value: value).
	].
	^list

]
