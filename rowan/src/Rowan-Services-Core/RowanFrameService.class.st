"
No class-specific documentation for RowanFrameService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanFrameService( label method stepPoint vars oop homeMethodSelector homeMethodClassName classIsResolvable)

"
Class {
	#name : 'RowanFrameService',
	#superclass : 'RowanService',
	#instVars : [
		'label',
		'method',
		'stepPoint',
		'vars',
		'oop',
		'homeMethodSelector',
		'homeMethodClassName',
		'classIsResolvable'
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
  "In 3.2.15 the server does some whacky things with IP, stepPoint, and nested methods.
	See http://kermit.gemtalksystems.com/bug?bug=45553 --JGF"

  | frameData gsNMethod homeMethodService |
  frameData := aGsProcess _frameContentsAt: anInteger.
  frameData isNil
    ifTrue: [ ^ self	"not sure if bad frame data is a 3.2.15 bug or not" ].
  oop := (frameData at: 8) asOop.
  gsNMethod := frameData at: 1.
  label := aGsProcess _reportAt: anInteger.
  method := RowanMethodService
    forGsNMethod: gsNMethod
    organizer: aClassOrganizer.
  homeMethodService := RowanMethodService
    forGsNMethod: gsNMethod homeMethod
    organizer: aClassOrganizer.
  method breakPoints: homeMethodService breakPoints.
  homeMethodSelector := gsNMethod homeMethod selector.
  homeMethodClassName := gsNMethod homeMethod inClass
    ifNotNil: [ :cls | 
      | className |
      className := cls name asString.
      classIsResolvable := (Rowan image
        resolveClassNamed: cls theNonMetaClass name asString) isNil not.
      className ].
  stepPoint := gsNMethod == gsNMethod homeMethod
    ifTrue: [ aGsProcess _stepPointAt: anInteger ]
    ifFalse: [ gsNMethod homeMethod _stepPointForMeth: gsNMethod ip: (frameData at: 2) ].
  vars := self varsFor: frameData
]

{ #category : 'other' }
RowanFrameService >> varsFor: anArray [
	| keys list receiver values |
	receiver := anArray at: 10.
	values := OrderedCollection new.
	(Reflection classOf: receiver) name == #'ClientForwarder'
		ifTrue: [ 
			keys := OrderedCollection with: 'clientObject'.
			values add: receiver clientObject.
			receiver := '[aClientForwarder(' , (self oopOf: receiver) printString , ')]'.
			keys addAll: (anArray at: 9).
			keys := keys reject: [ :each | each first == $. ] ]
		ifFalse: [ 
			((receiver isKindOf: BlockClosure) or: [ receiver isKindOf: Class ])
				ifTrue: [ 
					keys := OrderedCollection new.
					keys addAll: (anArray at: 9).
					keys := keys reject: [ :each | each first == $. ] ]
				ifFalse: [ 
					keys := receiver class allInstVarNames asOrderedCollection
						collect: [ :each | '-' , each ].
					1 to: keys size do: [ :i | values add: (receiver instVarAt: i) ].
					keys
						addAll:
							(receiver dynamicInstanceVariables asOrderedCollection
								collect: [ :each | '$' , each asString ]).
					receiver dynamicInstanceVariables
						do: [ :symbol | values add: (receiver dynamicInstVarAt: symbol) ].
					keys addAll: (anArray at: 9).
					keys := keys reject: [ :key | key first == $. ].
					keys := keys collect: [ :key | key copyReplaceChar: $$ with: $. ] ] ].

	keys addFirst: #'receiver'.
	values addFirst: receiver.
	values
		addAll:
			(anArray size >= 11
				ifTrue: [ anArray copyFrom: 11 to: anArray size ]
				ifFalse: [ #() ]).
	list := Array new.
	1 to: (keys size min: values size) do: [ :i | 
		| theOop key value valueClass |
		key := keys at: i.
		value := values at: i.
		valueClass := value class.
		theOop := value asOop.
		value := [ value printString ]
			on: Error
			do: [ :ex | 
				ex
					return:
						'(' , value class name , ' printString error: ' , ex description , ')' ].
		value size > 500
			ifTrue: [ value := (value copyFrom: 1 to: 500) , '...' ].
		value := value
			collect: [ :char | 
				(char asciiValue < 32 or: [ 127 < char asciiValue ])
					ifTrue: [ $? ]
					ifFalse: [ char ] ].
		list
			add:
				(RowanVariableService
					oop: theOop
					key: key
					value: value
					className: valueClass name asString) ].
	^ list
]
