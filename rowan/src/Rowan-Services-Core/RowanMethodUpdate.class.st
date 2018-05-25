Class {
	#name : 'RowanMethodUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'gsMethod',
		'firstSendOffsets',
		'method',
		'isOverride',
		'isTestClass'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanMethodUpdate >> breakPoints [
	
	^gsMethod breakPoints

]

{ #category : 'accessing' }
RowanMethodUpdate >> breakPoints: anObject [

	gsMethod breakPoints: anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> category [

	^gsMethod category

]

{ #category : 'accessing' }
RowanMethodUpdate >> category: anObject [

	gsMethod category: anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> className [

	^gsMethod behaviorName

]

{ #category : 'accessing' }
RowanMethodUpdate >> className: anObject [

	gsMethod behaviorName: anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> classOop [

	^gsMethod behavior

]

{ #category : 'accessing' }
RowanMethodUpdate >> classOop: anObject [

	gsMethod behavior: anObject.

]

{ #category : 'accessing' }
RowanMethodUpdate >> compilationWarnings [

	^gsMethod warnings

]

{ #category : 'accessing' }
RowanMethodUpdate >> compilationWarnings: aString [

	gsMethod warnings: aString

]

{ #category : 'accessing' }
RowanMethodUpdate >> firstSendOffsets [

	^firstSendOffsets

]

{ #category : 'initialization' }
RowanMethodUpdate >> initialize [

	firstSendOffsets := Dictionary new.
	gsMethod := RowanGsMethod new.

]

{ #category : 'accessing' }
RowanMethodUpdate >> isOverride [

	^isOverride

]

{ #category : 'accessing' }
RowanMethodUpdate >> isOverride: anObject [

	isOverride := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> isReadOnly [

	^gsMethod isReadOnly

]

{ #category : 'accessing' }
RowanMethodUpdate >> isReadOnly: anObject [

	gsMethod isReadOnly: anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> isTestClass [

	^isTestClass

]

{ #category : 'accessing' }
RowanMethodUpdate >> isTestClass: anObject [

	isTestClass := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> method: anObject [

	method := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> methodOop [

	^gsMethod oopType

]

{ #category : 'accessing' }
RowanMethodUpdate >> methodOop: anObject [

	gsMethod oopType: anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> originalMethodSource [

	 ^gsMethod originalSource

]

{ #category : 'accessing' }
RowanMethodUpdate >> originalMethodSource: anObject [

	gsMethod originalSource: anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> selector [

	^gsMethod name

]

{ #category : 'accessing' }
RowanMethodUpdate >> selector: anObject [

	gsMethod name: anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> source [

	^gsMethod source

]

{ #category : 'accessing' }
RowanMethodUpdate >> source: anObject [

	gsMethod source: anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> stepPoints [

	^gsMethod stepPoints

]

{ #category : 'accessing' }
RowanMethodUpdate >> stepPoints: anObject [

	gsMethod stepPoints: anObject

]

{ #category : 'jade' }
RowanMethodUpdate >> writeMethodTo: writeStream [
		
		writeStream 	"Line 1 for GsMethod (line 3 for JadeSystemBrowserPresenter)"
"1"		nextPutAll: self classOop; tab;
"2"		nextPutAll: self className; tab;
"3"		nextPutAll: self methodOop; tab;
"4"		nextPutAll: (self selector ifNil:[String new]); tab;
"5"		nextPutAll: self category; tab;
"6"		nextPutAll: self isReadOnly; tab;
		lf.

	
	"Method source"
	writeStream nextPutAll: self source.
	self source last = Character lf ifFalse: [writeStream lf].
	writeStream nextPut: $%; lf.	"Lines 2-N"

	"unimplemented selectors"			"https://github.com/jgfoster/Jade/issues/117"
	self firstSendOffsets keysAndValuesDo:[:offset :sel |
		writeStream 
			nextPutAll: offset printString;
			space;
			nextPutAll: sel;
			tab].
	writeStream lf.

	self stepPoints keysAndValuesDo:[:key :value |
		writeStream 
			nextPutAll: key
			space;
			nextPutAll: value;
			tab].
	writeStream lf.
	
	self breakPoints do:[:bp | 
		writeStream nextPutAll: bp printString
		tab].
	writeStream lf.

	"original method"
	self originalMethodSource notNil ifTrue:[
		writeStream nextPutAll: self originalMethodSource.
		(self originalMethodSource notEmpty and: [self originalMethodSource last = Character lf]) ifFalse: [writeStream lf]].
	writeStream nextPut: $%; lf.

	"compilation warnings"
	writeStream nextPutAll: self compilationWarnings; nextPut: $%; lf.

]
