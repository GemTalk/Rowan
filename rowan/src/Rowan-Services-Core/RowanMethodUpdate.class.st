Class {
	#name : 'RowanMethodUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'classOop',
		'className',
		'methodOop',
		'selector',
		'category',
		'editable',
		'source',
		'firstSendOffsets',
		'stepPoints',
		'breakPoints',
		'originalMethodSource',
		'method',
		'compilationWarnings'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanMethodUpdate >> breakPoints: anObject [

	breakPoints := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> category: anObject [

	category := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> className: anObject [

	className := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> classOop: anObject [

	classOop := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> compilationWarnings: aString [

	compilationWarnings := aString

]

{ #category : 'accessing' }
RowanMethodUpdate >> editable: anObject [

	editable := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> firstSendOffsets [

	^firstSendOffsets

]

{ #category : 'initialization' }
RowanMethodUpdate >> initialize [

	firstSendOffsets := Dictionary new.
	stepPoints := Dictionary new.

]

{ #category : 'accessing' }
RowanMethodUpdate >> method: anObject [

	method := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> methodOop: anObject [

	methodOop := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> originalMethodSource: anObject [

	 originalMethodSource := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> selector: anObject [

	selector := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> source: anObject [

	source := anObject

]

{ #category : 'accessing' }
RowanMethodUpdate >> stepPoints [

	^stepPoints

]

{ #category : 'jade' }
RowanMethodUpdate >> writeMethodTo: writeStream [
		
		writeStream 	"Line 1 for GsMethod (line 3 for JadeSystemBrowserPresenter)"
"1"		nextPutAll: classOop; tab;
"2"		nextPutAll: className; tab;
"3"		nextPutAll: methodOop; tab;
"4"		nextPutAll: (selector ifNil:[String new]); tab;
"5"		nextPutAll: category; tab;
"6"		nextPutAll: editable; tab;
		lf.

	
	"Method source"
	writeStream nextPutAll: source.
	source last = Character lf ifFalse: [writeStream lf].
	writeStream nextPut: $%; lf.	"Lines 2-N"

	"unimplemented selectors"			"https://github.com/jgfoster/Jade/issues/117"
	firstSendOffsets keysAndValuesDo:[:offset :sel |
		writeStream 
			nextPutAll: offset printString;
			space;
			nextPutAll: sel;
			tab].
	writeStream lf.

	stepPoints keysAndValuesDo:[:key :value |
		writeStream 
			nextPutAll: key
			space;
			nextPutAll: value;
			tab].
	writeStream lf.
	
	breakPoints do:[:bp | 
		writeStream nextPutAll: bp printString
		tab].
	writeStream lf.

	"original method"
	originalMethodSource notNil ifTrue:[
		writeStream nextPutAll: originalMethodSource.
		(originalMethodSource notEmpty and: [originalMethodSource last = Character lf]) ifFalse: [writeStream lf]].
	writeStream nextPut: $%; lf.

	"compilation warnings"
	writeStream nextPutAll: compilationWarnings; nextPut: $%; lf.

]
