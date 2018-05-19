Class {
	#name : 'RowanClassesUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'classes',
		'selections'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanClassesUpdate >> classes [

	^classes

]

{ #category : 'private' }
RowanClassesUpdate >> historyOf: aClass [

	| history |
	(history := aClass classHistory) isNil ifTrue: [
		history := Array with: aClass.
	].
	^history

]

{ #category : 'initialization' }
RowanClassesUpdate >> initialize [

	classes := Array new.

]

{ #category : 'jade' }
RowanClassesUpdate >> writeClassesTo: writeStream [

	classes do:[:aClass | 
	writeStream nextPutAll: aClass name.
		1 < (self historyOf: aClass) size ifTrue: [
			writeStream nextPutAll: ' ('.
			((self historyOf: aClass) indexOf: aClass) printOn: writeStream.
			writeStream nextPut: $/.
			(self historyOf: aClass) size printOn: writeStream.
			writeStream nextPut: $)]
	].
	writeStream tab.
	writeStream lf.

]
