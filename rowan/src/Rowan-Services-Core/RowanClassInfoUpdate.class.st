Class {
	#name : 'RowanClassInfoUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'oop',
		'template',
		'category',
		'comment'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanClassInfoUpdate >> category: aString [

	category := aString

]

{ #category : 'accessing' }
RowanClassInfoUpdate >> comment: aString [

	comment := aString

]

{ #category : 'accessing' }
RowanClassInfoUpdate >> oop: aNumber [

	oop := aNumber

]

{ #category : 'accessing' }
RowanClassInfoUpdate >> template [ 

	^template

]

{ #category : 'accessing' }
RowanClassInfoUpdate >> template: anObject [

	template := anObject

]

{ #category : 'jade' }
RowanClassInfoUpdate >> writeInfoOn: writeStream [

	writeStream
		nextPutAll: oop printString; 
		lf;
		nextPut: $(;
		lf;
		nextPutAll: template;
		nextPut: $);
		lf;
		nextPutAll: String new.
	category ifNotNil: [
		writeStream
			nextPutAll: category;
			lf].
	writeStream
		nextPut: $%;
		lf.
	comment ifNotNil: [
		writeStream
			nextPutAll: comment;
			lf].
	writeStream
		nextPut: $%;
		lf.
]
