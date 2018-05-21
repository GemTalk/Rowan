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

{ #category : 'other' }
RowanClassInfoUpdate >> initialize [

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
RowanClassInfoUpdate >> writeClassInfoOn: writeStream [

	writeStream
		nextPutAll: oop printString;
		lf;
		nextPutAll: template;
		nextPut: $%;
		nextPutAll: (category ifNil:[String new]);
		lf;
		nextPutAll: (comment ifNil:[String new]);
		nextPut: $%;
		lf

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
		nextPutAll: (category ifNil:[String new]);
		nextPut: $%;
		lf;
		nextPutAll: (comment ifNil:[String new]);
		lf; 
		nextPut: $%;
		lf.

]
