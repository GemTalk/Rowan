Class {
	#name : 'RowanSuperclassUpdate',
	#superclass : 'RowanClassesUpdate',
	#instVars : [
		'tabName'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanSuperclassUpdate >> tabName [

	^tabName

]

{ #category : 'accessing' }
RowanSuperclassUpdate >> tabName: aString [

	tabName := aString

]

{ #category : 'jade' }
RowanSuperclassUpdate >> writeTabNameTo: writeStream [

	writeStream nextPutAll: tabName; lf.

]
