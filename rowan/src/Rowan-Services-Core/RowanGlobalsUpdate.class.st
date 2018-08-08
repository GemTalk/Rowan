Class {
	#name : 'RowanGlobalsUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'globals'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanGlobalsUpdate >> globals [

	^globals

]

{ #category : 'initialization' }
RowanGlobalsUpdate >> initialize [

	globals := Array new.

]

{ #category : 'jade' }
RowanGlobalsUpdate >> writeGlobalsTo: writeStream [
	"globals is an array of 5 elements"
	
	globals do: [:array | 
		1 to: 4 do:[:idx |
			writeStream nextPutAll: (array at: idx); tab].
		writeStream nextPutAll: (array at: 5); lf].
	writeStream nextPut: $%; lf.

]
