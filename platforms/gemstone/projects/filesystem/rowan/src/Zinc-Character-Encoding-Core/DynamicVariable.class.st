Class {
	#name : 'DynamicVariable',
	#superclass : 'ProcessSpecificVariable',
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : 'accessing' }
DynamicVariable class >> value: anObject during: aBlock [

	^ self soleInstance value: anObject during: aBlock
]

{ #category : 'accessing' }
DynamicVariable >> value: anObject during: aBlock [

"	The following is the Pharo code "

	| oldValue |
	"oldValue will be nil if the variable has not yet been set"
	oldValue := self psValueAt: index.
	^ [ self
			psValueAt: index
			put: anObject.
		aBlock value ] ensure: [ 
			self
				psValueAt: index
				put: oldValue ]
]
