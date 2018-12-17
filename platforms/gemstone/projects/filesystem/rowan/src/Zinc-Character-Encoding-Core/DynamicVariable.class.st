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
	"
	The following is the Pharo code 

	| activeProcess oldValue |
	activeProcess := Processor activeProcess.
	""oldValue will be nil if the variable has not yet been set""
	oldValue := activeProcess psValueAt: index.
	^ [ activeProcess
			psValueAt: index
			put: anObject.
		aBlock value ] ensure: [ 
			activeProcess
				psValueAt: index
				put: oldValue ]"

	index := anObject.
	aBlock value
]
