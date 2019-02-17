Class {
	#name : 'ValueNotFound',
	#superclass : 'NotFound',
	#category : 'Zinc-Resource-Meta-Core'
}

{ #category : 'private' }
ValueNotFound >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream << 'value '.
		stream print: self object.
		stream << ' not found in '.
		stream print: self collection class]
]

{ #category : 'accessing' }
ValueNotFound >> value [
	^self object
]

{ #category : 'accessing' }
ValueNotFound >> value: anObject [
	self object: anObject
]
