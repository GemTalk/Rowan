Class {
	#name : 'KeyNotFound',
	#superclass : 'NotFound',
	#category : 'Zinc-Resource-Meta-Core'
}

{ #category : 'private' }
KeyNotFound >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream << 'key '.
		stream print: self object.
		stream << ' not found in '.
		stream print: self collection class ]
]
