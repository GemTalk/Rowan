"
My subclasses are dynamic variables: each subclass represents a variable
whose value persists inside the block passed to #value:during:. There is
no way to change the value inside such a block, but it is possible to
temporarirly rebind it in a nested manner.
"
Class {
	#name : 'ZnMaximumNumberOfDictionaryEntries',
	#superclass : 'DynamicVariable',
	#category : 'Zinc-Resource-Meta-Core'
}

{ #category : 'accessing' }
ZnMaximumNumberOfDictionaryEntries >> default [
	"The default maximum number dictionary entries to accept."
	
	^ 256
]
