"
This is an abstract class which defines the file modes.
A file mode class is created for each type of mode. These
are subclasses of this class.

The instance returns the mode string to use when opening a file.

Also it knows if the file is read only.
"
Class {
	#name : 'FileMode',
	#superclass : 'Object',
	#category : 'FileSystem-Core-Kernel'
}

{ #category : 'testing' }
FileMode >> isWritable [
	self subclassResponsibility
]

{ #category : 'accessing' }
FileMode >> mode [
	"Return a file's mode string"

	self subclassResponsibility
]

{ #category : 'printing' }
FileMode >> printOn: aStream [

	super printOn: aStream.
	aStream 
		nextPutAll: '(';
		nextPutAll: ( self class name == #FileMode ifTrue: ['abstract class'] ifFalse: [ self mode] );
		nextPutAll: ')'
]
