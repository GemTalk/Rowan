Class {
	#name : 'NotFound',
	#superclass : 'LookupError',
	#instVars : [
		'signaler'
	],
	#category : 'Zinc-Resource-Meta-Core'
}

{ #category : 'instance creation' }
NotFound class >> signalFor: anObject [
	"Create and signal a NotFound exception for anObject in the default receiver."
	
	^ self new
		object: anObject;
		signal
]

{ #category : 'instance creation' }
NotFound class >> signalFor: anObject in: aCollection [
	"Create and signal a NotFound exception for anObject in aCollection."

	^ self new
		object: anObject;
		collection: aCollection;
		signal
]

{ #category : 'accessing' }
NotFound >> collection [
	"Return the collection where something is not found in"
	
	^ self signaler
]

{ #category : 'accessing' }
NotFound >> collection: aCollection [
	"Set the collection where something is not found in"
	
	self signaler: aCollection
]

{ #category : 'accessing' }
NotFound >> object [
	"Return the object that was not found"
	
	^ key
]

{ #category : 'accessing' }
NotFound >> signaler [
	"Return the object that is the subject involving me.
	This is set automatically to my #receiver during #signal
	but could be overwritten when I am signaled"

	^ signaler
]

{ #category : 'accessing' }
NotFound >> signaler: anObject [
	"Set the object that is the subject involving me.
	This is set automatically to my #receiver during #signal
	but could be overwritten when I am signaled"

	signaler := anObject
]

{ #category : 'private' }
NotFound >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		stream print: self object.
		stream << ' not found in '.
		stream print: self collection class ]
]
