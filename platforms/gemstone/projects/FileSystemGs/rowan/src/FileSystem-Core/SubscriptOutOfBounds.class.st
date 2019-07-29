Class {
	#name : 'SubscriptOutOfBounds',
	#superclass : 'FileSystemError',
	#instVars : [
		'signaler',
		'subscript',
		'lowerBound',
		'upperBound'
	],
	#category : 'FileSystem-Core-Kernel-Errors'
}

{ #category : 'signalling' }
SubscriptOutOfBounds class >> signalFor: subscript lowerBound: lowerBound upperBound: upperBound [
	^ self 
		signalFor: subscript 
		lowerBound: lowerBound 
		upperBound: upperBound 
		in: nil
]

{ #category : 'signalling' }
SubscriptOutOfBounds class >> signalFor: subscript lowerBound: lowerBound upperBound: upperBound in: object [
	^ self new
		signaler: object;
		subscript: subscript;
		lowerBound: lowerBound;
		upperBound: upperBound;
		signal
]

{ #category : 'signalling' }
SubscriptOutOfBounds class >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		self subscript 
			ifNil: [
				stream << 'subscript' ]
			ifNotNil: [ 
				stream print: self subscript ].
		(self lowerBound notNil and: [ self upperBound notNil])
			ifTrue: [
				stream << ' is not between '.
				stream print: self lowerBound.
				stream << ' and '.
				stream print: self upperBound ] ]
]

{ #category : 'accessing' }
SubscriptOutOfBounds >> lowerBound [
	^ lowerBound
]

{ #category : 'accessing' }
SubscriptOutOfBounds >> lowerBound: anObject [
	lowerBound := anObject
]

{ #category : 'accessing' }
SubscriptOutOfBounds >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"
	
	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : 'accessing' }
SubscriptOutOfBounds >> signaler [
	^ signaler
]

{ #category : 'accessing' }
SubscriptOutOfBounds >> signaler: anObject [
	signaler := anObject
]

{ #category : 'printing' }
SubscriptOutOfBounds >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		self subscript 
			ifNil: [
				stream << 'subscript' ]
			ifNotNil: [ 
				stream print: self subscript ].
		(self lowerBound notNil and: [ self upperBound notNil])
			ifTrue: [
				stream << ' is not between '.
				stream print: self lowerBound.
				stream << ' and '.
				stream print: self upperBound ] ]
]

{ #category : 'accessing' }
SubscriptOutOfBounds >> subscript [
	^ subscript
]

{ #category : 'accessing' }
SubscriptOutOfBounds >> subscript: anObject [
	subscript := anObject
]

{ #category : 'accessing' }
SubscriptOutOfBounds >> upperBound [
	^ upperBound
]

{ #category : 'accessing' }
SubscriptOutOfBounds >> upperBound: anObject [
	upperBound := anObject
]
