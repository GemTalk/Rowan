Class {
	#name : 'JadeServer64bit24',
	#superclass : 'JadeServer64bit',
	#category : 'Rowan-JadeServer'
}

{ #category : 'category' }
JadeServer64bit24 >> inspect: anObject [

	^(self isClientForwarder: anObject)
		ifTrue: [self inspectClientForwarder: anObject]
		ifFalse: [super inspect: anObject].


]

{ #category : 'category' }
JadeServer64bit24 >> inspectClientForwarder: anObject [

	| stream |
	(stream := WriteStream on: String new)
		nextPutAll: 'ClientForwarder'; tab;
		yourself.
	(self oopOf: anObject) printOn: stream.
	stream lf;
		nextPut: $1; lf;
		nextPutAll: 'clientObject'; tab;
		yourself.
	self print: (self oopOf: anObject clientObject) on: stream.
	stream lf; nextPutAll: (self printStringOf: anObject).
	^stream contents.


]

{ #category : 'category' }
JadeServer64bit24 >> isClientForwarder: anObject [

	^(Reflection classOf: anObject) name == #'ClientForwarder' 


]

{ #category : 'category' }
JadeServer64bit24 >> oopOf: anObject [

	^Reflection oopOf: anObject.


]

{ #category : 'category' }
JadeServer64bit24 >> printStringOf: anObject [

	^(self isClientForwarder: anObject)
		ifFalse: [anObject printString]
		ifTrue: ['aClientForwarder(' , anObject clientObject printString , ')'].


]

{ #category : 'category' }
JadeServer64bit24 >> registerOBNotificationsForPlatform: platform clientForwarder: clientForwarder [

	super
		registerOBNotificationsForPlatform: platform 
		clientForwarder: clientForwarder.
	platform 
		registerMultipleChoiceClientForwarder: clientForwarder;
		yourself.


]
