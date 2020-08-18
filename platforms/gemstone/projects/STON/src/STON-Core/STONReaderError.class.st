"
STONReaderError is the error/exception signalled by STONReader when illegal/incorrect input is seen. 

"
Class {
	#name : 'STONReaderError',
	#superclass : 'Error',
	#instVars : [
		'streamPosition'
	],
	#category : 'STON-Core'
}

{ #category : 'instance creation' }
STONReaderError class >> signal: aString streamPosition: streamPosition [ 
	^ self new
		streamPosition: streamPosition;
		signal: aString;
		yourself

]

{ #category : 'accessing' }
STONReaderError >> buildMessageText [
	streamPosition ifNotNil: [ :pos | 
    self details: 'Error at character position ', pos asString 
  ].
  super buildMessageText .
]

{ #category : 'accessing' }
STONReaderError >> streamPosition [
	^ streamPosition

]

{ #category : 'accessing' }
STONReaderError >> streamPosition: aNumber [
	streamPosition := aNumber

]
