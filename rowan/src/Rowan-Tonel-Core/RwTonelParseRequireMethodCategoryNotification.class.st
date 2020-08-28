"
The receiver is signalled when the RwTonelParser is about to create a method definition with no method category defined.

If the caller wants to continue, #resume: the notification with the value of the category to be used.

If unhandled,  a RwTonelParseError is signalled.
"
Class {
	#name : 'RwTonelParseRequireMethodCategoryNotification',
	#superclass : 'Notification',
	#instVars : [
		'className',
		'isMeta',
		'selector'
	],
	#category : 'Rowan-Tonel-Core'
}

{ #category : 'instance creation' }
RwTonelParseRequireMethodCategoryNotification class >> className: className isMeta: isMeta selector: selector [
	^ self new
		className: className;
		isMeta: isMeta;
		selector: selector;
		yourself
]

{ #category : 'accessing' }
RwTonelParseRequireMethodCategoryNotification >> className [
	^className
]

{ #category : 'accessing' }
RwTonelParseRequireMethodCategoryNotification >> className: object [
	className := object
]

{ #category : 'Handling' }
RwTonelParseRequireMethodCategoryNotification >> defaultAction [
	"handle and resume: with the desired method category to be used to avoid error"

	RwTonelParseError
		signal:
			'Missing method category for ' , self className
				,
					(self isMeta
						ifTrue: [ ' class >> ' ]
						ifFalse: [ ' >> ' ]) , self selector
]

{ #category : 'accessing' }
RwTonelParseRequireMethodCategoryNotification >> isMeta [
	^isMeta
]

{ #category : 'accessing' }
RwTonelParseRequireMethodCategoryNotification >> isMeta: object [
	isMeta := object
]

{ #category : 'accessing' }
RwTonelParseRequireMethodCategoryNotification >> selector [
	^selector
]

{ #category : 'accessing' }
RwTonelParseRequireMethodCategoryNotification >> selector: object [
	selector := object
]
