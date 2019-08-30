Extension { #name : 'Interval' }

{ #category : '*Rowan-GemStone-Kernel' }
Interval class >> fromSton: stonReader [
	| result values |
	values := OrderedCollection new.
	stonReader parseMapDo: [:key :value | values add: value].
	values size = 2 ifTrue: [result := self from: values first to: values second].
	values size = 3 ifTrue: [result := self from: values first to: values second by: values third].
	^Interval from: 0 to: 0
]
