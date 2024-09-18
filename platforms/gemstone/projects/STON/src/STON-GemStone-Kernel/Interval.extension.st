Extension { #name : 'Interval' }

{ #category : '*ston-gemstone-kernel' }
Interval class >> fromSton: stonReader [
	| result values |
	values := OrderedCollection new.
	stonReader parseMapDo: [:key :value | values add: value].
	values size = 2 ifTrue: [result := self from: values first to: values second].
	values size = 3 ifTrue: [result := self from: values first to: values second by: values third].
	^result
]

{ #category : '*ston-gemstone-kernel' }
Interval >> stonOn: stonWriter [
  stonWriter
    writeObject: self
    streamMap: [ :dictionary | 
      dictionary
        at: #'start' put: from;
        at: #'stop' put: to;
        at: #'step' put: by ]
]
