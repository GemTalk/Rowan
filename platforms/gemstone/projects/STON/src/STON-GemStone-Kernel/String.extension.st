Extension { #name : 'String' }

{ #category : '*ston-gemstone-kernel' }
String >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-gemstone-kernel' }
String >> stonOn: stonWriter [
	stonWriter writeString: self

]
