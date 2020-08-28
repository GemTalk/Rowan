Extension { #name : 'String' }

{ #category : '*ston-gemstone-core' }
String >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-gemstone-core' }
String >> stonOn: stonWriter [
	stonWriter writeString: self

]
