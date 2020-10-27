Extension { #name : 'Class' }

{ #category : '*ston-gemstone-kernel' }
Class >> stonName [
	"Override to encode my instances using a different class name."
	
	^ self name

]
