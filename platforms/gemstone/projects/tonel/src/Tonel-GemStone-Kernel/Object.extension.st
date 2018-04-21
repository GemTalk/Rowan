Extension { #name : 'Object' }

{ #category : '*tonel-gemstone-kernel' }
Object >> isCollection [

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ false

]

{ #category : '*tonel-gemstone-kernel' }
Object >> putOn: aStream [

	^ aStream nextPut: self

]
