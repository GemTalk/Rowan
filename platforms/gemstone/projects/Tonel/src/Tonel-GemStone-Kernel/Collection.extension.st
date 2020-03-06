Extension { #name : 'Collection' }

{ #category : '*tonel-gemstone-kernel' }
Collection >> ifNotEmpty: aBlock [

	^ self size == 0
		ifFalse: [ aBlock cull: self ]

]

{ #category : '*tonel-gemstone-kernel' }
Collection >> isCollection [

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ true

]

{ #category : '*tonel-gemstone-kernel' }
Collection >> select: selectBlock thenDo: doBlock [
  "Utility method to improve readability."

  ^ (self select: selectBlock) do: doBlock

]
