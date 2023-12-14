Extension { #name : 'RwGsImage' }

{ #category : '*rowan-tests-shared-gemstone' }
RwGsImage class >> testImageClass [

	"answer an alternate image class (presumably a subclass of the receiver) that can be used as the image class for the duration of tests"

	^ RwGsTestImage
]
