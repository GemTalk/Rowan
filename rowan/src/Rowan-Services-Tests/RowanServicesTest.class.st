Class {
	#name : 'RowanServicesTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
RowanServicesTest >> setUp [

	super setUp
]

{ #category : 'support' }
RowanServicesTest >> tearDown [

	RowanCommandResult initializeResults.  "squash the gui updates"
]
