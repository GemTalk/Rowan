Class {
	#name : 'RowanServicesTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
RowanServicesTest >> setUp [

	System abortTransaction.
]

{ #category : 'support' }
RowanServicesTest >> tearDown [

	System abortTransaction.
	RowanCommandResult initializeResults.  "squash the gui updates"
]
