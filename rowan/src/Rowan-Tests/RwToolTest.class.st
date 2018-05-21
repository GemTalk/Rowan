Class {
	#name : 'RwToolTest',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwToolTest >> gsInteractionConfirmationHandler [

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'expected a confirmation' ];
		confirmBlock: [ :interaction | interaction ok ];
		yourself
]

{ #category : 'private' }
RwToolTest >> gsInteractionInformFailureHandler [

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'unexpected interaction' ];
		informBlock: [ :interaction | self assert: false description: 'unexpected inform' ];
		yourself
]

{ #category : 'private' }
RwToolTest >> handleConfirmationDuring: aBlock [

	"expect a confirmation"

	aBlock
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionConfirmationHandler) ]
]

{ #category : 'private' }
RwToolTest >> handleInformAsFailureDuring: aBlock [

	aBlock
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionInformFailureHandler) ]
]
