Class {
	#name : 'RwToolTest',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwToolTest >> gsInteractionInformFailureHandler [

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'unexpected interaction' ];
		informBlock: [ :interaction | self assert: false description: 'unexpected inform' ];
		yourself
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
