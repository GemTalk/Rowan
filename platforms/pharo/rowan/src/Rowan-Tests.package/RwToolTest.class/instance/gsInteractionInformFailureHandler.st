private
gsInteractionInformFailureHandler

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'unexpected interaction' ];
		informBlock: [ :interaction | self assert: false description: 'unexpected inform' ];
		yourself