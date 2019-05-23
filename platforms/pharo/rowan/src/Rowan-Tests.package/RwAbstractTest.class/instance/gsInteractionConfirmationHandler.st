private
gsInteractionConfirmationHandler

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'expected a confirmation' ];
		confirmBlock: [ :interaction | interaction ok ];
		yourself