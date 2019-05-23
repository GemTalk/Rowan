private
gsInteractionInformHandler: interactionBlock

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'expected an inform' ];
		informBlock: interactionBlock;
		yourself