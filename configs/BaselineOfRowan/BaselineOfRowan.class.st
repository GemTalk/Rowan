Class {
	#name : 'BaselineOfRowan',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfRowan'
}

{ #category : 'baseline' }
BaselineOfRowan >> baseline: spec [

	<baseline>
	spec
		for: #'common'
		do: [ 
			spec
				package: 'Cypress-Core';
				package: 'Rowan-Url' with: [ spec requires: 'Cypress-Core' ];
				package: 'Rowan-Core' with: [ spec requires: 'Rowan-Url' ];
				package: 'Rowan-Definitions' with: [ spec requires: 'Rowan-Core' ];
				package: 'Rowan-Cypress' with: [ spec requires: 'Rowan-Definitions' ];
				package: 'Rowan-Tools' with: [ spec requires: 'Rowan-Cypress' ];
				package: 'Rowan-Tode' with: [ spec requires: 'Rowan-Tools' ];
				package: 'Rowan-Tests' with: [ spec requires: 'Rowan-Tode' ];
				package: 'Rowan-Services' with: [ spec requires: 'Rowan-Tools' ];
				yourself ].
	spec
		for: #'gemstone'
		do: [ 
			spec
				package: 'Rowan-GemStone' with: [ spec requires: 'Rowan-Definitions' ];
				yourself ].
	spec
		for: #'gs3.2.15x'
		do: [ 
			spec
				package: 'Rowan-3215' with: [ spec requires: 'Rowan-Url' ];
				yourself ].
]
