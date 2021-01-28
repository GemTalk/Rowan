Class {
	#name : 'RwEntitySetXXX',
	#superclass : 'RwEntitySet',
	#category : 'Rowan-Definitions'
}

{ #category : 'definitions' }
RwEntitySetXXX >> asProjectDefinitionSet [

	| result |
	result := RwProjectSetDefinitionXXX new.
	entities do: [ :entity | result addDefinition: entity ].
	^ result
]
