Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwRowanSampleAbstractTest',
	#category : 'Rowan-Tests-35x'
}

{ #category : 'tests' }
RwRowanSample9Test >> testCreateProject [
	| projectName projectDefinition projectLoadSpecification |
	projectName := 'RowanSample9'.
	Rowan
		projectNamed: projectName
		ifPresent: [ :prj | Rowan image _removeLoadedProject: prj _loadedProject ].
	Rowan
		projectNamed: projectName
		ifPresent: [ :prj | self error: 'The project ' , projectName printString , ' should not be loaded' ].
	projectDefinition := RwProjectDefinitionV2
		projectName: projectName
		comment: 'Example project exposing the standard project creation api'.
	projectLoadSpecification := projectDefinition projectLoadSpecification.
""
	{(STON toStringPretty: projectDefinition).
	(STON toStringPretty: projectLoadSpecification)}
]
