Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-Tests-35x'
}

{ #category : 'tests' }
RwRowanSample9Test >> testCreateNewProject [
	| projectName projectDefinition project |
	projectName := 'RowanSample9'.
	projectDefinition := self _canonicalRowanSample9ProjectFor: projectName.
{ projectDefinition. (STON toStringPretty: projectDefinition) }halt.
	projectDefinition load.
	project := Rowan projectNamed: projectName.
	self assert: project audit isEmpty
]
