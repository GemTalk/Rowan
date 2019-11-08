Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-Tests-35x'
}

{ #category : 'tests' }
RwRowanSample9Test >> testExportNewProject [
	| projectName projectDefinition project |
	projectName := 'RowanSample9'.
	projectDefinition := self _canonicalRowanSample9ProjectFor: projectName.
	projectDefinition export.
	project := Rowan projectNamed: projectName.
	self assert: project audit isEmpty
]

{ #category : 'tests' }
RwRowanSample9Test >> testLoadNewProject [
	| projectName projectDefinition projects |
	projectName := 'RowanSample9'.
	projectDefinition := self _canonicalRowanSample9ProjectFor: projectName.
	projects := projectDefinition load.
	self assert: projects size = 1.
	projects do: [ :project | self assert: project audit isEmpty ]
]
