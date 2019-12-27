Class {
	#name : 'RwProjectComponentVisitorV2Test',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests-DiskConfigurationsV2'
}

{ #category : 'private' }
RwProjectComponentVisitorV2Test >> _visitorClass [
	^ RwResolvedProjectComponentVisitorV2
]

{ #category : 'tests' }
RwProjectComponentVisitorV2Test >> testBasicVisit_independent [
	"test of RwProjectLoadComponentVisitorV2 as it would be used without a RwResolvedProject."

	| platformAttributes groupNames visitor componentNamesToLoad projectAlias projectPath projectSpecUrl projectSpec |
	platformAttributes := {'common'.
	'gemstone'.
	('3.5.0' asRwGemStoneVersionNumber)}.
	projectAlias := 'RowanSample9_DiskConfig_Test'.
	componentNamesToLoad := #('Core').
	groupNames := #('core').

	projectPath := self _cloneRowanSample9: projectAlias.	"clone https://github.com/dalehenrich/RowanSample9"

	visitor := RwIndependentComponentVisitorV2 new
		platformAttributes: platformAttributes;
		groupNames: groupNames;
		yourself.

	self assert: visitor packageNames isEmpty.
	projectSpecUrl := 'file:' , projectPath , '/rowan/project.ston'.
	projectSpec := RwSpecification fromUrl: projectSpecUrl.

	componentNamesToLoad
		do: [ :componentName | 
			| component url |
			url := 'file:' , projectPath , '/' , projectSpec componentsPath , '/'
				, componentName , '.ston'.
			component := RwAbstractProjectConfiguration fromUrl: url.
			component projectName: projectAlias.

			visitor visit: component ].
	self
		assert: visitor packageNames sort
		equals:
			#('RowanSample9-Core' 'RowanSample9-Extensions' 'RowanSample9-GemStone') sort.
	self assert: visitor projectLoadSpecs isEmpty
]
