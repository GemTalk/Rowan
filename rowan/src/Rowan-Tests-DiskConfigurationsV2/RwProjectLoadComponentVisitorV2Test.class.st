Class {
	#name : 'RwProjectLoadComponentVisitorV2Test',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests-DiskConfigurationsV2'
}

{ #category : 'private' }
RwProjectLoadComponentVisitorV2Test >> _visitorClass [
	^ RwProjectLoadComponentVisitorV2
]

{ #category : 'tests' }
RwProjectLoadComponentVisitorV2Test >> testBasic [
	| platformAttributes groupNames componentDirectory projectsDirectory resolvedProject visitor componentNamesToLoad projectName |
	platformAttributes := {'common'.
	'gemstone'.
	('3.5.0' asRwGemStoneVersionNumber)}.

	visitor := self _visitorClass new
		platformAttributes: platformAttributes;
		groupNames: groupNames;
		componentsRoot: componentDirectory;
		projectsRoot: projectsDirectory;
		resolvedProject: resolvedProject;
		yourself.
	projectName := resolvedProject projectAlias.
	componentNamesToLoad
		do: [ :componentName | 
			| component url |
			url := 'file:' , (componentDirectory / componentName , 'ston') pathString.
			component := RwAbstractProjectConfiguration fromUrl: url.
			component projectName: projectName.

			visitor visit: component ]
]
