Class {
	#name : 'RwActiveComponentTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests-ComponentsV2'
}

{ #category : 'tests' }
RwActiveComponentTest >> testIssue789 [
	"https://github.com/GemTalk/Rowan/issues/789"

	| projectName componentName component packageName componentCopy |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	{
		{ RwLoadComponent } .
		{ RwSubcomponent .  'common' } .
		{ RwPlatformSubcomponent . {'common'} }.
		{ RwPackageGroup } .
	} 
		do: [:ar |
			| componentClass condition |
			componentClass := ar at: 1.
			condition := ar atOrNil: 2.
			component := componentClass newNamed: componentName.
			condition ifNotNil: [ component condition: condition ].
			self assert: component validate.
			packageName := projectName , '-Core'.
			component
				addPackageNames: {packageName}.
			component class ~~ RwPackageGroup
				ifTrue: [ 
					component
						conditionalPackageMapSpecsAtGemStoneUserId: 'DataCurator'
							andPackageName: packageName
							setSymbolDictNameTo: 'UserGlobals';
						conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
							andPackageName: packageName
							setSymbolDictNameTo: 'UserGlobals';
						conditionalPackageMapSpecsAtGemStoneUserId: 'DataCurator' 
							andPackageName: packageName 
							setUseSessionMethodsForExtensions: true;
						conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser' 
							andPackageName: packageName 
							setUseSessionMethodsForExtensions: false;
						yourself ].

			componentCopy := component copy.
			self assert: component ~~ componentCopy.

			component class ~~ RwPackageGroup
				ifTrue: [
					self assert: component conditionalPackageMapSpecs equals: componentCopy conditionalPackageMapSpecs.
					self assert: (component conditionalPackageMapSpecs at: 'gemstone') ~~ (componentCopy conditionalPackageMapSpecs at: 'gemstone').

					#('DataCurator' 'SystemUser') do: [:userId |
						" Issue #789 - ensure that the copy is deep enough"
						self 
							assert: 
								((component conditionalPackageMapSpecs at: 'gemstone') at: 'DataCurator') ~~ 
								((componentCopy conditionalPackageMapSpecs at: 'gemstone') at: 'DataCurator').
						self 
							assert: 
								(((component conditionalPackageMapSpecs at: 'gemstone') at: userId) 
									at: #packageNameToPlatformPropertiesMap) ~~ 
								(((componentCopy conditionalPackageMapSpecs at: 'gemstone') at: userId)
									at: #packageNameToPlatformPropertiesMap).
						self 
							assert: 
								((((component conditionalPackageMapSpecs at: 'gemstone') at: userId) 
									at: #packageNameToPlatformPropertiesMap) at: packageName) ~~ 
								((((componentCopy conditionalPackageMapSpecs at: 'gemstone') at: userId)
									at: #packageNameToPlatformPropertiesMap) at: packageName) ] ].

			self assert: component = componentCopy.
			self assert: component ~~ componentCopy ].
]
