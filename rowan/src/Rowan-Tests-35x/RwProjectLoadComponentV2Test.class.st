Class {
	#name : 'RwProjectLoadComponentV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-Tests-35x'
}

{ #category : 'tests' }
RwProjectLoadComponentV2Test >> testBasic [
	"excercise basic functionality"

	| componentName projectName component packageName stonString |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.
	self assert: component validate.
	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPackagesAtConditions: {'common'}
			andGroup: 'core'
			addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	stonString := STON toStringPretty: component.	"useful in case of error"
	self assert: component validate
]
