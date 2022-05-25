Class {
	#name : 'RwSimpleProjectLoadComponentV2Test',
	#superclass : 'RwAbstractLoadComponentV2Test',
	#category : 'Rowan-Tests-ComponentsV2'
}

{ #category : 'private' }
RwSimpleProjectLoadComponentV2Test >> _testComponentClass: componentClass projectName: projectName componentName: componentName projectRef: projectRef componentRef: componentRef projectRef: projectRef1 [
	"excercise basic functionality"

	| component packageName1 packageName2 stonString |
	component := componentClass newNamed: componentName for: projectName.

	self assert: component validate.

	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Extension'.
	component
		condition: 'common';
		addPackageNames: {packageName1};
		addComponentNames: {componentRef};
		addProjectNamed: projectRef;
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName1
			setSymbolDictNameTo: 'UserGlobals'.

	stonString := STON toStringPretty: component.	"useful in case of error"
	self assert: component validate.
	self
		_validateComponent: component
			condition: 'common'
			componentNames: {componentRef}
			packageNames: {packageName1}
			projectNames: {projectRef};
		yourself.

	component
		removePackageNamed: packageName1;
		removeProjectNamed: projectRef1;
		removeComponentNamed: componentRef;
		yourself.

	stonString := STON toStringPretty: component.	"useful in case of error"
	self assert: component validate.

	self
		_validateComponent: component
			condition: 'common'
			componentNames: {}
			packageNames: {}
			projectNames: {projectRef};
		yourself
]

{ #category : 'private' }
RwSimpleProjectLoadComponentV2Test >> _validateComponent: component condition: condition componentNames: componentNames packageNames: packageNames projectNames: projectNames [
	| x |
	self assert: (x := component condition) = condition.
	self assert: (x := component componentNames) asArray = componentNames.
	self assert: (x := component packageNames) asArray = packageNames.
	self assert: (x := component projectNames) asArray = projectNames
]

{ #category : 'tests' }
RwSimpleProjectLoadComponentV2Test >> testBasic [
	"excercise basic functionality for RwProjectLoadComponentV2"

	self
		_testComponentClass: RwSimpleProjectLoadComponentV2
		projectName: 'RowanSample9'
		componentName: 'Core'
		projectRef: 'Project'
		componentRef: 'Nested'
		projectRef: 'Project1'
]

{ #category : 'tests' }
RwSimpleProjectLoadComponentV2Test >> testBasic_nested [
	"excercise basic functionality for RwSimpleNestedProjectLoadComponentV2"

	self
		_testComponentClass: RwSimpleNestedProjectLoadComponentV2
		projectName: 'RowanSample9'
		componentName: 'Core'
		projectRef: 'Project'
		componentRef: 'Nested'
		projectRef: 'Project1'
]

{ #category : 'tests' }
RwSimpleProjectLoadComponentV2Test >> testIssue789 [
	"https://github.com/GemTalk/Rowan/issues/789"

	| componentName projectName component packageName componentCopy |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	{
		{ RwSimpleProjectLoadComponentV2 .  'common' } .
		{ RwSimpleNestedProjectLoadComponentV2 .  'common' } .
		{ RwPlatformNestedProjectLoadComponentV2 . {'common'} }.
	} 
		do: [:ar |
			| componentClass condition |
			componentClass := ar at: 1.
			condition := ar at: 2.
			component := (componentClass
				newNamed: componentName
				for: projectName) condition: condition.
			self assert: component validate.
			packageName := projectName , '-Core'.
			component
				addPackageNames: {packageName};
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
				yourself.

			componentCopy := component copy.
			self assert: component ~~ componentCopy.

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
							at: #packageNameToPlatformPropertiesMap) at: packageName) ].

			self assert: component = componentCopy.
			self assert: component ~~ componentCopy ].
]

{ #category : 'tests' }
RwSimpleProjectLoadComponentV2Test >> testUknownPlatform [
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := (RwSimpleProjectLoadComponentV2
		newNamed: componentName
		for: projectName) condition: 'common'.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	component conditionalPackageMapSpecs at: 'boom' put: Dictionary new.

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: Unknown platform name ''boom'' in conditional package map specs'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwSimpleProjectLoadComponentV2Test >> testUknownPlatformPropertiesKey [
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := (RwSimpleProjectLoadComponentV2
		newNamed: componentName
		for: projectName) condition: 'common'.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	((component conditionalPackageMapSpecs at: 'gemstone') at: 'SystemUser')
		at: #'boom'
		put: Dictionary new.

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description) = 'Error: Unknown platformPropertiesMap key #''boom'''.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwSimpleProjectLoadComponentV2Test >> testUndefinedPackageName [
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := (RwSimpleProjectLoadComponentV2
		newNamed: componentName
		for: projectName) condition: 'common'.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	(((component conditionalPackageMapSpecs at: 'gemstone') at: 'SystemUser')
		at: #'packageNameToPlatformPropertiesMap') at: 'boom' put: Dictionary new.

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: Undefined package name ''boom'' used in plaform properties map'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwSimpleProjectLoadComponentV2Test >> testUnknownPackagePropertName [
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := (RwSimpleProjectLoadComponentV2
		newNamed: componentName
		for: projectName) condition: 'common'.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	(((component conditionalPackageMapSpecs at: 'gemstone') at: 'SystemUser')
		at: #'packageNameToPlatformPropertiesMap')
		at: packageName
		put:
			(Dictionary new
				at: #'boom' put: 'boom';
				yourself).

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description) = 'Error: Unknown package property name #''boom'''.
			hitError := true ].
	self assert: hitError
]
