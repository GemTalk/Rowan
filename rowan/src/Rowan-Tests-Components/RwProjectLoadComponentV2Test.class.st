Class {
	#name : 'RwProjectLoadComponentV2Test',
	#superclass : 'RwAbstractLoadComponentV2Test',
	#category : 'Rowan-Tests-Components'
}

{ #category : 'private' }
RwProjectLoadComponentV2Test >> _testComponentClass: componentClass projectName: projectName componentName: componentName projectRef: projectRef componentRef: componentRef projectRef: projectRef1 [
	"excercise basic functionality"

	| component packageName1 packageName2 stonString conditionalProperties |
	component := componentClass newNamed: componentName for: projectName.

	self assert: component validate.

	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Extension'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addPackageNames: {packageName1};
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addComponentNames: {componentRef};
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addProjectNames: {projectRef};
		conditionalPropertiesAtConditions: {'gemstone'}
			andGroup: 'core'
			addComponentNames: {componentRef};
		conditionalPropertiesAtConditions: {'gemstone'}
			andGroup: 'core'
			addPackageNames: {packageName2};
		conditionalPropertiesAtConditions: {'vast'}
			andGroup: 'core'
			addProjectNames: {projectRef1};
		conditionalPropertiesAtConditions: {'vast'}
			andGroup: 'core'
			addPackageNames: {packageName2};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName1
			setSymbolDictNameTo: 'UserGlobals'.


	stonString := STON toStringPretty: component.	"useful in case of error"
	self assert: component validate.
	self assert: component definedGroupNames keys asArray sort = {'core'}.
	conditionalProperties := component copy initializeForExport
		conditionalProperties.	"sorted in canonical key order"
	self
		assert:
			conditionalProperties keys
				=
					{{'common'}.
					{'gemstone'}.
					{'vast'}}.

	self
		_validateComponent: component
			condition: {'common'}
			group: 'core'
			componentNames: {componentRef}
			packageNames: {packageName1}
			projectNames: {projectRef};
		_validateComponent: component
			condition: {'gemstone'}
			group: 'core'
			componentNames: {componentRef}
			packageNames: {packageName2}
			projectNames: {};
		_validateComponent: component
			condition: {'vast'}
			group: 'core'
			componentNames: {}
			packageNames: {packageName2}
			projectNames: {projectRef1};
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
			condition: {'common'}
			group: 'core'
			componentNames: {}
			packageNames: {}
			projectNames: {projectRef};
		_validateComponent: component
			condition: {'gemstone'}
			group: 'core'
			componentNames: {}
			packageNames: {packageName2}
			projectNames: {};
		_validateComponent: component
			condition: {'vast'}
			group: 'core'
			componentNames: {}
			packageNames: {packageName2}
			projectNames: {};
		yourself
]

{ #category : 'private' }
RwProjectLoadComponentV2Test >> _validateComponent: component condition: conditionArray group: groupName componentNames: componentNames packageNames: packageNames projectNames: projectNames [
	| x |
	self
		assert:
			(x := component
				conditionalComponentsAtConditions: conditionArray
				andGroup: groupName) asArray = componentNames.
	self
		assert:
			(x := component
				conditionalPackagesAtConditions: conditionArray
				andGroup: groupName) asArray = packageNames.
	self
		assert:
			(x := component
				conditionalProjectsAtConditions: conditionArray
				andGroup: groupName) asArray = projectNames
]

{ #category : 'tests' }
RwProjectLoadComponentV2Test >> testBasic [
	"excercise basic functionality for RwProjectLoadComponentV2"

	self
		_testComponentClass: RwProjectLoadComponentV2
		projectName: 'RowanSample9'
		componentName: 'Core'
		projectRef: 'Project'
		componentRef: 'Nested'
		projectRef: 'Project1'
]

{ #category : 'tests' }
RwProjectLoadComponentV2Test >> testBasic_nested [
	"excercise basic functionality for RwNestedProjectLoadComponentV2"

	self
		_testComponentClass: RwNestedProjectLoadComponentV2
		projectName: 'RowanSample9'
		componentName: 'Core'
		projectRef: 'Project'
		componentRef: 'Nested'
		projectRef: 'Project1'
]

{ #category : 'tests' }
RwProjectLoadComponentV2Test >> testInvalidConditionalGroupName [
	"error coverage for invalid components"

	| componentName projectName component packageName stonString conditionalPackages hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.

	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	conditionalPackages := component conditionalProperties.
	(conditionalPackages at: #('common')) at: 'boom' put: Dictionary new.

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Conditional packages includes group name ''boom'' that is not a defined group'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectLoadComponentV2Test >> testInvalidGroupName [
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component defineGroupNamed: 'core' toIncludeGroups: #('boom').

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: The group ''boom'' is not a defined group'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectLoadComponentV2Test >> testUknownPlatform [
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
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
						=
							'Error: Unknown platform name ''boom'' in conditional package map specs'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectLoadComponentV2Test >> testUknownPlatformPropertiesKey [
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
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
					(x := ex description)
						= 'Error: Unknown platformPropertiesMap key #''boom'''.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectLoadComponentV2Test >> testUndefinedPackageName [
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
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
						=
							'Error: Undefined package name ''boom'' used in plaform properties map'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwProjectLoadComponentV2Test >> testUnknownPackagePropertName [
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
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
					(x := ex description)
						= 'Error: Unknown package property name #''boom'''.
			hitError := true ].
	self assert: hitError
]
