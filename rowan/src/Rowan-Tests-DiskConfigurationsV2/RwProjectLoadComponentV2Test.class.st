Class {
	#name : 'RwProjectLoadComponentV2Test',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests-DiskConfigurationsV2'
}

{ #category : 'tests' }
RwProjectLoadComponentV2Test >> testBasic [
	"excercise basic functionality"

	| componentName projectName component packageName1 packageName2 stonString projectRef componentRef projectRef1 componentRef1 conditionalProperties x |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	projectRef := 'Project'.
	componentRef := 'Nested'.
	projectRef1 := 'Project1'.
	componentRef1 := 'Nested1'.

	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

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
		assert:
			(x := component conditionalComponentsAtConditions: {'common'} andGroup: 'core')
				asArray = {componentRef}.
	self
		assert:
			(x := component conditionalPackagesAtConditions: {'common'} andGroup: 'core')
				asArray = {packageName1}.
	self
		assert:
			(x := component conditionalProjectsAtConditions: {'common'} andGroup: 'core')
				asArray = {projectRef}.

	self
		assert:
			(x := component conditionalComponentsAtConditions: {'gemstone'} andGroup: 'core')
				asArray = {componentRef}.
	self
		assert:
			(x := component conditionalPackagesAtConditions: {'gemstone'} andGroup: 'core')
				asArray = {packageName2}.
	self
		assert:
			(x := component conditionalProjectsAtConditions: {'gemstone'} andGroup: 'core')
				asArray = {}.

	self
		assert:
			(x := component conditionalComponentsAtConditions: {'vast'} andGroup: 'core')
				asArray = {}.
	self
		assert:
			(x := component conditionalPackagesAtConditions: {'vast'} andGroup: 'core')
				asArray = {packageName2}.
	self
		assert:
			(x := component conditionalProjectsAtConditions: {'vast'} andGroup: 'core')
				asArray = {projectRef1}.

	component
		removePackageNamed: packageName1;
		removeProjectNamed: projectRef1;
		removeComponentNamed: componentRef;
		yourself.

	stonString := STON toStringPretty: component.	"useful in case of error"
	self assert: component validate.


	self
		assert:
			(x := component conditionalComponentsAtConditions: {'common'} andGroup: 'core')
				asArray = {}.
	self
		assert:
			(x := component conditionalPackagesAtConditions: {'common'} andGroup: 'core')
				asArray = {}.
	self
		assert:
			(x := component conditionalProjectsAtConditions: {'common'} andGroup: 'core')
				asArray = {projectRef}.

	self
		assert:
			(x := component conditionalComponentsAtConditions: {'gemstone'} andGroup: 'core')
				asArray = {}.
	self
		assert:
			(x := component conditionalPackagesAtConditions: {'gemstone'} andGroup: 'core')
				asArray = {packageName2}.
	self
		assert:
			(x := component conditionalProjectsAtConditions: {'gemstone'} andGroup: 'core')
				asArray = {}.

	self
		assert:
			(x := component conditionalComponentsAtConditions: {'vast'} andGroup: 'core')
				asArray = {}.
	self
		assert:
			(x := component conditionalPackagesAtConditions: {'vast'} andGroup: 'core')
				asArray = {packageName2}.
	self
		assert:
			(x := component conditionalProjectsAtConditions: {'vast'} andGroup: 'core')
				asArray = {}.
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
