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
		conditionalPackagesAtConditions: {'common'}
			andGroup: 'core'
			addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	conditionalPackages := component conditionalPackages.
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
		conditionalPackagesAtConditions: {'common'}
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
		conditionalPackagesAtConditions: {'common'}
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
		conditionalPackagesAtConditions: {'common'}
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
		conditionalPackagesAtConditions: {'common'}
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
