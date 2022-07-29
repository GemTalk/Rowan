Class {
	#name : 'RwLoadSpecificationV2Test',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests-SpecificationsV2'
}

{ #category : 'private' }
RwLoadSpecificationV2Test >> _sampleSymbolDictionaryName1 [

	^ #'RowanSample9_1'
]

{ #category : 'private' }
RwLoadSpecificationV2Test >> _sampleSymbolDictionaryName2 [

	^ #'RowanSample9_2'
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testBasic_1 [
	"excercise basic functionality"

	| projectName loadSpecification stonString specName |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		componentNames: #('Default');
		projectSpecFile: 'rowan/xxx.ston';
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	self assert: loadSpecification  _validate
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testBasic_2 [
	"excercise basic functionality"

	| projectName loadSpecification stonString specName |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		componentNames: #('Default');
		projectSpecFile: 'rowan/xxx.ston';
		revision: 'master';
		gitUrl: 'file:/x/y/z';
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	self assert: loadSpecification  _validate
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testBasic_3 [
	"excercise basic functionality"

	| projectName loadSpecification stonString specName |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 2;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	self assert: loadSpecification  _validate
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testComparison_1 [
	| projectName loadSpecification specName projectSpecCopy stonString stonStringCopy x |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	self
		assert:
			(x := RwLoadSpecificationV2 allInstVarNames)
				=
					#(#'specName' #'projectName' #'projectAlias' #'gitUrl' #'diskUrl' #'mercurialUrl' #'readonlyDiskUrl' #'relativeRepositoryRoot' #'svnUrl' #'revision' #'projectSpecFile' #'versionPrefix' #'componentNames' #'customConditionalAttributes' #'platformProperties' #'comment' #'projectsHome' #'repositoryResolutionPolicy').	"If inst vars don't match, copy and hash methods have to change"

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.

	stonStringCopy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = loadSpecification.
	self assert: projectSpecCopy hash = loadSpecification hash
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testComparison_2 [
	"compare equal even if lazy initialization has taken place"

	| projectName loadSpecification specName projectSpecCopy stonString stonStringCopy stonStringLazy |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.

	stonStringCopy := STON toStringPretty: projectSpecCopy.

	projectSpecCopy platformProperties.	"trigger the selectors that cause lazy initialization"

	stonStringLazy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = loadSpecification.
	self assert: projectSpecCopy hash = loadSpecification hash.
	self deny: stonStringLazy = stonStringCopy
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testInvalidPropertyValue [
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 'boom';
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Value of property (#''defaultMethodEnv''->''boom'') is expected to be class ''SmallInteger'' not class ''String'''.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testInvalidPropertyValueForUser [
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 'boom';
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Value of property (#''defaultMethodEnv''->''boom'') is expected to be class ''SmallInteger'' not class ''String'''.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testInvalidRevision [
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		revision: 'boom';
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Invalid revision ''boom''. Should be nil for disk-based repository'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testIssue_530_1 [
	"https://github.com/GemTalk/Rowan/issues/530"

	"state changes to a copy of a loadSpec should not affect oriinal load spec"

	"original has no predefined platformSpec dictionary"

	| projectName loadSpecification stonStrings specName projectSpecCopy stonString stonStringCopy |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.
	stonStringCopy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = loadSpecification.

	projectSpecCopy
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 2;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	self deny: projectSpecCopy = loadSpecification.

	stonStrings := {
	(STON toStringPretty: loadSpecification).
	(STON toStringPretty: projectSpecCopy)}	"useful in case of test failure"
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testIssue_530_2 [
	"https://github.com/GemTalk/Rowan/issues/530"

	"state changes to a copy of a loadSpec should not affect oriinal load spec"

	"predefine platformSpec dictionary in original"

	| projectName loadSpecification stonStrings specName projectSpecCopy stonString |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultSymbolDictNameForUser: 'Bozo'
			to: self _sampleSymbolDictionaryName2;
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.

	self assert: projectSpecCopy = loadSpecification.

	projectSpecCopy
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 2;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	self deny: projectSpecCopy = loadSpecification.

	stonStrings := {stonString.	"original loadSpec"
	(STON toStringPretty: loadSpecification).	"origiinal after copy modified"
	(STON toStringPretty: projectSpecCopy)	"copy"}.
	self assert: stonString = (stonStrings at: 1).	"duh"
	self assert: stonString = (stonStrings at: 2).	"point of test"
	self deny: stonString = (stonStrings at: 3)	"duh"
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testIssue_530_3 [
	"https://github.com/GemTalk/Rowan/issues/530"

	"state changes to a copy of a loadSpec should not affect oriinal load spec"

	"componentNames and groupNames need to be isolated"

	| projectName loadSpecification stonStrings specName projectSpecCopy stonString |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		componentNames: #('Default');
		customConditionalAttributes: {'mine'};
		projectSpecFile: 'rowan/xxx.ston';
		revision: 'master';
		gitUrl: 'file:/x/y/z';
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.

	self assert: projectSpecCopy = loadSpecification.

	projectSpecCopy componentNames add: 'Boom'.
	projectSpecCopy customConditionalAttributes add: 'boom'.

	self deny: projectSpecCopy = loadSpecification.

	stonStrings := {stonString.	"original loadSpec"
	(STON toStringPretty: loadSpecification).	"origiinal after copy modified"
	(STON toStringPretty: projectSpecCopy)	"copy"}.
	self assert: stonString = (stonStrings at: 1).	"duh"
	self assert: stonString = (stonStrings at: 2).	"point of test"
	self deny: stonString = (stonStrings at: 3)	"duh"
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testMissingRevision [
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		gitUrl: 'https://github.com/user/' , projectName;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: The instance variable ''revision'' must be set for the''gitUrl'''.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testNilInstanceVariable [
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectSpecFile: nil;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: The instance variable ''projectSpecFile'' cannot be nil'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testOnlyOneRepositoryUrl [
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		diskUrl: 'ftp://$ROWAN_PROJECTS_HOME/' , projectName;
		yourself.
	loadSpecification
		instVarAt: (loadSpecification class instVarNames indexOf: #'gitUrl')
		put: 'https://github.com/user/' , projectName.	"simulate manual editting of spec on disk"

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Only one of (gitUrl diskUrl mercurialUrl readOnlyDiskUrl svnUrl) must be be set'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testRevisionMustBeSet [
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		gitUrl: 'https://github.com/user/' , projectName;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: The instance variable ''revision'' must be set for the''gitUrl'''.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testUknownPlatform [
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		yourself.

	hitError := false.
	loadSpecification platformProperties at: 'boom' put: Dictionary new.
	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: Unknown platform name ''boom'' in platform properties'.
			hitError := true ].
	self assert: hitError
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testUnknownPropertyKey [
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		yourself.
	(loadSpecification platformProperties at: 'gemstone')
		at: 'BOOM'
		put:
			(Dictionary new
				at: #'boom' put: nil;
				yourself).

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description) = 'Error: Unknown platform property key #''boom'''.
			hitError := true ].
	self assert: hitError
]
