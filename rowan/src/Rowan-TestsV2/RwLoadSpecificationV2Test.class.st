Class {
	#name : 'RwLoadSpecificationV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

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
		groupNames: #('core');
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
		groupNames: #('core');
		projectSpecFile: 'rowan/xxx.ston';
		revision: 'master';
		gitUrl: 'file://x/y/z';
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	self assert: loadSpecification  _validate
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testComparison_1 [
	| projectName loadSpecification specName projectSpecCopy stonString stonStringCopy |
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
	self assert: projectSpecCopy hash = loadSpecification hash
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
		groupNames: #('core');
		projectSpecFile: 'rowan/xxx.ston';
		revision: 'master';
		gitUrl: 'file://x/y/z';
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.

	self assert: projectSpecCopy = loadSpecification.

	projectSpecCopy componentNames add: 'Boom'.
	projectSpecCopy groupNames add: 'boom'.

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
		gitUrl: 'https://github.com/user/' , projectName;
		diskUrl: 'ftp://$ROWAN_PROJECTS_HOME/' , projectName;
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
						= 'Error: Only one of (gitUrl diskUrl mercurialUrl svnUrl) must be be set'.
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
