Class {
	#name : 'RwLoadSpecificationV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testInvalidRevision [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		revision: 'boom';
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification validate ]
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

	| projectName projectSpecification stonStrings specName projectSpecCopy stonString |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: projectSpecification.

	projectSpecCopy := projectSpecification copy.

	self assert: projectSpecCopy = projectSpecification.

	projectSpecCopy componentNames add: 'Boom'.
	projectSpecCopy groupNames add: 'boom'.

	self deny: projectSpecCopy = projectSpecification.

	stonStrings := {stonString.	"original loadSpec"
	(STON toStringPretty: projectSpecification).	"origiinal after copy modified"
	(STON toStringPretty: projectSpecCopy)	"copy"}.
	self assert: stonString = (stonStrings at: 1).	"duh"
	self assert: stonString = (stonStrings at: 2).	"point of test"
	self deny: stonString = (stonStrings at: 3)	"duh"
]

{ #category : 'tests' }
RwLoadSpecificationV2Test >> testMissingRevision [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		gitUrl: 'https://github.com/user/' , projectName;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification validate ]
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
RwLoadSpecificationV2Test >> testOnlyOneRepositoryUrl [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		gitUrl: 'https://github.com/user/' , projectName;
		diskUrl: 'ftp://$ROWAN_PROJECTS_HOME/' , projectName;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification validate ]
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
