Class {
	#name : 'RwProjectSpecificationV2Test',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests-SpecificationsV2'
}

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testBasic [
	"excercise basic functionality"

	| projectName projectSpecification stonString specName |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	self assert: projectSpecification  _validate
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testComparison_1 [
	| projectName projectSpecification specName projectSpecCopy stonString stonStringCopy x |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.

	self
		assert:
			(x := RwProjectSpecificationV2 allInstVarNames)
				=
					#(#'specName' #'projectName' #'projectSpecPath' #'componentsPath' #'packagesPath' #'projectsPath' #'specsPath' #'packageFormat' #'packageConvention' #'comment' #'repoType' #'loadedCommitId').	"If inst vars don't match, copy and hash methods have to change"

	stonString := STON toStringPretty: projectSpecification.

	projectSpecCopy := projectSpecification copy.

	stonStringCopy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = projectSpecification.
	self assert: projectSpecCopy hash = projectSpecification hash
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testComparison_2 [
	"compare equal even if lazy initialization has taken place"

	| projectName projectSpecification specName projectSpecCopy stonString stonStringCopy stonStringLazy |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: projectSpecification.

	projectSpecCopy := projectSpecification copy.

	stonStringCopy := STON toStringPretty: projectSpecCopy.

	projectSpecCopy repoType.	"trigger the selectors that cause lazy initialization"

	stonStringLazy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = projectSpecification.
	self assert: projectSpecCopy hash = projectSpecification hash.
	self deny: stonStringLazy = stonStringCopy
]

{ #category : 'tests' }
RwProjectSpecificationV2Test >> testNilInstanceVariable [
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		projectsPath: nil;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: The instance variable ''projectsPath'' cannot be nil'.
			hitError := true ].
	self assert: hitError
]
