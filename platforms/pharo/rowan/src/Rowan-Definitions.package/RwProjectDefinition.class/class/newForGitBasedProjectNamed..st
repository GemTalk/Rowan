instance creation
newForGitBasedProjectNamed: aString

	| spec |
	spec := RwSimpleProjectSpecification new
		specName: aString;
		yourself.
	^ self newForSpecification: spec
