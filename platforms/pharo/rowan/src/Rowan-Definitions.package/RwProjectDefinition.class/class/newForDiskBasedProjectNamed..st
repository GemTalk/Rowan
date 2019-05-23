instance creation
newForDiskBasedProjectNamed: aString

	| spec |
	spec := RwSimpleProjectSpecification new
		specName: aString;
		repoSpec: RwDiskRepositorySpecification new;
		yourself.
	^ self newForSpecification: spec
