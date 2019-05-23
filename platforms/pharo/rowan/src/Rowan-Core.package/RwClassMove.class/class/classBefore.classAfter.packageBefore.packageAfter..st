instance creation
classBefore: beforeClassDefinition classAfter: afterClassDefinition packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition

	^(self new)
		classBefore: beforeClassDefinition;
		classAfter: afterClassDefinition;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		yourself