instance creation
classBefore: beforeClassDefinition classAfter: afterClassDefinition packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition projectBefore: beforeProjectDefinition projectAfter: afterProjectDefinition

	^(self new)
		classBefore: beforeClassDefinition;
		classAfter: afterClassDefinition;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		projectBefore: beforeProjectDefinition;
		projectAfter: afterProjectDefinition;
		yourself
