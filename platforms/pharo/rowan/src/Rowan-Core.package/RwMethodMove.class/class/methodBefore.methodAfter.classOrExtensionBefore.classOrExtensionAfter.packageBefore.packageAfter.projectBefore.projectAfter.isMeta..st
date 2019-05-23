unknown
methodBefore: beforeMethodDefinition methodAfter: afterMethodDefinition classOrExtensionBefore: beforeClassDefinitionOrExtension classOrExtensionAfter: afterClassDefinitionOrExtension packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition projectBefore: beforeProjectDefinition projectAfter: afterProjectDefinition isMeta: isMeta

	^(self new)
		methodBefore: beforeMethodDefinition;
		methodAfter: afterMethodDefinition;
		classOrExtensionBefore: beforeClassDefinitionOrExtension;
		classOrExtensionAfter: afterClassDefinitionOrExtension;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		projectBefore: beforeProjectDefinition;
		projectAfter: afterProjectDefinition;
		isMeta: isMeta;
		yourself
