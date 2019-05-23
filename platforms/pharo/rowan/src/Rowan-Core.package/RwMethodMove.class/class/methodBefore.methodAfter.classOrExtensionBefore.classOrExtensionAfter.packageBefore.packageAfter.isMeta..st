unknown
methodBefore: beforeMethodDefinition methodAfter: afterMethodDefinition classOrExtensionBefore: beforeClassDefinitionOrExtension classOrExtensionAfter: afterClassDefinitionOrExtension packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition isMeta: isMeta

	^(self new)
		methodBefore: beforeMethodDefinition;
		methodAfter: afterMethodDefinition;
		classOrExtensionBefore: beforeClassDefinitionOrExtension;
		classOrExtensionAfter: afterClassDefinitionOrExtension;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		isMeta: isMeta;
		yourself