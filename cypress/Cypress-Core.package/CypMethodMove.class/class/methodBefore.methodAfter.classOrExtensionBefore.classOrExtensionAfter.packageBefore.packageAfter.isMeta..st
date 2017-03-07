"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: unknown
"
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