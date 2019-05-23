instance creation
packageDefinition: aPackageDefinition classDefinitionOrExtension: aClassDefinitionOrExtension methodKey: aMethodKey isMeta: isMeta methodsModification: aMethodsModification

	^(self new)
		packageDefinition: aPackageDefinition;
		classDefinitionOrExtension: aClassDefinitionOrExtension;
		methodKey: aMethodKey;
		isMeta: isMeta;
		methodsModification: aMethodsModification;
		yourself