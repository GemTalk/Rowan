instance creation
projectDefinition: aProjectDefinition packageDefinition: aPackageDefinition classDefinitionOrExtension: aClassDefinitionOrExtension methodKey: aMethodKey isMeta: isMeta methodsModification: aMethodsModification

	^(self new)
		projectDefinition: aProjectDefinition;
		packageDefinition: aPackageDefinition;
		classDefinitionOrExtension: aClassDefinitionOrExtension;
		methodKey: aMethodKey;
		isMeta: isMeta;
		methodsModification: aMethodsModification;
		yourself
