"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
packageDefinition: aPackageDefinition classDefinitionOrExtension: aClassDefinitionOrExtension methodKey: aMethodKey isMeta: isMeta methodsModification: aMethodsModification

	^(self new)
		packageDefinition: aPackageDefinition;
		classDefinitionOrExtension: aClassDefinitionOrExtension;
		methodKey: aMethodKey;
		isMeta: isMeta;
		methodsModification: aMethodsModification;
		yourself