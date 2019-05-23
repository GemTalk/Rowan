instance creation
packageDefinition: aPackageDefinition classKey: aClassKey classesModification: aClassesModification

	^(self new)
		packageDefinition: aPackageDefinition;
		classKey: aClassKey;
		classesModification: aClassesModification;
		yourself