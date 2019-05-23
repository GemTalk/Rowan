instance creation
projectDefinition: aProjectDefinition packageDefinition: aPackageDefinition classKey: aClassKey classesModification: aClassesModification

	^(self new)
		projectDefinition: aProjectDefinition;
		packageDefinition: aPackageDefinition;
		classKey: aClassKey;
		classesModification: aClassesModification;
		yourself
