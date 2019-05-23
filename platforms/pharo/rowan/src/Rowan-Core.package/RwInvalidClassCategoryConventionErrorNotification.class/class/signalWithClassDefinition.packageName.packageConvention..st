instance creation
signalWithClassDefinition: aClassDefinition packageName: packageName packageConvention: aString

	^ self new
			classDefinition: aClassDefinition;
			packageName:  packageName;
			packageConvention: aString;
			signal