instance creation
signalWithMethodDefinition: aMethodDefinition className: className isMeta: isMeta packageName:  packageName packageConvention: aString

	^ self new
			methodDefinition: aMethodDefinition;
			className: className;
			isMeta: isMeta;
			packageName:  packageName;
			packageConvention: aString;
			signal