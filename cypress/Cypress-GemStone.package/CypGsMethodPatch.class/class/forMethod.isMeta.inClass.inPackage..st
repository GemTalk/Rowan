"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
forMethod: aMethodDefinition isMeta: isMeta inClass: aClassDefinition inPackage: aPackageDefinition

	^(self new)
		methodDefinition: aMethodDefinition;
		isMeta: isMeta;
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself