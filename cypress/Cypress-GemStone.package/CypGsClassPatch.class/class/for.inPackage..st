"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
for: aClassDefinition inPackage: aPackageDefinition

	^(self new)
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself