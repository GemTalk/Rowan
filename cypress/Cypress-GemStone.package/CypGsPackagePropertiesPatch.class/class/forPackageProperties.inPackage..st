"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
forPackageProperties: aPropertiesModification inPackage: aPackageDefinition

	^(self new)
		packageDefinition: aPackageDefinition;
		propertiesModification: aPropertiesModification;
		yourself