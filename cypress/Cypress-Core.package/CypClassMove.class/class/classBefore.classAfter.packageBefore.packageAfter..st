"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
classBefore: beforeClassDefinition classAfter: afterClassDefinition packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition

	^(self new)
		classBefore: beforeClassDefinition;
		classAfter: afterClassDefinition;
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		yourself