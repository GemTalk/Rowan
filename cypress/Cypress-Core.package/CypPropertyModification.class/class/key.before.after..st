"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
key: key before: beforeDefinition after: afterDefinition

	^(super before: beforeDefinition after: afterDefinition)
		key: key;
		yourself