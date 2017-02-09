"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
before: beforeDefinition after: afterDefinition

	^(self new initialize)
		before: beforeDefinition;
		after: afterDefinition;
		yourself