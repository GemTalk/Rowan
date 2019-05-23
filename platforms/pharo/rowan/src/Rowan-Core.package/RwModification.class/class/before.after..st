instance creation
before: beforeDefinition after: afterDefinition

	^(self new initialize)
		before: beforeDefinition;
		after: afterDefinition;
		yourself