"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
imageDefinitionCounts

	| classCount methodCount |
	classCount := methodCount := 0.
	self imageDefinitions do: 
			[:each |
			each classDefinition: [:classDefinition | classCount := classCount + 1]
				methodDefinition: [:methodDefinition | methodCount := methodCount + 1]].
	^Array with: classCount with: methodCount
