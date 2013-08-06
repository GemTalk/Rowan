"
notice: This work is protected by copyright. All rights reserved.
category: enumerating
"
classDefinitions: classBlock methodDefinitions: methodBlock

	self definitions do: [:definition |
		definition classDefinition: classBlock methodDefinition: methodBlock]
