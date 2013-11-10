"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: enumerating
"
classDefinitions: classBlock methodDefinitions: methodBlock

	self definitions do: [:definition |
		definition classDefinition: classBlock methodDefinition: methodBlock]
