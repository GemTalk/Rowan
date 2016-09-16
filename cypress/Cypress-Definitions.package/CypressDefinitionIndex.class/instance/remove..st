"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: removing
"
remove: aDefinition
	self definitionMap removeKey: aDefinition description ifAbsent: []
