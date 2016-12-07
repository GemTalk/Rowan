"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
forGemStone

	| platformClass |
	platformClass := CypPlatformSpecificNameResolver
				resolve: 'CypGemStonePlatform'
				ifAbsent: 
					[self
						error: 'The CypressGemStone prerequisite package does not seem to be installed.'].
	^platformClass new