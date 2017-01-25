"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
readPropertiesFileFromPath: aPath

	^disk readStreamFor: aPath
		do: [:stream | CypressJsonParser parseStream: stream]