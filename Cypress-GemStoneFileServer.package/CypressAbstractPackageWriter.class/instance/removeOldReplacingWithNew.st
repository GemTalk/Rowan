"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing
"
removeOldReplacingWithNew

	self fileUtils deleteAll: self packageDirectory.
	self writePropertiesFile.
	self writePackageStructure