"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
packageNames

	^(self fileUtils directoryEntriesFrom: self directoryPath , '*', self packageExtension)
		collect: [:each | (self fileUtils localNameFrom: each) copyWithoutSuffix: self packageExtension]