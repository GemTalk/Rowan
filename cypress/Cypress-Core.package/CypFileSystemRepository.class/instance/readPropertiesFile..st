"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readPropertiesFile: fileName

	disk
		readStreamFor: fileName
		in: self directoryPath
		do: [:fileStream | properties := self propertiesParserClass parseStream: fileStream]