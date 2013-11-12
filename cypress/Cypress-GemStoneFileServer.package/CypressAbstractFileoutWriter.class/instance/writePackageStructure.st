"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing
"
writePackageStructure

	CypressFileUtilities current
		writeStreamFor: self packageStructure packageName, self packageNameExtension
		in: self directory
		do: [:fileStream | fileStream nextPutAll: (String streamContents: [:stream | self fileOutPackageOn: stream])]
