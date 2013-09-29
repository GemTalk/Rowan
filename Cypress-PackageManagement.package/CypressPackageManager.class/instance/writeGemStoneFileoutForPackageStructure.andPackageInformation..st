"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
writeGemStoneFileoutForPackageStructure: packageStructure andPackageInformation: aCypressPackageInformation

	| filePath |
	filePath := aCypressPackageInformation savedLocation, aCypressPackageInformation name , '.gs'.
	aCypressPackageInformation repository areGemStoneFileoutsEnabled
		ifTrue: 
			[(GsFile openWriteOnServer: filePath)
				nextPutAll: (String streamContents: [:stream | packageStructure fileOutOn: stream]);
				close]
		ifFalse: [GsFile removeServerFile: filePath]