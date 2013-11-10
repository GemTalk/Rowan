"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
writeGemStoneFileoutForPackageStructure: packageStructure andPackageInformation: aCypressPackageInformation

	aCypressPackageInformation repository areGemStoneFileoutsEnabled
		ifTrue: 
			[CypressFileUtilities current
				writeStreamFor: aCypressPackageInformation name , '.gs'
				in: aCypressPackageInformation savedLocation
				do: [:fileStream | fileStream nextPutAll: (String streamContents: [:stream | packageStructure fileOutOn: stream])]]
		ifFalse: [GsFile removeServerFile: aCypressPackageInformation savedLocation, aCypressPackageInformation name , '.gs']