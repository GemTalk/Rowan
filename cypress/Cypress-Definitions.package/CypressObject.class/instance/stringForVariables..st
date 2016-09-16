"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: converting
"
stringForVariables: variableList

	^String streamContents: 
			[:stream |
			variableList do: [:each | stream nextPutAll: each]
				separatedBy: [stream space]]