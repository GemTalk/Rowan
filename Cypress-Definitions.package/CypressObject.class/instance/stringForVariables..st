"
notice: This work is protected by copyright. All rights reserved.
category: converting
"
stringForVariables: variableList

	^String streamContents: 
			[:stream |
			variableList do: [:each | stream nextPutAll: each]
				separatedBy: [stream space]]