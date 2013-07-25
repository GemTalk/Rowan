converting
stringForVariables: variableList

	^String streamContents: 
			[:stream |
			variableList do: [:each | stream nextPutAll: each]
				separatedBy: [stream space]]