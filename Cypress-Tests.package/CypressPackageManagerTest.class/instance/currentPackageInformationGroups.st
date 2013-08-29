private
currentPackageInformationGroups

	^currentManager packageInformationList
		inject: Dictionary new
		into: 
			[:dict :each |
			(dict at: each class ifAbsentPut: [OrderedCollection new]) add: each.
			dict]