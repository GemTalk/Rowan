"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
currentPackageInformationGroups

	^currentManager packageInformationList
		inject: Dictionary new
		into: 
			[:dict :each |
			(dict at: each class ifAbsentPut: [OrderedCollection new]) add: each.
			dict]