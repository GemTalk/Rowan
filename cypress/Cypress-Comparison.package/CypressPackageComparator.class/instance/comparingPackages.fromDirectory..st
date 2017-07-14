"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing
"
comparingPackages: someNames fromDirectory: aDirectory

	(directoryPackageMap at: aDirectory ifAbsentPut: [OrderedCollection new])
		addAll: someNames.
	someNames do: 
			[:packageName |
			| reader modTime modTimestamp |
			reader := (CypressFileSystemRepository on: aDirectory) reader
						readPackageStructureForPackageNamed: packageName.
			diskSnapshots at: packageName put: reader packageStructure snapshot.
			modTime := System
						performOnServer: 'stat --printf=%y ' , reader packageDirectory.
			modTimestamp := (modTime indexOfSubCollection: 'stat:') = 1
						ifTrue: [nil]
						ifFalse: [self dateAndTimeFromUnixFormatString: modTime].
			diskTimestamps at: packageName put: modTimestamp.
			imageSnapshots at: packageName
				put: (CypressPackageDefinition named: packageName) snapshot]
