actions
readProjectSetForPackageNames: packageNames
	"drop all existing packages on the floor and replace with fresh versions of the packageNames read from disk"

	| format visitorClass |
	packages := Dictionary new. 
	format := self 
		packageFormatIfAbsent: [  
			| formatFromDisk |
			formatFromDisk := (RwAbstractReaderWriterVisitor _repositoryPropertyDictFor: self packagesRoot)
				at: #format ifAbsent: [ 'tonel' ].
			self packageFormat: formatFromDisk.
			formatFromDisk ].
	visitorClass := format = 'tonel'
		ifTrue: [ RwRepositoryComponentProjectTonelReaderVisitor ]
		ifFalse: [ RwRepositoryComponentProjectFiletreeReaderVisitor ].
	^ visitorClass new
		packageNames: packageNames;
		visit: self.