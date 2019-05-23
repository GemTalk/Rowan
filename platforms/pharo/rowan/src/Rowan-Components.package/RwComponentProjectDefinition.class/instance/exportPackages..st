exporting
exportPackages: diskProjectSetDefinition

	| projectSetDefinition visitor projectSetModification writerVisitorClass |
	(self packagesRoot /  'properties', 'st') 
		writeStreamDo: [ :fileStream | 
			fileStream nextPutAll: '{ '; 
				lf;
				tab;
				nextPutAll: '#format : ' , self packageFormat printString, ',' ;
				lf;
				tab;
				nextPutAll: '#convention : ', self packageConvention printString;
				lf;
				nextPutAll: '}';
				lf ].

"write out packages"
	writerVisitorClass := self packageFormat = 'tonel'
		ifTrue: [ RwModificationTonelWriterVisitor ]
		ifFalse: [ RwModificationFiletreeWriterVisitor ].
	projectSetDefinition:= RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition compareAgainstBase: diskProjectSetDefinition.
	visitor := writerVisitorClass new.

	visitor visit: projectSetModification.