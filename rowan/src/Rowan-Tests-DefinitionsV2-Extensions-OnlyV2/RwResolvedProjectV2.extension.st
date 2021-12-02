Extension { #name : 'RwResolvedProjectV2' }

{ #category : '*rowan-tests-definitionsv2-extensions-onlyv2' }
RwResolvedProjectV2 >> create [
	"RwComponentProjectDefinition tests compatibility ... eventually get rid of this"
	self resolve; export
]

{ #category : '*rowan-tests-definitionsv2-extensions-onlyv2' }
RwResolvedProjectV2 >> exportPharoTonelFormatV1 [

	Rowan projectTools writeV2 writeResolvedProjectPharoTonelFormatV1: self
]

{ #category : '*rowan-tests-definitionsv2-extensions-onlyv2' }
RwResolvedProjectV2 >> exportPharoTonelFormatV1Packages [
	| projectSetDefinition |
	projectSetDefinition := [ 
	RwProjectSetDefinition new
		addProject: self copy read;
		yourself ]
		on: Error
		do: [ :ignored | RwProjectSetDefinition new ].
	self
		exportPharoTonelFormatV1Packages: projectSetDefinition
		packagesRoot: self packagesRoot
		packageFormat: self packageFormat
		packageConvention: self packageConvention
]

{ #category : '*rowan-tests-definitionsv2-extensions-onlyv2' }
RwResolvedProjectV2 >> exportPharoTonelFormatV1Packages: diskProjectSetDefinition packagesRoot: packagesRoot packageFormat: packageFormat packageConvention: packageConvention [
	| projectSetDefinition visitor projectSetModification writerVisitorClass |
	packagesRoot / 'properties' , 'st'
		writeStreamDo: [ :fileStream | 
			fileStream
				nextPutAll: '{ ';
				lf;
				tab;
				nextPutAll: '#format : ' , packageFormat printString , ',';
				lf;
				tab;
				nextPutAll: '#convention : ' , packageConvention printString;
				lf;
				nextPutAll: '}';
				lf ].	"write out packages"
	writerVisitorClass := packageFormat = 'tonel'
		ifTrue: [ RwModificationPharoTonelFormatV1WriterVisitorV2 ]
		ifFalse: [ self error: 'should not be used for filetree format packages' ].
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: self.
	projectSetModification := projectSetDefinition
		compareAgainstBase: diskProjectSetDefinition.
	visitor := writerVisitorClass new
		packagesRoot: packagesRoot;
		yourself.

	visitor visit: projectSetModification
]
