run
	"should be run as DataCurator with Rowan loaded as SystemUser"
	| specUrlString project suite strm res resultCases |

	specUrlString := 'file:$ROWAN_PROJECTS_HOME/Rowan/rowan/specs/RowanV2_DataCurator.ston'.
	Rowan projectTools load loadFromUrl: specUrlString.
	project := Rowan projectNamed: 'Rowan_DataCurator'.
	suite := project testSuite.
	res := suite run.

	strm := WriteStream on: String new.
	strm lf; lf.
	strm nextPutAll: suite name, ' for GemStone ', (System gemVersionAt: #gsVersion) printString; lf.
	strm nextPutAll: res printString; lf.
	strm nextPutAll: '  errors'; lf.
	(res errors collect: [:each | each printString ]) asArray sort do: [:each |
		strm tab; nextPutAll: each; lf].
	res failures size = 0
		ifTrue: [ ^ strm contents ].
	strm nextPutAll: '  failures'; lf.
	(res failures collect: [:each | each printString]) asArray sort do: [:each |
		strm tab; nextPutAll: each; lf].

	strm contents
%
