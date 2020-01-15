Class {
	#name : 'TonelExampleApp',
	#superclass : 'Application',
	#category : 'TonelExampleApp'
}

{
	#category : 'Not categorized'
}
TonelExampleApp class >> _PRAGMA_TonelExampleConstants [


"%%PRAGMA DECLARE
	(name: TonelExampleConstants isPool: true)
	(pool: TonelExampleConstants declarations: (
		(name: Constant1 isConstant: true valueExpression: '1')
		(name: Constant2 isConstant: true valueExpression: '2')
	
	))
	"
]

{
	#category : 'Not categorized'
}
TonelExampleApp class >> writeAppToTonel [

	| writer fileSystem |

	fileSystem := (Smalltalk at: #TonelCommonFileSystem) new.
	writer := (Smalltalk at: #TonelWriter) on: fileSystem.
	writer writeInWorkingDirectoryProjectIncluding: (Array with: TonelExampleApp)
]

{
	#category : 'Not categorized'
}
TonelExampleApp class >> writeAppToTonelToGit [

	| writer fileSystem |

	fileSystem := (Smalltalk at: #TonelCommonFileSystem) new.
	writer := (Smalltalk at: #TonelWriter) on: fileSystem.
	writer
		writeApplication: TonelExampleApp
		into: (CfsPath named: 'c:\Users\mpeck\Documents\Instantiations\git\tonel-demos\source\')
]
