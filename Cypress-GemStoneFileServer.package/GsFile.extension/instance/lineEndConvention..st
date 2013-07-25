*Cypress-GemStoneFileServer
lineEndConvention: aSymbol

	aSymbol = #lf ifTrue: [^self].
	self error: 'GsFile does not support line end conventions other than #lf'
