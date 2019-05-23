visiting
acceptVisitor: aVisitor

	^self error: 'nested configuration cannot be used as a top-level configuration. The receiver is nested inside of project load configurations'
