accessing
selectorSpecials

	^ specials ifNil: [ specials := self initializeSpecials ]