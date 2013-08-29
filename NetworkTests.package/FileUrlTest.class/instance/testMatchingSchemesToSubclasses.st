testing
testMatchingSchemesToSubclasses

	{ { nil. GenericUrl }. "Assume HTTP by default (i.e. when no scheme is provided)"
	{ 'isbn'. GenericUrl }. "Handle unknown Url types with GenericUrl"
	{ 'http'. HttpUrl }.
	{ 'https'. HttpsUrl }.
	{ 'file'. FileUrl }.
	{ 'mailto'. MailtoUrl }.
	{ 'browser'. BrowserUrl } } do: [ :pair | | schemeString urlClassToUse |
		schemeString := pair first.
		urlClassToUse := pair at: 2.
		self assert: (Url urlClassForScheme: schemeString) equals: urlClassToUse ].