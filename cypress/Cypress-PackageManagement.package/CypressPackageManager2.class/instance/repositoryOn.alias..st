"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Initializing - private
"
repositoryOn: url alias: aString
	"Answer a repository instance for the specified URL.
	 The characteristics will come from the properties file at the URL location,
	 or will default according to the schema, if there is no properties file."

	^self knownRepositories
		at: url
		ifAbsentPut: [CypressAbstractRepository onUrl: url alias: aString].
