"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: authentication
"
user: userId
	"Return the requesting user."
	^users at: userId ifAbsent: [ self error: (self class unauthorizedFor: realm) ]