"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: instance creation
"
inRepository: aRepository name: nameString

	^(self new)
		repository: aRepository;
		name: nameString;
		yourself