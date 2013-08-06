"
notice: This work is protected by copyright. All rights reserved.
category: instance creation
"
fromJson: aJsonString

	^self fromJs: (CypressJsonParser parse: aJsonString)
