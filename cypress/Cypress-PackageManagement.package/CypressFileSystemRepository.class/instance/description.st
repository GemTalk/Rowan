"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
description
  | desc |
  desc := super description.
  ^ desc notEmpty
    ifTrue: [ desc ]
    ifFalse: [ self url asString ]