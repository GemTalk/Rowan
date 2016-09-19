"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
unloadDefinition

  self actualClass ifNotNil: [:cl | cl removeSelector: self selector asSymbol ].
