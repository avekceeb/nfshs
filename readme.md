### Note

Thanks to Yuriy Lazaryev for
[Xdr Parser libary](https://github.com/Unisay/haskell-xdr-parser)

Two small modifications made to this lib:
- accept underscores in identifier names (they are used throughout nfs xdr)
- changed Parser.unionDiscriminant to keep Enum id info


### ???
	Doesn't work with resolver: lts-13.18

	Set to resolver: lts-12.7


### XDR Syntax problems so far
- keyword `program` is not in RFC
- `argop` and `resop` as `unsigned` in switch
- symbol `%` - where did it come from?
- hexadecimals in enum
