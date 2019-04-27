### Note

Thanks to Yuriy Lazaryev for
[Xdr Parser libary](https://github.com/Unisay/haskell-xdr-parser)

Small modification added to accept underscores in identifier names (they are used throughout nfs xdr)


### ???
	Doesn't work with resolver: lts-13.18
	
	Set to resolver: lts-12.7


### XDR Syntax problems so far
- keyword `program` is not in RFC
- `argop` and `resop` as `unsigned` in switch
- symbol `%` - where did it come from?
- hexadecimals in enum
