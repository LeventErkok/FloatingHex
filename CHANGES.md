* Hackage: <http://hackage.haskell.org/package/FloatingHex>
* GitHub:  <http://github.com/LeventErkok/FloatingHex/>

* Latest Hackage released version: 0.2, 2017-01-15

### Version 0.3, Not yet released

  * Bump up template-haskell dependency to >= 2.10. As noted by Herbert Valerio Riedel,
    FloatingHex fails to compile with older versions.

  * Make the double->float conversions more robust, by avoiding the rational route.
    (Avoids issues in https://ghc.haskell.org/trac/ghc/ticket/3676)

### Version 0.2, 2017-01-14

  * Support for parsing nan/infinity values
  * Make the printer compliant with printf %a modifier in C

### Version 0.1, 2017-01-14

  * First implementation. The quasiquoter and the pretty-printer are implemented.
