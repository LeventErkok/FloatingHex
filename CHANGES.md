* Hackage: <http://hackage.haskell.org/package/FloatingHex>
* GitHub:  <http://github.com/LeventErkok/FloatingHex/>

* Latest Hackage released version: 0.1, 2017-01-14

### Version 0.1, 2017-01-14

  * First implementation. The quasiquoter and the pretty-printer are implemented.

  * NB. The pretty-printer is currently not 100% compatible with the %a modifier
    of C printf function. While it will print correct values, it will not always
    print the same string as C does. (Note that string representations are not
    unique for hexadecimal floats, similar to the scientific notation.)
