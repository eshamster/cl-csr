![.github/workflows/main.yml](https://github.com/eshamster/cl-csr/workflows/.github/workflows/main.yml/badge.svg)

# CL-CSR - A client side reindering libirary in Common Lisp

CL-CSR is a client side reindering libirary in Common Lisp.

* This is only in alpha quality.

## Execute sample

Please clone this (and [ps-experiment](https://github.com/eshamster/ps-experiment)) to where quicklisp (asdf) can find. Then, load it by `ql:quickload` and start server. After that, you can access to http://localhost:5000 .

```lisp
CL-USER> (ql:quickload :sample-cl-csr)
CL-USER> (sample-cl-csr:start :port 5000)
```


## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2019 eshamster (hamgoostar@gmail.com)

## License

Licensed under the MIT License.
