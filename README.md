# Kinten's ppx-expect

ppx-expect (a.k.a. ppx-k-expect, `Ppx_k_expect`) is a framework for writing Cram-like tests in OCaml. It is heavily inspired by Janestreet's [Expect-test](https://github.com/janestreet/ppx_expect), which is an amazing framework in and of itself and is natively integrated with the Dune build system, but that integration means I couldn't use it in other OCaml build systems (e.g. B0) which was a source of frustration for me.

(From this point on, ppx-expect refers to Kinten's ppx-expect)

ppx-expect is "build system"-agnostic. The only requirement for using it is that your compiler / build-system supports PPX, and that your compiler has access to its stdin and stderr while running (which should be true for most compilers)
