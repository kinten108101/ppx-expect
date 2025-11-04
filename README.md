# Kinten's ppx-expect

ppx-expect (a.k.a. ppx-k-expect, `Ppx_k_expect`) is a framework for writing Cram-like tests in OCaml. It is heavily inspired by Janestreet's [Expect-test](https://github.com/janestreet/ppx_expect), which is an amazing framework in and of itself and is natively integrated with the Dune build system, but that integration means I couldn't use it in other OCaml build systems (e.g. B0) which was a source of frustration for me.

(From this point on, ppx-expect refers to Kinten's ppx-expect)

ppx-expect is "build system"-agnostic.

# Walkthrough

ppx-expect will check if it is enabled by querying if the environment variable `KEXPECT_TEST` is set to `y` (which is done by calling `Sys.getenv`, and it will optionally read from a `.env` file in CWD in case your build system environment is pure a.k.a. doesn't inherit environment variables from shell).

If ppx-expect is enabled, during preprocessing it will expand `%expect_test` evaluation blocks throughout your source code, like

```ocaml
let%expect_test "test name" =
    Printf.printf "%d %d" 1 2;
    [%expect {| 1 2 |}]
```

into test blocks, and when you execute your program these test blocks will be run.

If the test results don't match, expect-test will prompt you to promote them, and changes will be applied to your source code in real-time

```
-|  [%expect {| |1 1 0 3| |}
+|  [%expect {| |1 0 0 3| |}
Promote? [y/n] y
```
