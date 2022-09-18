---
[[To project source]](#top) •|||• [[See live web app]](http://bilingual.curlgrep-phantom-funspec.hu:3003?lang=en) •|||• [[Back to central personal homepage]](https://alignalghii.github.io)

---

# Bilingual practice — a sample web app implemented with pure functional programming

## User documentation

This small web app is a simple, quiz-like self-learning tool to practice foreign-languages phrases and words (here: helping the practice activity of Hungarian pupils in their learning English).

[You can see and try it here](http://bilingual.curlgrep-phantom-funspec.hu:3003?lang=en), the labels and explanations are all in English.

## Developer documentation

It is implemented in Scotty, a Haskell microframework. For database, it uses a small self-made file-based storage library.

As for coding style, it intends to be extrmely DRY by using the tools where declarative and functional programming languages shine: algebraic datatypes, monad transformers, domain-specific embedded languages, arrows. Although the app is small, it tries to provide a presentation about at least very embryonic stages of using very concise concepts and a point-free style stolen from and inspired by combinatory logic and category theory.

A strange backyard of the program: although I intended to provided the project with a modern test environment (unit tests, database tests, web API tests), but for now it has a hacky-tailored **curl-grep** style of custom test tool to investigate reproducible and traceable test runs: [test/API-client-tester.bash](test/API-client-tester.bash).

