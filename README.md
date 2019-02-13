# TODO app

This is forked from https://github.com/evancz/elm-todomvc because it was a
convenient starting point, but my intention is to grow this into an application
which is actually useful to me. I want an app which has just enough features to
be more useful than pen and paper, but which doesn't want me to use it to plan
my whole life and track my productivity.

## Goals

- Entries should be organized by day.
- When opened for the first time every day the user should be asked whether they
  want to import incomplete entries from the previous day.
- Should be able to add entries for future days.
- Should be able to create recurring entries. I.e. do the laundry every Monday.

## Setup

Run the following command from the root of this project:

```bash
make
```

Then open `index.html` in your browser!

To run tests:
```bash
make test
```
