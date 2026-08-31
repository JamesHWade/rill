# local Defuddle reports CLI failures

    Code
      fetch_defuddled_markdown_local("https://example.com/article", list(
        defuddle_command = "defuddle"), runner = runner)
    Condition
      Error in `fetch_defuddled_markdown_local()`:
      ! Local Defuddle extraction failed.
      x Unable to fetch the page

# local Defuddle requires an installed executable

    Code
      run_defuddle_cli("rill-defuddle-command-that-does-not-exist", character(),
      timeout = 1)
    Condition
      Error in `run_defuddle_cli()`:
      ! The local Defuddle executable 'rill-defuddle-command-that-does-not-exist' was not found.
      i Install it with `npm install -g defuddle`, or set `DEFUDDLE_COMMAND`.
