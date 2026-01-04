---
title: "Vibe coding case study: ScubaDuck"
date: 2025-06-02 00:31:25
slug: vibe-coding-case-study-scubaduck
categories: [AI Coding]
comments:
    - id: 35515
      author: skibidi
      date: "2025-06-02 02:54:00"
      content: love your ways of thinking...
    - id: 35518
      author: terrorjack
      date: "2025-06-02 06:17:08"
      content: "yeah, setting up a build environment with dependencies is such a pain for hosted codex. i spent a good chunk of time trying to make a ghc build environment with all the submodules and hadrian build cache available so at least codex could try to typecheck the compiler code with ghci; it's possible, but really a ton of work."
    - id: 35795
      author: Ciprian Turcu
      date: "2025-07-20 14:11:17"
      content: "for a more complex snippet, for a quick implementation of something and even for 150 prompts on building a small, superficial project like this, sure, it can work. but when dealing with \"real\" projects with complex environments and complex logical functionalities and connections, vibe coding is going to provide a bad time. The larger the specs and the project, the larger the context window the more prone it is to hallucinations."
    - id: 36124
      author: grufti
      date: "2025-08-30 22:30:39"
      content: And here I thought that I was the only one that ever struggled with setting up the agent environment in Codex. My experience vibe coding in Codex aligns almost perfectly with yours. I also found stops and restarts much less problematic than I expected. Programming experience translates well to coding using less familiar tools.
---

A lot of strong engineers that I know haven't really taken a serious look at AI coding; they've used LLMs to ask questions or write simple scripts and appreciate that it is a useful tool, but haven't actually tried building a nontrivial application entirely from scratch in vibe coding style (here, I use the term in its original meaning: when you do AI coding without carefully reviewing the output). This is understandable: if you're not working on a green field project, there aren't that many opportunities to write code in this style--standard practice for established projects is that someone else needs to review all of the code you write: this is a bad match for vibe coding! So in this post, I want to give a concrete case study of a nontrivial system that was entirely vibe coded (ScubaDuck), to argue the following claims:

1.  AI coding can be done on a manager's schedule: you don't need continuous blocks of coding time and context-switching is considerably less harmful. ScubaDuck was implemented in three days of *part* time work, where all of the work happened when the baby was napping.
2.  AI coding substantially lowers the cost of doing projects in tech stacks you are less familiar with. ScubaDuck is mostly JavaScript UI code, which is not something I write on a day-to-day basis.
3.  AI coding is an unlock for "sidequests": support software that's ancillary to your main task that is nice to have, but not essential. If previously you would have decided the cost outweighed the benefit, AI coding reducing the cost means you should redo these calculations.
4.  Vibe coding works and can produce working software. ScubaDuck is an existence proof that vibe coding is a viable strategy for generating JavaScript UI code (NB: I don't claim vibe coding will work for *all* domains, nor do I claim this is the *only* domain for it works. Hopefully you can also build some intuition for where it is more or less likely to work). You will not one shot it (ScubaDuck was 150 prompts in the end) but if you are prompting the LLM to also generate tests, you can reliably fix issues without causing regressions to existing code.
5.  Vibe coding is good for situations where buggy software is low impact; be on the lookout for ways to engineer this sort of situation. ScubaDuck is a read-only interface, where the only downside to being buggy is you can't issue the queries you want to issue.

Update: You can see all of my prompts and the resulting agent trajectories at [scubaduck-prompts](https://ezyang.github.io/scubaduck-prompts/).

# What is ScubaDuck?

[ScubaDuck](https://github.com/ezyang/scubaduck) is a discount implementation of Meta's internal Scuba realtime database system. You can read more about what exactly this is on GitHub, but it's not so important for the purposes of this post: the key details you need to know about ScubaDuck is that it consists of a Python server that exposes an API to perform queries against a DuckDB database, and an HTML and JavaScript frontend application which implements the forms for building these queries and rendering of the output data. Both the forms and output data rendering have nontrivial JavaScript enhancements: some form inputs are [chip inputs](https://mui.com/material-ui/react-chip/) and support autocomplete, and the time series view is an SVG chart. All of these components were coded from scratch, so the project has no third-party JavaScript dependencies.

So on the one hand, this project is pretty simple. There are no stringent performance or uptime requirements, it's a pretty standard server-client program that the LLM has seen millions of times before (this is good!) On the other hand, the exact behavior of the frontend UI is quite intricate and would be very difficult to one-shot in a single prompt. Indeed, as I was coding and testing the application, I frequently ran into situations that I didn't anticipate in my original specification, and that I had to ask Codex to refine. Another way to put it is that ScubaDuck is a relatively simple functional specification (although this too was not one shot), but I did a lot of polishing of small behaviors so that the interface behaved in the way that I expected Scuba to behave. Here, it was helpful that I had a very clear idea of what I wanted (since I've used Scuba quite a lot at work).

Going into ScubaDuck, I had a pretty good sense that this project should be a good fit for LLMs. HTML, JavaScript and Python are all extremely high resource languages, and I'd heard lots of people raving about how good LLMs were at transforming wireframes and mockups into fully functional websites. It is also fully self contained and straightforward-ish to test (only "ish" because you do have to use something like Playwright to actually test the frontend UI, which honestly is a slog. But fortunately, the LLM can write the tests for you!) One design decision I made, which I didn't originally anticipate but worked out in the end, was the decision to not use any third-party JavaScript libraries. This was by accident: Python has no native of bundling third party JavaScript, but I wanted the tool to work offline. I wasn't sure if you could vibe code an SVG charting library from scratch, but apparently you can and it's quite easy!

# Agent setup

ScubaDuck was implemented with OpenAI Codex in the cloud (not the CLI tool). Codex's cloud offering requires you to initialize a hermetic environment which the coding agent can execute commands in. It's pretty well known now that AI coding agents work much better if they are able to run the code they write and see if it worked or not, so this is quite an important part of the process. Unfortunately, this was somewhat time consuming trial and error to setup. I had a fairly detailed initial prompt, and what I would do was submit it to Codex, watch it fail, read over the trajectory (the agent logs) to see what happened (Codex wanted to use npm! Codex couldn't download something from the internet! Codex tried to use a package that wasn't available!) and then fixed whatever environment misconfiguration had caused it to fail, or edited AGENTS.md to instruct it to not do some behavior. According to my history, the first day of the project was spent unsuccessfully trying to get the project setup, and my first successful Codex PR only happened on May 19.

At the end of setup, I had the following:

1.  A [pyproject.toml](https://github.com/ezyang/scubaduck/blob/main/pyproject.toml) with exactly the dependencies I wanted to be used (duckdb, flask and python-dateutil), a lockfile for it (since I was using uv) and my preferred configuration for various tools (pytest, ruff). I'm a big fan of pytest-xdist for vibe coded projects, since you can prompt the LLM to write tests that will work when run in parallel and it does a pretty good job at this. Later I'd also add a pyright configuration, though initially I left it out because I saw Codex doing some strange things on account of duckdb being untyped, and I didn't want to debug it at the time (the fix, by the way, is instructing the LLM to define stubs as necessary in this case.)
2.  An [AGENTS.md](https://github.com/ezyang/scubaduck/blob/main/AGENTS.md) file with some basic instructions to try to get Codex to stop doing things I saw it doing in the initial trajectories that I didn't want it to do. Nothing fancy, just if you see Codex do something bad, tell it not to do it in AGENTS.md. A good example of this is the "There are no nested AGENTS.md files, this is the only agents file": Codex is post-trained to look for nested AGENTS.md files, but you can save a few tool calls if you tell it there aren't any. (Note: folklore for Claude 3.7 is that instruction following for this sort of rules following was not great. Word on the street is that both Codex and Claude 4 are substantially better at this. Extra note: For uv users, another notable instruction in AGENTS.md is how to activate the venv, since at time of writing I couldn't get Codex to make this happen automatically.)
3.  A [setup script](https://github.com/ezyang/scubaduck/blob/main/codex_setup.sh) for the environment. This took the most debugging, because Codex runs all Internet access through a proxy and sometimes it works imperfectly.

After I got my initial prompt to [generate a first draft of the application](https://github.com/ezyang/scubaduck/pull/1), I was able to begin vibe coding in earnest.

# The Human-Agent loop

The basic vibe coding loop works like this:

1.  Interact with the application and find things that are broken
2.  Prompt the LLM to fix them
3.  Repeat

For example, after the very first PR, some very mild poking around immediately revealed the bugs fixed in [\#2](https://github.com/ezyang/scubaduck/pull/9):

> There's a race condition in the current test logic for matching against table contents in run_query. Specifically, if there were previously valid results in lastResults, and for some reason Dive doesn't do anything, then we will still see the old results. The testing framework should explicitly clear lastResults before attempting an interaction.

...and [\#3](https://github.com/ezyang/scubaduck/pull/10):

> Filter functionality does not work. We will first add a failing test, and then fix it. The failing test should click "Add Filter", then select "user" as the field, and then add an "alice" chip (by typing alice in the text box and pressing ENTER). Then when we dive, we should see two alice rows. Right now, NO request is issued at all when we click Dive. Diagnose and then fix the problem.

**Prompt the agent to write tests.** It's very helpful to prompt the agent to generate tests for whatever bugs its fixing. For frontend code, I decided to use playwright to write these tests. An example in [\#11](https://github.com/ezyang/scubaduck/pull/11):

    def test_header_and_tabs(page: Any, server_url: str) -> None:
        page.goto(server_url)
        page.wait_for_selector("#order_by option", state="attached")

        header = page.text_content("#header")
        assert "sample.csv" in header
        assert "events" in header

        assert page.is_visible("#settings")
        assert page.is_hidden("#columns")
        page.click("text=Columns")
        assert page.is_visible("#columns")
        cols = page.locator("#column_list li").all_inner_texts()
        assert "timestamp" in cols
        assert "event" in cols
        page.click("text=View Settings")
        assert page.is_visible("#settings")

        btn_color = page.evaluate("getComputedStyle(document.querySelector('#dive')).backgroundColor")
        assert "rgb(0, 128, 0)" == btn_color

        sidebar_overflow = page.evaluate("getComputedStyle(document.querySelector('#sidebar')).overflowY")
        view_overflow = page.evaluate("getComputedStyle(document.querySelector('#view')).overflowY")
        assert sidebar_overflow == 'auto'
        assert view_overflow == 'auto'

This is kind of a very manual and somewhat brittle test. I doubt I would have had the patience to manually write and debug this. But the LLM will happily do it. There is also a fear that the test might not actually be testing anything for real. But because these tests are all generated from bug reports, you can prompt the model to write the test first, check that it's failing, and then fix the problem. I didn't instruct this in the above prompt, but I did instruct it for [\#155](https://github.com/ezyang/scubaduck/pull/155), for example. The trajectory shows that the LLM demonstrates it can repro the bug before fixing the issue:

> I’ll add a case-insensitive test for "samples" to check server behavior with lowercase input.
>
>     (scubaduck) root@5b69743466e4:/workspace/scubaduck# python - <<'EOF'
>     > import json
>     > from scubaduck import server
>     >
>     > app = server.app
>     > client = app.test_client()
>     > payload = {
>     >     "table": "events",
>     >     "start": "2024-01-01 00:00:00",
>     >     "end": "2024-01-03 00:00:00",
>     >     "graph_type": "table",
>     >     "order_by": "samples",  # lowercase
>     >     "order_dir": "DESC",
>     >     "limit": 10,
>     >     "columns": [],
>     >     "group_by": ["user"],
>     > }
>     > rv = client.post("/api/query", data=json.dumps(payload), content_type="application/json")
>     > print('status', rv.status_code)
>     > print('data', rv.get_json())
>     > EOF
>     status 400
>     data {'error': 'Unknown column: samples'}
>     (scubaduck) root@5b69743466e4:/workspace/scubaduck#
>
> I’ve found the bug happens when "Samples" is lowercase. I’ll adjust the query to handle case insensitivity and add a test for it. Time to modify server.py to fix the issue!

**Beware BC-breaking changes.** The LLM generated tests worked well when I didn't need to change behavior in a BC-breaking way. Example prompt:

> Change time column to default to none unconditionally. (I want to default it to a time column if we find a good candidate but this threshold needs tuning and I don't have the tuning right now.)

This hit a Codex timeout:

> This attempt to update the default time column led to extensive frontend test failures that require deeper changes. The environment made it difficult to fully diagnose the Playwright test behaviors within the time available, so the work could not be completed.

In standard software engineering practice, when this happens, decouple the BC compatible and BC breaking changes!

> Make it so that Time Column can be set to (none). When it is set this way, the Start/End fields are hidden and we don't apply a filter on time range. ([\#115](https://github.com/ezyang/scubaduck/pull/115))

and then later instead of defaulting the time column to none, I added a heuristic to pick a column that looked like time, which picked the same column that all of the existing tests had also expected to be called with.

**Refactors have to be split up.** Codex's timeout means that you can't ask it to do too much in one go. Here's a prompt that timed out:

> scubaduck/index.html has gotten a bit long. Let's split out some of the JS code into dedicated JS files for their functionality. Also setup the necessary Flask scaffolding to serve these JS files. I think splitting out these specific components would be good:
>
> - Dropdown implementation
> - Sidebar resizing
> - JS controlling the View Settings (e.g., updateDisplayTypeUI, as well as one off interactions on form elements, columns handling, filter handling, the actual Dive implementation (including query updating), reading in defaults from query string)
> - Table rendering (e.g., formatNumber, sorting)
> - Chip input implementation
> - Chart rendering (showTimeSeries)
>
> Make changes to AGENTS.md or README.md describing the structure so you can quickly find where the components you need are

I eventually did manage the refactor by prompting Codex to individually move out the pieces I wanted to extract one-by-one. This is a place where I think Claude Code probably would have performed better.

**Parallelizing tasks.** As you can see from the lengths of my prompts, it does take a while to write a good prompt; you're basically writing a bug report with enough detail that the LLM can repro it and then fix it. So sometimes I would be bottlenecked on prompt writing. However, sometimes the prompts were quite short. In those cases, Codex encourages you to submit more tasks that can run in parallel. I found this worked well, and I'd sometimes have as many as five instances going (once again, rate limited by discovering problems, making designs and typing prompts!) One irritation is when the tasks end up conflicting with each other. Sometimes the conflicts are easy to fix, but if it feels nontrivial, it's often better to just ask Codex to redo one of the PRs on latest main after the other has landed. To avoid merge conflicts, it helps to have only one "main feature" agent going at any time, and then ask the agent to do random bugfixes in parallel with it. Once you have no more tasks to get running, you can go do something else while you wait for the agents to finish (manager schedule!)

# Prompting

As a reminder, I've posted all of my prompts (including the ones that failed) at [scubaduck-prompts](https://ezyang.github.io/scubaduck-prompts/), and I think it's helpful to skim through them to get a flavor of what I was asking the LLM. But to summarize, what did I spend most of my time on prompting Codex to do? My general vibe (ahem) is that I spent most of my time doing minor enhancements, where I instructed Codex to make some part of the program work slightly differently, in a way that was previously unspecified from the previous prompt. The metaphor I had in my head while I was working on the project was like that of a sculptor chiseling away marble: in the beginning, anything is possible, but as I kept prompting, I continuously narrowed down the space of possible programs I had until I had exactly the one I wanted. One big thing I want to note is that Codex rarely needed to make updates to my tests; for the most part, tests that were added never got taken away, because I never "changed my mind". I suspect that the vibe coding process would have been rockier if I was having to change behavior frequently.

One of the things that surprised me the most about the process was how easy it was to implement a line chart in SVG with Codex. My first prompt resulted in a chart that looked broken on the test data:

> We're going to add a new View type, to go along with Samples and Table: Time Series. Time Series supports all the fields that Table supports, and a few more:
>
> - X-axis: Main group by dimension, e.g., the x-axis on time series view. This is our custom dropdown selector, but only time columns are populated here. It should prefer a default setting from the following list, most preferred first: "time", "timestamp"
> - Granularity: Choose the time interval between data points on the chart. For example, a granularity of 1 hour means there will be a data point every 60 minutes that is aggregated with the chosen Aggregate function over the data for the granularity period before point. This is a plain drop down. The valid values are: Auto, Fine, 1 second, 5 seconds, 10 seconds, 30 seconds, 1 minute, 4 minutes, 5 minutes, 10 minutes, 15 minutes, 30 minutes, 1 hour, 3 hours, 6 hours, 1 day, 1 week, 30 days. The semantics of the Auto setting is that it sets the interval to whatever would result in maximum 100 buckets (if there are not enough data points for that many buckets, it just picks the finest time interval that makes sense), and Fine which sets the interval to 500 buckets.
> - Fill Missing Buckets: This is a dropdown. For now, it has the settings "Fill with 0 (Per Series)" (default), "Connect (Per Series)" and "Leave blank".
>
> Additionally, the default setting of Limit is 7, as it controls how many elements from group by will be plotted (the actual number of lines plotted could be a multiple of this, as we will plot every selected Column).
>
> Unlike Samples and Table, we will instead display a line chart in the right panel. To plot the line chart, we will implement it by hand with JS and SVG, similar to how highcharts implements it. We will not use any third party dependencies. Lines will be plotted as paths, no smoothing, no dots for individual data points. Each series (as generated by group by) should be plotted with a different color, assigned using a best practices color palette for graph design. There should be a rendering of x-axis and y-axis; the x-axis should have slanted labels to aid readability. When we mouse over the chart, a vertical line should snap to the center of the time bucket that we are closest to. We should also display a crosshair on all of the series showing us their values at that data point, and highlight the closest point we are on, and increase the thickness of the series that point is on. To the left of the graph (still in the right panel), there should be a legend. The legend looks like this:
>
>     [GROUP BY VALUE] [AGGREGATE]
>     [First Column name, with series color]
>     [Number of samples for the first column]
>     [Second Column name, with series color]
>     [Number of samples for the second column]
>     ... for all columns
>     ----
>     ... for all group by values (up to the limit)
>
> So for example, if I group by user, I might see:
>
>     Alice AVG
>     value
>     4 (samples)
>
> The highlighted series (which has a thicker line) should also be highlighted in the legend).

This was kind of terrifying, because I initially thought I didn't have a good way to test the SVG outputs. But after doing some regular old-fashioned debugging and reading the code (yes, this part not vibe coded), I figured out the problem, and also realized that Playwright can test that an SVG path is not just entirely straight. After the initial bugs were fixed, I mostly had to add missing features like x-axis/y-axis and interactivity features (amusingly, Codex ignored most of the instructions in the latter half of the prompt, giving only the barest bones legend. I suspect this was because I had some files which were too long). My general take after this was that JS chart libraries are going to become obsolete: it's much easier to vibe code a bespoke implementation and then customize the heck out of it.

# Conclusion

ScubaDuck was implemented in about 150 Codex prompts. As you can see from the sample prompts above, the prompts are recognizably *programming*, they just happen to be in plain English language. This is a big help, because I never had to keep track of the nest of callbacks and state machines for implementing complex UI elements in JavaScript. I had to be fluent in what I wanted my program to do, and a good QA tester for the application to discover new problems that needed to be fixed, but I did not have to worry at all about the vagaries of SVG DOM elements or pixel position computation minutiae. It's hard to say how long it would have taken to code this by hand, but I think reproducing a UI that's been in production for years at Meta in three (part-time) days is pretty good!

Despite having done a bit of AI coding before, I also learned a bit from working on Codex. Codex made it blindingly clear that the parallel modality (and subsequent conflict resolution) is important. It made me adjust up my estimation of the capability of LLMs to write raw HTML/JS and evoked a future where people vibe code components in place of taking on a third party dependency. I was very appreciative of no rate limit Codex (though I doubt it's going to last.) It also reminded me how difficult it will be to setup agent environments for "real" projects (like PyTorch).

Hopefully, this case study has given you some ideas for things to try. Go forth and vibe code, responsibly!
