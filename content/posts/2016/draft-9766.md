---
title: "Backpack syntax final call"
date: 2016-10-11 01:54:23
slug: 
draft: true
categories: [Miscellaneous]
---

    backpack-includes:
      regex-indef (Regex as Regex.String) requires (Str as Str.String),
      regex-indef (Regex as Regex.ByteString) requires (Str as Str.ByteString)

Known proposals:

David Turner (@DaveCTurner):

    regex-indef (Regex as Regex.String) satisfies (Str with Str.String),
    regex-indef (Regex as Regex.ByteString) satisfies (Str with Str.ByteString)

Sergio Benitez:

    regex-indef (Str as Str.String)     => (Regex as Regex.String),
    regex-indef (Str as Str.ByteString) => (Regex as Regex.ByteString)

But it polled poorly: <https://twitter.com/ezyang/status/781383407854948354>

Field name proposals: `hs-includes`, `module-aliases`, `unit-includes`

------------------------------------------------------------------------

Less important syntax: file extension `foo.bkp`; stanza name `unit p`.
