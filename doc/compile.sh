#!/usr/bin/env bash
pandoc document.md -o out/document.html -c doc-style.css --self-contained -V "pagetitle:Project document"
