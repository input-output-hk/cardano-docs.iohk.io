# Cardano SL documentation

Build on [Jekyll](https://jekyllrb.com/) using Github pages.

Manage, add and edit documents only in `/_docs/` folder.

Header example:
<pre>
---
layout: default
title: Getting started
group: base
children: getting-started
---
</pre>

Page anchors example (  Heading sentence: heading-sentence):
<pre>
---
layout: default
title: Introduction
permalink: /introduction/
group: base
anchors:
  Cryptocurrency Basics: cryptocurrency-basics
  What Makes Cardano SL Special?: what-makes-cardano-sl-special
  Beyond Settlement Layer: beyond-settlement-layer
---
</pre>
