#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from wordcloud import WordCloud

wc = WordCloud(mask=mask, background_color='white')
wc.max_words=2000
wc.fit_words(tuple_dict)
wc.to_file(r.wordcloud_output_path)