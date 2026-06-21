#import "./utils.typ": parse-content, has-number, size-to-scale
#import "./single.typ": single-chord

// Lesson learned: Counting the bytes in a string, works only until
// the first non-ASCII character appears. For example a German umlaut
// like 'Ä' consists of two bytes and would therefore be miscounted as
// two characters.
// The solution to this is to only work on the grapheme clusters, which
// can be derived from a string using the str.clusters() method. See:
// https://typst.app/docs/reference/foundations/str/#definitions-clusters
// And for more details about what grapheme clusters are:
// https://doc.rust-lang.org/book/ch08-02-strings.html#bytes-scalar-values-and-grapheme-clusters

// Splits a line into words, noting their index, content and length.
// -> array((index: int, word: str, length: int))
#let parse-line(line) = {
  assert.eq(type(line), str)
  let words = ()
  let index = 0
  let in-word = false
  let word-index = -1
  let space-matches = line.matches(" ")
  let space-match-indices = ()
  for space-match in space-matches {
    space-match-indices.push(space-match.start)
  }
  while(index < line.len()) {
    //if not line.slice(index, count: 1) == " " {
    // Above will fail, if there are multi-byte characters, e.g. German umlauts.
    // See https://doc.rust-lang.org/book/ch08-02-strings.html#bytes-scalar-values-and-grapheme-clusters
    // and https://typst.app/docs/reference/foundations/str/
    // But we can't just use typsts iterating grapheme-based iterator over strings, because we need
    // the indices (not the grapheme count) to slice the string.
    if not space-match-indices.contains(index) {
      // Character is not a space.
      if not in-word {
        // Previously not in a word --> This index is the start of a new one.
        word-index = index
      }
      in-word = true
    } else {
      if in-word {
        // Previously in a word --> This is the index behind a word.
        let word-length = index - word-index
        // -1 because the current index is on the space after the word.
        let word = (
          "index" : word-index,
          "text" : line.slice(word-index, count: word-length),
          "length" : word-length
        )
        words.push(word)
      }
      in-word = false
    }
    index += 1
  }
  if in-word {
    // The line ended on a word.
    let word-length = line.len() - word-index
    let word = (
      "index" : word-index,
      "text" : line.slice(word-index, count: word-length),
      "length" : word-length
    )
    words.push(word)
  }

  words
}

// Processes a pair of annotated lyric- and chord-words into a line of content
#let process-line-pair(
  /// Embeds the native *text* parameters from the standard library of *typst*. *Optional*.
  /// -> auto
  ..text-params,

  /// The pair of lines to process. *Mandatory*.
  /// -> dictionary (Keys: "chords" and "lyrics"; Values: the corresponding lines;)
  pair
) = {
  let chord = single-chord.with(..text-params)
  let line = []
  let chord-words = parse-line(pair.chords)
  let lyric-words = parse-line(pair.lyrics)
  let chord-index = 0
  let lyric-index = 0
  let split-word = false // We'll need to know if words were split, so we can omit the space!
  while lyric-index < lyric-words.len() {
    // lyric-words.len() will change between iterations, if merging or splitting happens!
    let lyric-word = lyric-words.at(lyric-index)
    if chord-index < chord-words.len() and chord-words.at(chord-index).index <= lyric-word.index+lyric-word.text.len() {
      // Detailed processing only, if there are more chords AND the next chord starts above this lyric!
      let chord-word = chord-words.at(chord-index)
      
      // ========== Merge Words================================================
      // We will merge, if:
      //   - another lyric-word is available 
      //   - the current chord-word reaches over the next lyric-word 
      if (lyric-index+1) < lyric-words.len() {
        let end-of-chord-word = chord-word.index + chord-word.text.len()
        let start-of-next-word = lyric-words.at(lyric-index+1).index
        if end-of-chord-word > start-of-next-word {
          lyric-word.text = lyric-word.text + " " + lyric-words.at(lyric-index+1).text
          let removed = lyric-words.remove(lyric-index+1)
          // This is a horrible trap: removed isn't used for anything. But if the
          // return value of remove() isn't stored away, it will move upwards,
          // and at the end the method will unsuccessfully try to join it (a dictionary)
          // and the actual return value (line, of type content)-
        }
      }
      
      // ========== Split Words================================================
      // We will split, if:
      // - another chord-word is available
      // - the next-chord-word starts before the end of the current lyric-word
      if chord-words.len() > (chord-index+1) {
        let end-of-this-word = lyric-word.index + lyric-word.text.len()
        let start-of-next-chord = chord-words.at(chord-index+1).index
        if start-of-next-chord <= end-of-this-word {
          split-word = true
          let new-index = start-of-next-chord
          let slice-index = start-of-next-chord - lyric-word.index
          let new-text = lyric-word.text.slice(slice-index)
          let new-lyric-word = (
            "index" : new-index,
            "text" : new-text
          )
          lyric-word.text = lyric-word.text.slice(0, slice-index)
          lyric-words.insert(lyric-index+1, new-lyric-word)
        }
      }

      // ========== Process Word ==============================================
      let chord-pos = [#str(chord-word.index - lyric-word.index + 1)]
      // The weird casting of chord-pos should not be necessary. That's an issue with single-chord.
      line = [#line;#chord[#lyric-word.text][#chord-word.text][#chord-pos]]
      chord-index += 1 // One chord was used, increase index.
    } else {
      // No more chords, still need to print the rest of the words...
      line = [#line;#lyric-word.text]
    }
    if not split-word {
        line = [#line ] 
    }
    split-word = false
    lyric-index += 1 // One lyric was used, increase index.
  }

  // If there are any chords after the last lyric-word, this will display them:
  while chord-index < chord-words.len() {
    let chord-word = chord-words.at(chord-index)
    line = [#line;~~#chord[~][#chord-word.text][]]
    chord-index += 1
  }

  line
}

/// A helper method, to simplify entering entire stanzas, without having to type a complete
/// single-chord command for each chord.
/// -> content
#let chorded-stanza(
  /// Embeds all the parameters of the `single-chord` function, including the native *text*
  /// parameters from the standard library of *typst*. *Optional*.
  /// -> auto
  ..single-chord-params,

  /// If true, linebreaks of the input will be replicated in the output. If false,
  /// the output will be without explicit linebreaks. *Optional*.
  /// -> bool
  preserve-linebreaks: true,

  /// The stanza. Lines of chords and lines of text alternating. *Required*.
  /// -> str
  content
) = context {
  assert.eq(type(preserve-linebreaks), bool)
  assert.eq(type(content), str)

  // Split the input in pairs of one line with chords and one line with lyrics:
  let lines = content.split("\n");
  let line-pairs = ()
  let line-pair = (:)
  let chords = true // First line is chords, from there on alternating.
  for line in lines.slice(1) {
    if chords {
      chords = false
      line-pair.insert("chords", line)
    } else {
      chords = true
      line-pair.insert("lyrics", line)
      line-pairs.push(line-pair)
    }
  }

  let stanza = []
  for line-pair in line-pairs {
    let current-line = process-line-pair(..single-chord-params, line-pair)
    if preserve-linebreaks {
      stanza = [#stanza;#linebreak();#current-line;]
    } else {
      stanza = [#stanza;~#current-line;]
    }
  }

  stanza
}
