#import "./utils.typ": parse-content, has-number, size-to-scale
#import "./single.typ": single-chord

/// Splits a line into words, noting their index, content and length.
/// -> array((index: int, word: str, length: int))
#let parse-line(line) = {
  assert.eq(type(line), str)
  let words = ()
  let index = 0
  let in-word = false
  let word-index = -1
  while(index < line.len()) {
    if not line.slice(index, count: 1) == " " {
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

/// Processes a pair of annotated lyric- and chord-words into a line of content
#let process-line-pair(
  /// Embeds the native *text* parameters from the standard library of *typst*. *Optional*.
  /// -> auto
  ..text-params,

  /// The pair of lines to process. *Mandatory*.
  /// -> dictionary (Keys: "chords" and "lyrics"; Values: the corresponding lines;)
  pair,

  /// Should two words be merged, if a chord reaches from the first over the second? *Mandatory*
  /// -> bool
  merge-words
  ) = {
  assert.eq(type(merge-words), bool)
  let chord = single-chord.with(..text-params)
  let line = []
  let chords = pair.chords
  let lyrics = pair.lyrics
  let chord-words = parse-line(chords)
  let lyric-words = parse-line(lyrics)
  let chord-index = 0
  let lyric-index = 0
  while lyric-index < lyric-words.len() {
    let lyric-word = lyric-words.at(lyric-index)
    if chord-index < chord-words.len() {
      let chord-word = chord-words.at(chord-index)
      let chord-word-index = chord-word.index
      let chord-word-length = chord-word.length
      let lyric-word-index = lyric-word.index
      let lyric-word-length = lyric-word.length
      if lyric-word-index - 1 <= chord-word-index and chord-word-index <= lyric-word-index+lyric-word-length - 1 {
        // Match! --> Insert lyric with chord; increment chord index;
        let chord-pos = [#str(chord-word-index - lyric-word-index + 1)]
        // The weird casting of chord-pos should not be necessary. That's an issue with single-chord.
        let merge-required = false
        let merge-possible = false
        if merge-words and (lyric-index + 1) < lyric-words.len() {
          // merging is selected, and a word to merge is available
          let next-lyric-index = lyric-words.at(lyric-index+1).index
          if (chord-index + 1) < chord-words.len() {
            // There is another chord, that could potentially match the next word.
            let next-lyric-length = lyric-words.at(lyric-index+1).length
            let next-chord-index = chord-words.at(chord-index+1).index
            if next-lyric-index - 1 <= next-chord-index and next-chord-index <= next-lyric-index + next-lyric-length - 1 {
              // The other chord matches the next word, so we can't merge!
              merge-possible = false
            } else {
              merge-possible = true
            }
          } else {
            // There is no other chord that could match the next word.
            merge-possible = true
          }
          if merge-possible {
            // merging is possible, but is it necessary?
            // Necessary, if the end of the current chord goes beyond the start of the next word!
            if chord-word-index + chord-word-length - 1 >= next-lyric-index {
              merge-required = true
            }
          }
        }
        if merge-required {
          line = [#line; #chord[#lyric-word.text; #lyric-words.at(lyric-index+1).text][#chord-word.text][#chord-pos]]
          lyric-index += 1
        } else {
          line = [#line; #chord[#lyric-word.text][#chord-word.text][#chord-pos]]
        }
        chord-index += 1
      } else {
        // No match! --> Insert lyric without chord;
        line = [#line; #lyric-word.text]
      }
    } else {
      // No more chords, still need to print the rest of the words...
      line = [#line; #lyric-word.text]
    }
    lyric-index += 1
  }
  // If there are any chords after the last lyric-word, this will display them:
  while chord-index < chord-words.len() {
    let chord-word = chord-words.at(chord-index)
    line = [#line;~~#chord[~][#chord-word.text][]]
    chord-index += 1
  }
  line
}

/// The single chord a chord without diagram used to show the chord name over a word.
/// -> content
#let chorded-stanza(
  /// Embeds the native *text* parameters from the standard library of *typst*. *Optional*.
  /// -> auto
  ..text-params,

  /// Sets the inner gap between the bottom word and the chord name. *Optional*.
  /// -> bool
  preserve-linebreaks: true,

  /// Should two words be merged, if a chord reaches from the first over the second? *Mandatory*
  /// -> bool
  merge-words: true,

  /// Debug output?
  /// -> bool
  debug: false,

  /// The stanza. Lines of chords and lines of text alternating. *Required*.
  /// -> str
  content

  /// Annotates a plaintext stanza using the single-chord function.
  /// -> content
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

  let chords-lines = () // for debugging only
  let lyrics-lines = () // for debugging only
  let stanza = []
  for line-pair in line-pairs {
    chords-lines.push(parse-line(line-pair.chords)) // for debugging only
    lyrics-lines.push(parse-line(line-pair.lyrics)) // for debugging only
      if preserve-linebreaks {
        stanza = [#stanza;#linebreak();#process-line-pair(..text-params, line-pair, merge-words);]
      } else {
        stanza = [#stanza;~#process-line-pair(..text-params, line-pair, merge-words);]
      }
  }
  
  if debug {
    [
      === Input
      #text()[#set par(leading: 0.3em);#raw(content)]

      === Line-Pairs
      #text()[#set par(leading: 0.3em);#line-pairs]
      
      === Word-Lists
      ==== Chords
      #text()[#set par(leading: 0.3em);#chords-lines]
      ==== Lyrics
      #text()[#set par(leading: 0.3em);#lyrics-lines]

      === Output
      #stanza
    ]
  } else {
    stanza
  }
}
