#import "./utils.typ": parse-content, has-number, size-to-scale

/// The single chord a chord without diagram used to show the chord name over a word.
/// -> content
#let chorded-stanza(
  /// Embeds the native *text* parameters from the standard library of *typst*. *Optional*.
  /// -> auto
  ..text-params,

  /// Sets the inner gap between the bottom word and the chord name. *Optional*.
  /// -> length
  preserve-linebreaks: true,


  /// The stanza. Lines of chords and lines of text alternating. *Required*.
  /// -> content
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

  // TODO: External function, to do this splitting:
  // Split a chord line into a dictionary mapping indices to chords:

  // Split a lyrics line into a dictionary mapping indicices to words:


  text()[#set par(leading: 0.3em);#line-pairs]
}
