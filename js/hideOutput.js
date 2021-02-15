$(document).ready(function() {
// from https://stackoverflow.com/questions/37755037/how-to-add-code-folding-to-output-chunks-in-rmarkdown-html-documents

  $chunks = $('.fold');

  $chunks.each(function () {

    // add button to source code chunks
    if ( $(this).hasClass('s') ) {
      $('pre.r', this).prepend("<div class=\"showopt\"><a href='#'>le code</a></div>");
      // $('pre.r', this).children('code').addClass('folded');
      $('pre.r', this).children('code').hide();
    }

    // add button to output chunks
    if ( $(this).hasClass('o') ) {
      $('pre:not(.r)', this).has('code').prepend("<div class=\"showopt\"><a href='#'>la sortie R</a></div>");
      $('pre:not(.r)', this).children('code:not(r)').addClass('folded');

      // add button to plots
      $(this).find('img').wrap('<pre class=\"plot\"></pre>');
      $('pre.plot', this).prepend("<div class=\"showopt\"><a href='#'>le graph</a></div>");
      $('pre.plot', this).children('img').addClass('folded');

    }
  });

  // hide all chunks when document is loaded
  $('.folded').css('display', 'none')

  // function to toggle the visibility
  $('.showopt').click(function() {
    var label = $(this).html();
    $(this).siblings('code, img').slideToggle('fast', 'swing');
    $(this).toggleClass("shown");
  });
});
