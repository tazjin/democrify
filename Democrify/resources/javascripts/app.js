;(function ($, window, undefined) {
  'use strict';

  var $doc = $(document),
      Modernizr = window.Modernizr;

  $(document).ready(function() {
    $.fn.foundationAlerts           ? $doc.foundationAlerts() : null;
    $.fn.foundationButtons          ? $doc.foundationButtons() : null;
    $.fn.foundationNavigation       ? $doc.foundationNavigation() : null;
    $.fn.foundationTopBar           ? $doc.foundationTopBar() : null;
    $.fn.foundationCustomForms      ? $doc.foundationCustomForms() : null;
    $.fn.foundationMediaQueryViewer ? $doc.foundationMediaQueryViewer() : null;
    $.fn.foundationTabs             ? $doc.foundationTabs({callback : $.foundation.customForms.appendCustomMarkup}) : null;

    $.fn.placeholder                ? $('input, textarea').placeholder() : null;

    //Check for all objects that have been voted on and turn the arrows orange
    $.each($.cookie(), function(c, v){
      $('#' + c).attr('src', '/upvote_colour.png');
    });

    // Attach the voting function to all voting objects
    $('body').on('click', '.vote', function(t){
      var thing = $(t.target);
      var song = $(t.target).attr('id');
      thing.attr('src', '/upvote_colour.png');
      if (typeof ($.cookie(song)) != "undefined") {
         void(0);
      } else {
        var now = new Date();
        $.cookie(song, '1', {expires : now.addHours(1) });
        $.get('/upvote/' + song);
      }
    });


  });

  // Hide address bar on mobile devices (except if #hash present, so we don't mess up deep linking).
  if (Modernizr.touch && !window.location.hash) {
    $(window).load(function () {
      setTimeout(function () {
        window.scrollTo(0, 1);
      }, 0);
    });
  }

})(jQuery, this);