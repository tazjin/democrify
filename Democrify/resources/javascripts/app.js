$(document).foundation();

$(document).ready(function() {
  //Check for all objects that have been voted on and turn the arrows orange
//  $.each($.cookie(), function(c, v){
//    $('img[id$="' + c + '"][class$="vote"]').attr('src', '/upvote_colour.png');
//  });

  // Attach the voting function to all voting objects
  $('body').on('click', '.vote', function(t){
    var thing = $(t.target);
    var song = $(t.target).attr('id');
    thing.attr('src', '/upvote_colour.png');
    if ($.cookie(song) !== null) {
       void(0);
    } else {
      var now = new Date();
      $.cookie(song, '1', {expires : now.addHours(1) });
      $.get('/upvote/' + song);
    }
  });
});