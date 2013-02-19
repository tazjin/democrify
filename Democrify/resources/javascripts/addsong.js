$(function(){
    // search implementation
    $('#searchbutton').click(function(e){
        var searchtype = $('#searchtype option:selected').val();
        var searchterm = $('#search').val();
        $('#resultcontainer').empty();
        $.getJSON(('http://ws.spotify.com/search/1/' + searchtype + '.json?q=' + searchterm), function(data){
            $.each(data.tracks.slice(0, 10), function(k, track){
                var trackId = track.href.replace('spotify:track:', '');
                $('#resultcontainer').append('<div class="row"><div class="two columns mobile-one"><a href="#" id="'+trackId+'" class="addtrack"><img src="http://placehold.it/80x80&text=ADD"></a></div><div class="ten columns mobile-three resultspans"><span class="track">'+track.name+'</span><br /><span class="artist">by '+track.artists[0].name+'</span></div><br /><hr /></div>');
                $('#' + trackId).click(function(t){
                    $.get('/add/' + trackId);
                    $(this).attr('id', 'added');
                    $(this).children('img').attr('src', 'http://placehold.it/80x80&text=ADDED');
                });
            });
        });
    });
});
