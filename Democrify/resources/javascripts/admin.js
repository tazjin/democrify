$(function (){
    //Attach admin voting function
    $('.next').click(function(t){
        var song = $(this).attr('id');
        $(this).attr('src', '/upvote_colour.png');
        $(this).attr('style', 'width:80px;height:80px;')
        $.get('/admin/vote/' + song);
    });

    //Attach admin delete function
    $('.delete').click(function(t){
        var song = $(this).attr('id');
        var c = confirm('Do you want to delete this song?');
        if (c) {
            $(this).parent().parent().next().remove(); // remove hr
            $(this).parent().parent().remove();        // remove the track
            $.get('/admin/delete/' + song);
        }
    });
});