$(function (){
    //Attach admin voting function
    $('#adminc').on('click', '.next', function(t){
        var song = $(this).attr('id');
        $(this).attr('src', '/upvote_colour.png');
        $.get('/admin/vote/' + song);
    });

    //Attach admin delete function
    $('#adminc').on('click', '.delete', function(t){
        var song = $(this).attr('id');
        $(this).parent().remove();
        $.get('/admin/delete/' + song);
    });
});