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
        $(this).parent().parent().remove();
        $.get('/admin/delete/' + song);
    });
});