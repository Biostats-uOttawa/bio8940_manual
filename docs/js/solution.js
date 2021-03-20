(function() {

$exerc = $('.exer');

  $exerc.each(function() {
    $(this).before("<p class='solution-link'><a href='#'>the solution</a></p>");
    $(this).hide();
    console.log($(this).prev());
    $(this).prev().click(function() {
      $(this).next().slideToggle();
      $(this).toggleClass("shown");
    });
  });
})();
